{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module DBus.Property where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Singletons
import           Data.Text (Text)
import qualified Data.Text as Text

import           DBus.Signal
import           DBus.Types
import           DBus.Error
import           DBus.Message
import           DBus.Representable

property :: SingI t => Property t -> Objects
property p = root (propertyPath p)
                  (object (propertyInterface p)
                  mempty{interfaceProperties = [SomeProperty p]})




-- | Create a property from a getter and a setter. It will emit a
-- PropertyChanged signal when the setter is called. To change
-- this behaviour modify the propertyEmitsChangedSignal field
mkProperty :: Representable a =>
              ObjectPath
           -> Text
           -> Text
           -> Maybe (MethodHandlerT IO a)
           -> Maybe (a -> MethodHandlerT IO Bool)
           -> PropertyEmitsChangedSignal
           -> Property (RepType a)
mkProperty path iface name get set pecs =
    let prop = Property { propertyPath = path
                        , propertyInterface = iface
                        , propertyName = name
                        , propertySet = doSet <$> set
                        , propertyGet = fmap toRep <$> get
                        , propertyEmitsChangedSignal = pecs
                        }
    in prop
  where
    doSet f v = f =<< fromRepHelper v
    fromRepHelper x = case fromRep x of
        Nothing -> methodError argTypeMismatch
        Just r -> return r

-- | Make a property out of a TVar. The property is considered changed on every
-- outside set, no matter if the updated value is actually different from the
-- old one
mkTVarProperty :: Representable a =>
                  ObjectPath
               -> Text
               -> Text
               -> PropertyAccess
               -> PropertyEmitsChangedSignal
               -> TVar a
               -> Property (RepType a)
mkTVarProperty path iface name acc pecs tv =
    mkProperty path iface name
        (case acc of
              Write -> Nothing
              _ -> Just (liftIO . atomically $ readTVar tv))
        (case acc of
              Read -> Nothing
              _ -> Just (\v -> liftIO (atomically (writeTVar tv v))
                               >> return True))
        pecs

manageStmProperty :: (Representable t, Eq t) =>
                     Property (RepType t)
                  -> STM t
                  -> DBusConnection
                  -> IO ()
manageStmProperty prop get con = do
    let sendSig v = emitPropertyChanged prop v con
    forkIO $ onEdge sendSig
    return ()
  where
    onEdge f = do
        x <- atomically get
        go f x
    go f x = do
        x' <- atomically $ do
            x' <- get
            when (x == x') $ retry
            return x'
        f x'
        go  f x'

-- | Interface for D-BUs properties
propertiesInterfaceName = "org.freedesktop.DBus.Properties"

-- | Create a propertyChangedSignal for a property
propertyChangedSignal :: Representable a =>
                         Property (RepType a)
                      -> a
                      -> Maybe SomeSignal
propertyChangedSignal prop x =
    let path = propertyPath prop
        iface = propertyInterface prop
        name = propertyName prop
    in case propertyEmitsChangedSignal prop of
        PECSFalse -> Nothing
        PECSTrue ->
            Just $ SomeSignal $
            Signal { signalPath = path
                   , signalInterface = propertiesInterfaceName
                   , signalMember = "PropertiesChanged"
                   , signalBody = flattenRep $ toRep
                       ( iface
                       , Map.fromList [ ( name , DBVVariant $ toRep x )]
                       , [] :: [Text]
                       )}
        -- invalidates or emits changed but is write-only
        PECSInvalidates -> Just $ SomeSignal $
            Signal { signalPath = path
                   , signalInterface = propertiesInterfaceName
                   , signalMember = "PropertiesChanged"
                   , signalBody = flattenRep $ toRep
                       ( iface
                       , Map.empty :: Map.Map Text (DBusValue (TypeVariant))
                       , [name]
                       )
                   }

propertyChanged :: (MonadIO m, Representable a) =>
                   Property (RepType a) -> a -> MethodHandlerT m ()
propertyChanged prop a =
    Foldable.mapM_ signal' (propertyChangedSignal prop a)

emitPropertyChanged :: Representable a =>
                       Property (RepType a) -> a -> DBusConnection -> IO ()
emitPropertyChanged prop x con = do
    let mbSig = propertyChangedSignal prop x
    Foldable.forM_ mbSig $ flip emitSignal' con

getProperty :: Representable a =>
               RemoteProperty (RepType a)
            -> DBusConnection
            -> IO (Either MethodError a)
getProperty rp con = do
     res <- callMethod (rpEntity rp) (rpObject rp) propertiesInterfaceName "Get"
                (rpInterface rp , rpName rp) [] con
     return $ castVariant =<< res
  where
    castVariant v = case fromRep =<< fromVariant v of
        Nothing -> Left $ MethodSignatureMissmatch [DBV v]
        Just x -> Right x

setProperty :: Representable a =>
               RemoteProperty (RepType a)
            -> a
            -> DBusConnection
            -> IO (Either MethodError ())
setProperty rp x con = do
     callMethod (rpEntity rp) (rpObject rp) propertiesInterfaceName "Set"
                (rpInterface rp , rpName rp, DBVVariant $ toRep x) [] con

handlePropertyChanged :: Representable a =>
                         RemoteProperty (RepType a)
                      -> (Maybe a -> IO ())
                      -> DBusConnection
                      -> IO ()
handlePropertyChanged rp f' con = do
    let ms = anySignal { matchInterface = Just $ propertiesInterfaceName
                       , matchMember = Just "PropertiesChanged"
                       , matchPath = Just $ rpObject rp
                       , matchSender = Just $ rpEntity rp
                       }
        mr = (matchSignalToMatchRule ms)
               <> matchAll {mrArgs = [(0, rpInterface rp)]}
        f Nothing = f' Nothing
        f (Just sdbv) = case fromRep =<< dbusValue sdbv of
                Nothing -> logError $ "Property type error "
                           ++ show (rpObject rp) ++ " / "
                           ++ Text.unpack (rpInterface rp)
                           ++ "." ++ Text.unpack (rpName rp)
                Just v -> f' (Just v)
        slot = Map.singleton (rpObject rp, rpInterface rp, rpName rp) [f]
    atomically $ modifyTVar' (dbusPropertySlots con) (Map.unionWith (++) slot)
    addMatch mr con

propertyToTVar :: Representable a =>
                  RemoteProperty (RepType a)
               -> DBusConnection
               -> IO (TVar a)
propertyToTVar rp con = do
    eiv <- getProperty rp con
    case eiv of
     Left e -> Ex.throwIO e
     Right iv -> do
        tv <- newTVarIO iv
        handlePropertyChanged rp
            (\mbv -> case mbv of
                      Nothing -> do
                          eniv <- getProperty rp con
                          case eniv of
                           Left e -> return () -- @TODO
                           Right nv -> atomically $ writeTVar tv nv
                      Just v -> atomically $ writeTVar tv v
            ) con
        return tv
