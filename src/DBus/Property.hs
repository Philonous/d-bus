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

property :: SingI t => Property t -> Objects
property p = root (propertyPath p)
                  (object propertiesInterfaceName
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
                      -> Maybe Signal
propertyChangedSignal prop x =
    let path = propertyPath prop
        iface = propertyInterface prop
        name = propertyName prop
    in case propertyEmitsChangedSignal prop of
        PECSFalse -> Nothing
        PECSTrue ->
            Just $ Signal { signalPath = path
                          , signalInterface = propertiesInterfaceName
                          , signalMember = "PropertiesChanged"
                          , signalBody =
                              [ DBV $
                                toRep ( iface
                                      , Map.fromList [( name
                                                      , DBVVariant $ toRep x )]
                                      , [] :: [Text])]}
        -- invalidates or emits changed but is write-only
        PECSInvalidates -> Just $
            Signal { signalPath = path
                   , signalInterface = propertiesInterfaceName
                   , signalMember = "PropertiesChanged"
                   , signalBody =
                       [ DBV $ toRep
                         ( iface
                         , Map.empty :: Map.Map Text (DBusValue (TypeVariant))
                         , [name]
                         )]}

propertyChanged :: (MonadIO m, Representable a) =>
                   Property (RepType a) -> a -> MethodHandlerT m ()
propertyChanged prop a =
    Foldable.mapM_ signal (propertyChangedSignal prop a)

emitPropertyChanged :: Representable a =>
                       Property (RepType a) -> a -> DBusConnection -> IO ()
emitPropertyChanged prop x con = do
    let mbSig = propertyChangedSignal prop x
    Foldable.forM_ mbSig $ flip emitSignal con

data RemoteProperty a = RP { rpEntity :: Text
                           , rpObject :: ObjectPath
                           , rpInterface :: Text
                           , rpName :: Text
                           } deriving (Show, Eq)

getProperty :: Representable a =>
               RemoteProperty a
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
               RemoteProperty a
            -> a
            -> DBusConnection
            -> IO (Either MethodError a)
setProperty rp x con = do
     callMethod (rpEntity rp) (rpObject rp) propertiesInterfaceName "Set"
                (rpInterface rp , rpName rp, DBVVariant $ toRep x) [] con