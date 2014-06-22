module DBus.Property where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Foldable as Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           DBus.Signal
import           DBus.Types
import           DBus.Error

-- | Create a property from a getter and a setter. It will emit a
-- PropertyChanged signal when the setter is called. To change
-- this behaviour modify the propertyEmitsChangedSignal field
mkProperty :: Representable a =>
              ObjectPath
           -> Text
           -> Text
           -> Maybe (IO a)
           -> Maybe (a -> IO Bool)
           -> Property
mkProperty path iface name get set =
    let pw = PropertyWrapper { setProperty = doSet <$> set
                             , getProperty = fmap toRep <$> get
                             }
        in Property { propertyPath = path
                    , propertyInterface = iface
                    , propertyName = name
                    , propertyAccessors = pw
                    , propertyEmitsChangedSignal = PECSTrue
                    }
  where
    doSet f v = lift $ f =<< fromRepHelper v
    fromRepHelper x = case fromRep x of
        Nothing -> Ex.throwIO argTypeMismatch
        Just r -> return r

-- | Make a property out of a TVar. The property is considered changed on every
-- set, no matter if the updated value is actually different from the old one
mkTVarProperty :: Representable a =>
                  ObjectPath
               -> Text
               -> Text
               -> TVar a
               -> Property
mkTVarProperty path iface name tv =
    mkProperty path iface name
        (Just (atomically $ readTVar tv))
        (Just (\v -> atomically (writeTVar tv v) >> return True))

-- | Interface for D-BUs properties
propertiesInterface = "org.freedesktop.DBus.Properties"

-- | Create a propertyChangedSignal for a property
propertyChangedSignal :: Property -> IO (Maybe Signal)
propertyChangedSignal prop = do
    let path = propertyPath prop
        iface = propertyInterface prop
        name = propertyName prop
    case (propertyEmitsChangedSignal prop, prop) of
        (PECSFalse, _) -> return Nothing
        (PECSTrue, Property{propertyAccessors =
                              PropertyWrapper{getProperty = Just gp}}) -> do
                    v <- liftIO gp
                    return . Just $
                        Signal { signalPath = path
                               , signalInterface = propertiesInterface
                               , signalMember = "PropertiesChanged"
                               , signalBody =
                                   [ DBV $
                                     toRep ( iface
                                           , Map.fromList [( name
                                                           , DBVVariant v)]
                                           , [] :: [Text])]}
        -- invalidates or emits changed but is write-only
        _ -> return . Just $
            Signal { signalPath = path
                   , signalInterface = propertiesInterface
                   , signalMember = "PropertiesChanged"
                   , signalBody =
                       [ DBV $ toRep
                         ( iface
                         , Map.empty :: Map.Map Text (DBusValue (TypeVariant))
                         , [name]
                         )]}

propertyChanged :: MonadIO m => Property -> MethodHandlerT m ()
propertyChanged prop =
    Foldable.mapM_ signal =<< liftIO (propertyChangedSignal prop)

emitPropertyChanged :: Property -> DBusConnection -> IO ()
emitPropertyChanged prop con = do
    mbSig <- propertyChangedSignal prop
    Foldable.forM_ mbSig $ flip emitSignal con
