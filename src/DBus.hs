module DBus
    (
-- * Connection management
      DBusConnection
    , ConnectionType(..)
    , connectClient
    , connectClientWithAuth
    , makeServer
    , connectBus
    , MethodCallHandler
    , SignalHandler
    , checkAlive
    , waitFor
-- * Message handling
    , objectRoot
    , ignore
-- * Signals
    , MatchRule(..)
    , Signal(..)
    , SignalDescription(..)
    , SomeSignalDescription(..)
    , matchAll
    , matchSignal
    , addMatch
    , removeMatch
    , addSignalHandler
    , signalChan
    , handleSignal
-- * Representable Types
    , Representable(..)
    , FlattenRepType
    , makeRepresentable
    , makeRepresentableTuple
-- * DBus specific types
-- ** DBus Types
-- $Types
    , DBusSimpleType(..)
    , DBusType(..)
    , Signature(..)
    , typeOf
-- ** DBus Values
    , DBusValue(..)
    , castDBV
    , DBusStruct(..)
    , SomeDBusValue(..)
    , dbusValue
    , fromVariant
-- ** Objects
    , Object(..)
    , Interface(..)
    , ObjectPath
    , objectPath
    , objectPathToText
    , stripObjectPrefix
    , isPathPrefix
    , isRoot
    , isEmpty
-- * Methods
    , Method(..)
    , MethodWrapper(..)
    , ArgumentDescription(..)
    , repMethod
    , callMethod
    , callMethod'
    , call
    , callAsync
    , fromResponse
    , MsgError(..)
    , MethodError(..)
    , MethodHandlerT(..)
    , MethodDescription(..)
    , SomeMethodDescription(..)
-- * Properties
    , Property (..)
    , SomeProperty(..)
    , PropertyEmitsChangedSignal(..)
    , RemoteProperty(..)
    , propertyChanged
    , emitPropertyChanged
    , getProperty
    , setProperty
    , handlePropertyChanged
    , mkProperty
    , mkTVarProperty
-- * Introspection
    , addIntrospectable
-- * Message Bus
    , requestName
    , RequestNameFlag(..)
    , RequestNameReply(..)
    , releaseName
    , ReleaseNameReply (..)
    , listQueuedOwners
    , listNames
    , listActivatableNames
    , nameHasOwner
    , startServiceByName
    , getNameOwner
    , getConnectionUnixUser
    , getConnectionProcessID
    , getID
-- * Scaffolding
    , module DBus.Scaffold
-- * Re-exports
    , SingI (..)
    ) where

import qualified Data.ByteString as BS

import           DBus.Auth
import           DBus.Introspect
import           DBus.MainLoop
import           DBus.Message
import           DBus.MessageBus
import           DBus.Method
import           DBus.Property
import           DBus.Scaffold
import           DBus.Signal
import           DBus.TH
import           DBus.Types
import           Data.Default    (def)
import           Data.Singletons

-- | Ignore all incoming messages/signals
ignore :: Monad m => a -> b -> c -> m ()
ignore _ _ _ = return ()


-- | Connect to a message bus as a client, using the @EXTERNAL@ auth mechanism.
connectClient :: ConnectionType -> IO DBusConnection
connectClient bus = connectBus bus ignore ignore

-- | Connect to a message bus as a client with a custom auth mechanism.
connectClientWithAuth :: ConnectionType -> SASL BS.ByteString -> IO DBusConnection
connectClientWithAuth bus auth = connectBusWithAuth bus auth ignore ignore

-- $types
--
-- DBus has it's own type system, described here
-- <https://dbus.freedesktop.org/doc/dbus-specification.html#type-system>
--
-- Types are divided into basic types, represented in this library by
-- 'DBusSimpleType', and composite types, represented by 'DBusType'. Only simple
-- types can be the keys in a dictionary
