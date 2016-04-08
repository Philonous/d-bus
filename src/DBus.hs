module DBus
    (
-- * Connection management
      DBusConnection
    , ConnectionType(..)
    , connectClient
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
    , def
    ) where

-- $types
--
-- DBus has it's own type system, described here
-- <https://dbus.freedesktop.org/doc/dbus-specification.html#type-system>
--
-- Types are divided into basic types, represented in this library by
-- 'DBusSimpleType', and composite types, represented by 'DBusType'. Only simple
-- types can be the keys in a dictionary

import DBus.Introspect
import DBus.MainLoop
import DBus.Message
import DBus.MessageBus
import DBus.Object
import DBus.Property
import DBus.Method
import DBus.Signal
import DBus.TH
import DBus.Types
import DBus.Scaffold
import Data.Default (def)

-- | Ignore all incoming messages/signals
ignore _ _ _ = return ()

-- | Connect to a message bus as a client
connectClient :: ConnectionType -> IO DBusConnection
connectClient bus = connectBus bus ignore ignore
