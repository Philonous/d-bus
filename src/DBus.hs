module DBus
    (
-- * Connection management
      DBusConnection
    , ConnectionType(..)
    , connectBus
    , makeServer
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
-- ** DBus Values
    , DBusValue(..)
    , castDBV
    , DBusStruct(..)
    , SomeDBusValue(..)
    , dbusValue
    , fromVariant
-- ** Signature
    , DBusSimpleType(..)
    , DBusType(..)
    , Signature(..)
    , typeOf
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
-- * Re-exports
    , def
    ) where

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
import Data.Default (def)

-- | Ignore all incoming messages/signals
ignore _ _ _ = return ()
