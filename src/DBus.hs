module DBus
    (
-- * Connection management
      ConnectionType(..)
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
    , matchAll
    , matchSignal
    , addMatch
    , removeMatch
    , addSignalHandler
    , signalChan
-- * Representable Types
    , Representable(..)
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
    , ResultDescription(..)
    , repMethod
    , callMethod
    , callMethod'
    , MsgError(..)
    , MethodHandlerT(..)
-- * Properties
    , Property (..)
    , SomeProperty(..)
    , PropertyEmitsChangedSignal(..)
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
