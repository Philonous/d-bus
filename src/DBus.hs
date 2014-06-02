module DBus
    (
-- * Connection management
      ConnectionType(..)
    , connectBus
    , MethodCallHandler
    , SignalHandler
    , checkAlive
    , waitFor
-- * Message handling
    , objectRoot
-- * Signals
    , MatchRule(..)
    , matchAll
    , matchSignal
    , addMatch
    , removeMatch
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
    , MethodDescription(..)
    , repMethod
    , callMethod
    , callMethod'
    , MsgError(..)
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
    ) where

import           DBus.Introspect
import           DBus.MainLoop
import           DBus.MessageBus
import           DBus.Object
import           DBus.Types
import           DBus.Signal
import           DBus.TH
import           DBus.Message

-- | Ignore all incoming messages/signals
ignore _ _ _ = return ()
