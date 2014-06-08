{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DBus.Types where

import           Control.Applicative
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Error
import           Control.Monad.Writer.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Data (Data)
import           Data.Function (fix, on)
import           Data.Int
import           Data.List
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Singletons (withSingI)
import           Data.Singletons.Prelude.Bool
import           Data.Singletons.Prelude.List
import           Data.Singletons.TH hiding (Error)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.Word
import           Unsafe.Coerce (unsafeCoerce)

-- import qualified DBus.Connection as DBus
-- import qualified DBus.Message as DBus


data ObjectPath = ObjectPath { opAbsolute :: Bool
                             , opParts :: [Text.Text]
                             } deriving (Eq, Data, Typeable)


type InterfaceName = Text.Text
type MemberName = Text.Text


newtype Signature = Signature {fromSignature :: [DBusType]}
                  deriving (Show, Eq)

-- | Parse an object path. Contrary to the standard, empty path parts are ignored
objectPath txt = case Text.uncons txt of
    Just ('/', rest) -> ObjectPath True
                             $ filter (not. Text.null) $ Text.splitOn "/" rest
    Just _ -> ObjectPath False $ filter (not. Text.null) $ Text.splitOn "/" txt
    Nothing -> ObjectPath False []
objectPathToText (ObjectPath abs parts) = (if abs then "/" else "")
                                          `Text.append` Text.intercalate "/" parts

instance Show ObjectPath where
    show = Text.unpack . objectPathToText

stripObjectPrefix :: ObjectPath -> ObjectPath -> Maybe ObjectPath
stripObjectPrefix (ObjectPath abs1 pre) (ObjectPath abs2 x) | abs1 == abs2
                                        = ObjectPath False <$> stripPrefix pre x
stripObjectPrefix _ _ = Nothing

isPathPrefix :: ObjectPath -> ObjectPath -> Bool
isPathPrefix p x = case stripObjectPrefix p x of
    Nothing -> False
    Just _ -> True

isRoot (ObjectPath True p) = null p
isRoot _ = False

isEmpty (ObjectPath False p) = null p
isEmpty _ = False

-- | Types that are not composite. These can be the keys of a Dict
data DBusSimpleType
    = TypeByte
    | TypeBoolean
    | TypeInt16
    | TypeUInt16
    | TypeInt32
    | TypeUInt32
    | TypeInt64
    | TypeUInt64
    | TypeDouble
    | TypeUnixFD
    | TypeString
    | TypeObjectPath
    | TypeSignature
      deriving (Show, Read, Eq, Data, Typeable)

ppSimpleType :: DBusSimpleType -> String
ppSimpleType TypeByte       = "Word8"
ppSimpleType TypeBoolean    = "Boolean"
ppSimpleType TypeInt16      = "Int16"
ppSimpleType TypeUInt16     = "UInt16"
ppSimpleType TypeInt32      = "Int32"
ppSimpleType TypeUInt32     = "UInt32"
ppSimpleType TypeInt64      = "Int64"
ppSimpleType TypeUInt64     = "UInt64"
ppSimpleType TypeDouble     = "Double"
ppSimpleType TypeUnixFD     = "UnixFD"
ppSimpleType TypeString     = "String"
ppSimpleType TypeObjectPath = "ObjectPath"
ppSimpleType TypeSignature  = "Signature"

data DBusType
    = DBusSimpleType DBusSimpleType
    | TypeArray DBusType
    | TypeStruct [DBusType]
    | TypeDict DBusSimpleType DBusType
    | TypeVariant
    | TypeDictEntry DBusSimpleType DBusType
    | TypeUnit -- TODO: Remove
      -- Unit isn't actually a DBus type. It is included
      -- to make it easier to use methods without a return value
      deriving (Show, Read, Eq, Data, Typeable)

ppType :: DBusType -> String
ppType (DBusSimpleType t) = ppSimpleType t
ppType (TypeArray ts) = "[" ++ ppType ts ++ "]"
ppType (TypeStruct ts) = "(" ++ intercalate "," (ppType <$> ts) ++ ")"
ppType (TypeDict k v) = "{" ++ ppSimpleType k ++ " => " ++ ppType v ++ "}"
ppType (TypeDictEntry k v) = "<" ++ ppSimpleType k ++ " => " ++ ppType v ++ ">"
ppType TypeVariant = "Variant"
ppType TypeUnit = "()"

data Parity = Null
            | Arg Parity
              deriving (Eq, Show, Data, Typeable)


genSingletons [''DBusSimpleType, ''DBusType, ''Parity]
singEqInstances [''DBusSimpleType, ''DBusType, ''Parity]
singDecideInstances [''DBusSimpleType, ''DBusType]

-- | A Transformer for (IO) actions that might want to send a signal.
newtype SignalT m a = SignalT { unSignal :: WriterT [Signal] m a}
                      deriving ( Functor
                               , Applicative
                               , Monad
                               , MonadTrans )

data Signal = Signal { signalPath :: ObjectPath
                     , signalInterface :: InterfaceName
                     , signalMember :: MemberName
                     , signalBody :: [SomeDBusValue]
                     }

runSignalT :: SignalT m a -> m (a, [Signal])
runSignalT (SignalT w) = runWriterT w

signal :: Monad m => Signal -> SignalT m ()
signal sig = SignalT $ tell [sig]



type family ArgsOf x :: Parity where
     ArgsOf (IO x) = 'Null
     ArgsOf (SignalT IO x) = 'Null
     ArgsOf (a -> b) = 'Arg (ArgsOf b)

infixr 0 :>
data ResultDescription parity where
    (:>) :: Text.Text -> ResultDescription n -> ResultDescription (Arg n)
    ResultDone :: ResultDescription 'Null

infixr 0 :->
data ArgumentDescription parity where
    (:->) :: Text.Text -> ArgumentDescription n -> ArgumentDescription (Arg n)
    Result :: ArgumentDescription 'Null


data DBusArguments :: [DBusType] -> * where
    ArgsNil :: DBusArguments '[]
    ArgsCons :: DBusValue a -> DBusArguments as -> DBusArguments (a ': as)

data SomeDBusArguments where
    SDBA :: SingI ts => DBusArguments ts -> SomeDBusArguments

deriving instance Show SomeDBusArguments

listToSomeArguments :: [SomeDBusValue] -> SomeDBusArguments
listToSomeArguments [] = SDBA ArgsNil
listToSomeArguments (DBV v : xs) =
    case listToSomeArguments xs of
        SDBA sdba -> SDBA (ArgsCons v sdba)

argsToValues :: SomeDBusArguments -> [SomeDBusValue]
argsToValues (SDBA (a :: DBusArguments t)) = argsToValues' (sing :: Sing t) a
  where
    argsToValues' :: Sing ts -> DBusArguments ts -> [SomeDBusValue]
    argsToValues' (SNil) ArgsNil = []
    argsToValues' (SCons t ts) (ArgsCons a as) =
        withSingI t $ (DBV a) : argsToValues' ts as

argsToStruct :: DBusArguments (t ': ts) -> DBusStruct (t ': ts)
argsToStruct (ArgsCons x ArgsNil) = StructSingleton x
argsToStruct (ArgsCons x xs@(ArgsCons _ _)) = StructCons x (argsToStruct xs)

structToArgs :: DBusStruct ts -> DBusArguments ts
structToArgs (StructSingleton v) = ArgsCons v ArgsNil
structToArgs (StructCons v vs) = ArgsCons v (structToArgs vs)

maybeArgsToStruct :: (SingI ts, SingI ss) =>
                     DBusArguments ts
                  -> Maybe (DBusStruct ss)
maybeArgsToStruct (args :: DBusArguments (ts :: [DBusType])) =
    fix $ \(_ :: Maybe (DBusStruct (ss :: [DBusType]))) ->
    let singt = sing :: Sing ts
        sings = sing :: Sing ss
    in case singt of
        SNil -> Nothing
        SCons t' ts'  -> case singt %~ sings of
            Proved Refl -> withSingI ts' (Just $ argsToStruct args)
            Disproved _ -> Nothing


singletonArg :: DBusValue a -> DBusArguments '[a]
singletonArg x = ArgsCons x ArgsNil

instance Eq (DBusArguments t) where
    ArgsNil == ArgsNil = True
    ArgsCons x xs == ArgsCons y ys =
        x == y && xs == ys

instance SingI a => Show (DBusArguments a) where
    show xs = showArgs sing xs

showArgs :: Sing a -> DBusArguments a -> String
showArgs (SNil) ArgsNil = "ArgsNil"
showArgs (SCons t ts) (ArgsCons x xs) =
    withSingI t $ "ArgsCons (" ++ show x  ++ ") (" ++ showArgs ts xs ++ ")"

data DBusStruct :: [DBusType] -> * where
    StructSingleton :: DBusValue a -> DBusStruct '[a]
    StructCons :: DBusValue a -> DBusStruct as -> DBusStruct (a ': as)

instance Eq (DBusStruct t) where
    StructSingleton x == StructSingleton y = x == y
    StructCons x xs == StructCons y ys =
        x == y && xs == ys

data SomeDBusStruct where
    SDBS :: SingI ts => DBusStruct ts -> SomeDBusStruct

instance SingI a => Show (DBusStruct a) where
    show xs = showStruct sing xs

showStruct :: Sing a -> DBusStruct a -> String
showStruct (SCons t SNil) (StructSingleton x) =
    withSingI t $ "StructSingleton (" ++ show x ++ ")"
showStruct (SCons t ts ) (StructCons x xs) =
    withSingI t $ "StructCons (" ++ show x  ++ ") (" ++ showStruct ts xs ++ ")"

data DBusValue :: DBusType -> * where
    DBVByte       :: Word8         -> DBusValue ('DBusSimpleType TypeByte)
    DBVBool       :: Bool          -> DBusValue ('DBusSimpleType TypeBoolean)
    DBVInt16      :: Int16         -> DBusValue ('DBusSimpleType TypeInt16)
    DBVUInt16     :: Word16        -> DBusValue ('DBusSimpleType TypeUInt16)
    DBVInt32      :: Int32         -> DBusValue ('DBusSimpleType TypeInt32)
    DBVUInt32     :: Word32        -> DBusValue ('DBusSimpleType TypeUInt32)
    DBVInt64      :: Int64         -> DBusValue ('DBusSimpleType TypeInt64)
    DBVUInt64     :: Word64        -> DBusValue ('DBusSimpleType TypeUInt64)
    DBVDouble     :: Double        -> DBusValue ('DBusSimpleType TypeDouble)
    DBVUnixFD     :: Word32        -> DBusValue ('DBusSimpleType TypeUnixFD)
    DBVString     :: Text.Text     -> DBusValue ('DBusSimpleType TypeString)
    DBVObjectPath :: ObjectPath    -> DBusValue ('DBusSimpleType TypeObjectPath)
    DBVSignature  :: [DBusType]    -> DBusValue ('DBusSimpleType TypeSignature)
    DBVVariant    :: (SingI t )    => DBusValue t -> DBusValue TypeVariant
    DBVArray      :: [DBusValue a] -> DBusValue (TypeArray a)
    DBVByteArray  :: BS.ByteString -> DBusValue (TypeArray ('DBusSimpleType TypeByte))
    DBVStruct     :: DBusStruct ts -> DBusValue (TypeStruct ts)
    DBVDict       :: [(DBusValue ('DBusSimpleType k) ,DBusValue v)]
                                   -> DBusValue (TypeDict k v)
    -- TODO: Remove

    -- | Unit isn't an actual DBus type and is included only for use with methods
    -- that don't return a value.
    DBVUnit       :: DBusValue TypeUnit


instance Eq (DBusValue t) where
    DBVByte       x ==  DBVByte       y = x == y
    DBVBool       x ==  DBVBool       y = x == y
    DBVInt16      x ==  DBVInt16      y = x == y
    DBVUInt16     x ==  DBVUInt16     y = x == y
    DBVInt32      x ==  DBVInt32      y = x == y
    DBVUInt32     x ==  DBVUInt32     y = x == y
    DBVInt64      x ==  DBVInt64      y = x == y
    DBVUInt64     x ==  DBVUInt64     y = x == y
    DBVDouble     x ==  DBVDouble     y = x == y
    DBVUnixFD     x ==  DBVUnixFD     y = x == y
    DBVString     x ==  DBVString     y = x == y
    DBVObjectPath x ==  DBVObjectPath y = x == y
    DBVSignature  x ==  DBVSignature  y = x == y
    DBVVariant (x ::DBusValue s1) ==  DBVVariant (y ::DBusValue s2) =
        let xt = sing :: Sing s1
            yt = sing :: Sing s2
        in case xt %:== yt of -- Should be %~
           STrue  -> (unsafeCoerce x :: DBusValue t) == (unsafeCoerce y)
           SFalse -> False

    DBVArray      x ==  DBVArray      y = x == y
    DBVByteArray  x ==  DBVByteArray  y = x == y
    DBVStruct     x ==  DBVStruct     y = x == y
    DBVDict       x ==  DBVDict       y = x == y
    DBVUnit         ==  DBVUnit         = True
    DBVArray      x == DBVByteArray   y = BS.pack (map (\(DBVByte w) -> w) x) == y
    DBVByteArray  x == DBVArray       y = BS.pack (map (\(DBVByte w) -> w) y) == x
    _               ==  _               = False


-- TODO: Reinstate once https://github.com/goldfirere/singletons/issues/2 is
-- resolved

-- fromVariant :: SingI t => DBusValue TypeVariant -> Maybe (DBusValue t)
-- fromVariant (DBVVariant (v :: DBusValue s))
--     = fix $ \(_ :: Maybe (DBusValue t)) ->
--         let ss = (sing :: Sing s)
--             st = (sing :: Sing t)
--         in case (ss %~ st) of
--             Proved Refl -- Bring into scope a proof that s~t
--                 -> Just v
--             Disproved _ -> Nothing

castDBV :: (SingI s, SingI t) => DBusValue s -> Maybe (DBusValue t)
castDBV (v :: DBusValue s)
    = fix $ \(_ :: Maybe (DBusValue t)) ->
        let ss = (sing :: Sing s)
            st = (sing :: Sing t)
        in case (ss %:== st) of
            STrue -> Just (unsafeCoerce v)
            SFalse -> Nothing

data SomeDBusValue where
    DBV :: SingI t => DBusValue t -> SomeDBusValue

instance Show SomeDBusValue where
    show (DBV x) = "DBV<"++ ppType (typeOf x) ++ "> (" ++ show x ++ ")"

dbusValue :: SingI t => SomeDBusValue -> Maybe (DBusValue t)
dbusValue (DBV v) = castDBV v

dbusSValue :: SingI t => SomeDBusValue -> Maybe (DBusValue ('DBusSimpleType t))
dbusSValue (DBV v) = castDBV v

-- | Extract a DBusValue from a Variant iff the type matches or return nothing
fromVariant :: SingI t => DBusValue TypeVariant -> Maybe (DBusValue t)
fromVariant (DBVVariant v) = castDBV v

instance SingI t => Show (DBusValue t) where
    show (DBVByte       x) = "DBVByte " ++ show x
    show (DBVBool       x) = "DBVBool " ++ show x
    show (DBVInt16      x) = "DBVInt16 " ++ show x
    show (DBVUInt16     x) = "DBVUInt16 " ++ show x
    show (DBVInt32      x) = "DBVInt32 " ++ show x
    show (DBVUInt32     x) = "DBVUInt32 " ++ show x
    show (DBVInt64      x) = "DBVInt64 " ++ show x
    show (DBVUInt64     x) = "DBVUInt64 " ++ show x
    show (DBVDouble     x) = "DBVDouble " ++ show x
    show (DBVUnixFD     x) = "DBVUnixFD " ++ show x
    show (DBVString     x) = "DBVString " ++ show x
    show (DBVObjectPath x) = "objectPath " ++ show (show x)
    show (DBVSignature  x) = "DBVSignature " ++ show x
    show y@(DBVArray    x :: DBusValue t) = case (sing :: Sing t) of
        STypeArray t -> withSingI t $
          "DBVArray " ++ show x ++
            if null x
            then " :: DBusValue (" ++ (show $ typeOf y ) ++ ")"
            else ""


    show y@(DBVByteArray  x) = "DBVByteArray " ++ show x
    show y@(DBVStruct     x :: DBusValue t) = case (sing :: Sing t) of
        STypeStruct ts -> withSingI ts $
            "DBVStruct (" ++ show x ++ ")"
    show y@(DBVVariant   x ) = "DBVVariant (" ++ show x ++ ")"
    show y@(DBVDict      x :: DBusValue t ) = case (sing :: Sing t) of
        STypeDict kt vt -> withSingI kt $ withSingI vt $
            "DBDict (" ++ show x ++ ")" ++
              if null x
              then " :: " ++ show (typeOf y)
              else ""
    show y@(DBVUnit       ) = "DBVUnit"

typeOf :: SingI t => DBusValue t -> DBusType
typeOf (_ :: DBusValue a) = fromSing (sing :: SDBusType a)

-- | Class of types that can be represented in the D-Bus type system.
--
-- The toRep and fromRep functions form a Prism and should follow the "obvious"
-- laws:
--
-- * @fromRep (toRep x) == Just x@
--
-- * @fmap toRep (fromRep x) =<= Just x @
--
--   (where @x =<= y@ iff @x@ is @Nothing@ or @x == y@)
--
-- All 'DBusValues' represent themselves and instances for
-- the following "canonical" pairs are provided
--
-- Haskell type => D-Bus type
--
-- * Word/X/ and Int/X/ => UInt/X/ and Int/X/ respectively
-- (for /X/ in {16, 32, 64})
--
-- * 'Bool' => Boolean
--
-- * 'Word8' => Byte
--
-- * 'Double' => Double
--
-- * 'Text' => String
--
-- * 'ObjectPath' => ObjectPath
--
-- * 'DBusType' => Signature
--
-- * [a] => Array of a (for Representable a)
--
-- * 'ByteString' => Array of Byte
--
-- * Tuples up to length 20 => Structs of equal length where each of the members
-- is itself Representable
--
-- * 'Map' => Dict where the keys can be represented by a 'DBusSimpleType'
--
-- An instance for 'String' is impossible because it conflicts with the instance
-- for lists (use Text instead)
--
-- Also note that no Representable instances are provided for 'Int', 'Integer'
-- and 'Float'.
--
-- You can automatically derive an instance for your own Types with
-- 'makeRepresentable'
class SingI (RepType a) => Representable a where
    -- | The 'DBusType' that represents this type
    type RepType a :: DBusType
    -- | Conversion from Haskell to D-Bus types
    toRep :: a -> DBusValue (RepType a)
    -- | Conversion from D-Bus to Haskell types.
    fromRep :: DBusValue (RepType a) -> Maybe a


------------------------------------------------
-- Objects
------------------------------------------------



data MethodWrapper av rv where
    MReturn :: SingI ts => SignalT IO (DBusArguments ts) -> MethodWrapper '[] ts
    MAsk    :: SingI t => (DBusValue t -> MethodWrapper avs rv )
                       -> MethodWrapper (t ': avs) rv

type family ArgParity (x :: [DBusType]) :: Parity where
    ArgParity '[] = 'Null
    ArgParity (x ': xs) = Arg (ArgParity xs)

data Method where
    Method :: (SingI avs, SingI ts) =>
              MethodWrapper avs ts
           -> Text.Text
           -> ArgumentDescription (ArgParity avs)
           -> ResultDescription   (ArgParity ts)
           -> Method

data PropertyEmitsChangedSignal = PECSTrue
                                | PECSInvalidates
                                | PECSFalse


data PropertyWrapper t where
    PropertyWrapper :: forall t. SingI t =>
                       { setProperty :: Maybe (DBusValue t -> SignalT IO Bool)
                         -- | ^ setter for the property. Returns
                       , getProperty :: Maybe (SignalT IO (DBusValue t))
                       } -> PropertyWrapper t

data Property where
    Property :: forall t . (SingI t) =>
                { propertyName :: Text.Text
                , propertyAccessors :: PropertyWrapper t
                , propertyEmitsChangedSignal :: PropertyEmitsChangedSignal
                } -> Property

propertyType :: Property -> DBusType
propertyType Property{propertyAccessors = accs :: PropertyWrapper t}
    = fromSing (sing :: Sing t)

data Annotation = Annotation { annotationName :: Text.Text
                             , annotationValue :: Text.Text
                             } deriving (Eq, Show, Data, Typeable)


data SignalArgument =
    SignalArgument { signalArgumentName :: Text.Text
                   , signalArgumentType :: DBusType
                   }

data SignalInterface = SignalI { signalName :: Text.Text
                               , signalArguments :: [SignalArgument]
                               , signalAnnotations :: [Annotation]
                               }

data Interface = Interface { interfaceName :: Text.Text
                           , interfaceMethods :: [Method]
                           , interfaceAnnotations :: [Annotation]
                           , interfaceSignals :: [SignalInterface]
                           , interfaceProperties :: [Property]
                           }

instance Eq Interface where
    (==) = (==) `on` interfaceName

data Object = Object { objectObjectPath :: ObjectPath
                     , objectInterfaces :: [Interface]
                     , objectSubObjects :: [Object]
                     }

--------------------------------------------------
-- Connection and Message
--------------------------------------------------

data MsgError = MsgError { errorName :: Text.Text
                         , errorText :: Maybe Text.Text
                         , errorBody :: [SomeDBusValue]
                         } deriving (Show, Typeable)

instance Ex.Exception MsgError

instance Error MsgError where
    strMsg str = MsgError { errorName = "org.freedesktop.DBus.Error.Failed"
                          , errorText = Just (Text.pack str)
                          , errorBody = []
                          }
    noMsg = MsgError { errorName = "org.freedesktop.DBus.Error.Failed"
                     , errorText = Nothing
                     , errorBody = []
                     }


data Connection = Connection { primConnection :: () -- DBus.Connection
                             , answerSlots :: TVar (Map.Map Word32
                                                    (TMVar (Either MsgError
                                                                  SomeDBusValue)))
                             , mainLoop :: ThreadId
                             }

data MethodError = MethodErrorMessage [SomeDBusValue]
                 | MethodSignatureMissmatch [SomeDBusValue]
                   deriving (Show, Typeable)

instance Ex.Exception MethodError

type Serial = Word32
type Slot = Either [SomeDBusValue] [SomeDBusValue] -> STM ()
type AnswerSlots = Map.Map Serial Slot

data DBusConnection =
    DBusConnection
        { dBusCreateSerial :: STM Serial
        , dBusAnswerSlots :: TVar AnswerSlots
        , dBusWriteLock :: TMVar (BS.Builder -> IO ())
        , dBusConnectionName :: Text.Text
        , connectionAliveRef :: TVar Bool
        }
