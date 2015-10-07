{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DBus.Types where

import           Control.Applicative
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Writer.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Data (Data)
import           Data.Function (fix, on)
import           Data.Int
import           Data.List
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Singletons (withSingI)
import           Data.Singletons.Prelude.Bool
import           Data.Singletons.Prelude.List hiding (Map)
import           Data.Singletons.TH hiding (Error)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Data.Word
import           System.Log.Logger
import           Unsafe.Coerce (unsafeCoerce)

dbusLogger :: String
dbusLogger = "DBus"

logDebug :: String -> IO ()
logDebug = debugM dbusLogger

logWarning :: String -> IO ()
logWarning = warningM dbusLogger

logError :: String -> IO ()
logError = errorM dbusLogger


data ObjectPath = ObjectPath { opAbsolute :: Bool
                             , opParts :: [Text]
                             } deriving (Eq, Data, Typeable, Ord)

type InterfaceName = Text
type MemberName = Text

opNode op = ObjectPath { opAbsolute = False
                       , opParts = take 1 . reverse $ opParts op
                       }

opPath op = ObjectPath { opAbsolute = opAbsolute op
                       , opParts = case opParts op of
                           [] -> []
                           opps -> init opps
                       }


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

instance IsString ObjectPath where
    fromString = objectPath . Text.pack

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


singletons [d|
  flattenRepType :: DBusType -> [DBusType]
  flattenRepType TypeUnit              = []
  flattenRepType (TypeStruct ts)       = ts
  flattenRepType t@(DBusSimpleType _ )  = [t]
  flattenRepType t@(TypeArray _)  = [t]
  flattenRepType t@(TypeDict _ _)  = [t]
  flattenRepType t@(TypeDictEntry _ _)  = [t]
  flattenRepType t@(TypeVariant)  = [t]
  |]

-- | A Transformer for (IO) actions that might want to send a signal.
newtype MethodHandlerT m a =
    MHT { unMHT :: ExceptT MsgError (WriterT [SomeSignal] m) a}
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             )

methodError :: Monad m => MsgError -> MethodHandlerT m a
methodError = MHT . throwError
-- instance MonadError MethodHandlerT

catchMethodError :: Monad m =>
                    MethodHandlerT m a
                 -> (MsgError -> MethodHandlerT m a)
                 -> MethodHandlerT m a
catchMethodError m f = MHT $ catchError (unMHT m) (unMHT . f)

instance MonadTrans MethodHandlerT where
    lift = MHT . lift . lift

instance (Functor m, Monad m) => Alternative (MethodHandlerT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (MethodHandlerT m) where
    mzero = methodError (MsgError "org.freedesktop.DBus.Error.Failed" Nothing [])
    mplus (MHT m) (MHT n) = MHT . ExceptT $ do
        res <- runExceptT m
        case res of
         Left e -> runExceptT n
         Right r -> return $ Right r

runMethodHandlerT :: MethodHandlerT m a -> m (Either MsgError a, [SomeSignal])
runMethodHandlerT (MHT w) = runWriterT $ runExceptT w

data Signal a = Signal { signalPath :: ObjectPath
                       , signalInterface :: InterfaceName
                       , signalMember :: MemberName
                       , signalBody :: DBusArguments a
                       } deriving (Show)

data SomeSignal where
    SomeSignal :: SingI a => Signal a -> SomeSignal

deriving instance Show SomeSignal

data SignalDescription a = SignalDescription
                           { signalDPath :: ObjectPath
                           , signalDInterface :: InterfaceName
                           , signalDMember :: MemberName
                           , signalDArguments :: ArgumentDescription (ArgParity a)
                           } deriving (Typeable)

signalDArgumentTypes :: SingI ts => SignalDescription ts -> [DBusType]
signalDArgumentTypes (_ :: SignalDescription ts) = fromSing (sing :: Sing ts)

instance SingI a => Show (SignalDescription a) where
    show (sd :: SignalDescription ts)
        = "SignalDescription{ signalDPath = " ++ show (signalDPath sd)
          ++ ", signalDInterface = "
          ++ show (signalDInterface sd)
          ++ ", signalDMember = " ++ show (signalDMember sd)
          ++ ", signalDArgumentTypes = "
          ++ show (fromSing $ (sing :: Sing ts))
          ++ ",signalDArguments = "
          ++ show (adToList $ signalDArguments sd)
          ++ "}"

instance Eq (SignalDescription a) where
    x == y = and [ ((==) `on` signalDPath) x y
                 , ((==) `on` signalDInterface) x y
                 , ((==) `on` signalDMember) x y
                 -- argument types are guaranteed to be equal
                 , adToList (signalDArguments x) == adToList (signalDArguments y)
                 ]

data SomeSignalDescription where
    SSD :: forall (a :: [DBusType]) . SingI a =>
           SignalDescription a -> SomeSignalDescription
    deriving (Typeable)

deriving instance Show SomeSignalDescription

instance Eq SomeSignalDescription where
    (SSD (x :: SignalDescription a)) == (SSD (y :: SignalDescription b))
        = case (sing  :: Sing a) %~ (sing :: Sing b) of
           Proved (Refl{}) -> x == y
           Disproved{} -> False

type family ArgsOf x :: Parity where
     ArgsOf (IO x) = 'Null
     ArgsOf (MethodHandlerT IO x) = 'Null
     ArgsOf (a -> b) = 'Arg (ArgsOf b)

type family ArgParity (x :: [DBusType]) :: Parity where
    ArgParity '[] = 'Null
    ArgParity (x ': xs) = Arg (ArgParity xs)

infixr 0 :>
data ArgumentDescription parity where
    (:>) :: Text -> ArgumentDescription n -> ArgumentDescription (Arg n)
    Done :: ArgumentDescription 'Null
            deriving (Typeable)

adToList :: ArgumentDescription n -> [Text]
adToList Done = []
adToList (x :> xs) = x : adToList xs

instance Show (ArgumentDescription n) where
    show res = show $ adToList res

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
    DBVString     :: Text     -> DBusValue ('DBusSimpleType TypeString)
    DBVObjectPath :: ObjectPath    -> DBusValue ('DBusSimpleType TypeObjectPath)
    DBVSignature  :: [DBusType]    -> DBusValue ('DBusSimpleType TypeSignature)
    DBVVariant    :: (SingI t )    => DBusValue t -> DBusValue TypeVariant
    DBVArray      :: [DBusValue a] -> DBusValue (TypeArray a)
    DBVByteArray  :: BS.ByteString -> DBusValue (TypeArray ('DBusSimpleType TypeByte))
    DBVStruct     :: DBusStruct ts -> DBusValue (TypeStruct ts)
    DBVDict       :: [(DBusValue ('DBusSimpleType k) ,DBusValue v)]
                                   -> DBusValue (TypeDict k v)
    DBVUnit       :: DBusValue TypeUnit -- How to get rid of this?
    -- Unit isn't an actual DBus type and is included only for use with methods
    -- that don't return a value.


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

castDBV :: (SingI s, SingI t) => DBusValue s -> Maybe (DBusValue t)
castDBV (v :: DBusValue s)
    = fix $ \(_ :: Maybe (DBusValue t)) ->
        let ss = (sing :: Sing s)
            st = (sing :: Sing t)
        in case (ss %~ st) of
            Proved Refl -> Just v
            Disproved _ -> Nothing

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
    MReturn :: SingI ts => MethodHandlerT IO (DBusArguments ts) -> MethodWrapper '[] ts
    MAsk    :: SingI t => (DBusValue t -> MethodWrapper avs rv )
                       -> MethodWrapper (t ': avs) rv

data Method where
    Method :: (SingI avs, SingI ts) =>
              MethodWrapper avs ts
           -> Text
           -> ArgumentDescription (ArgParity avs)
           -> ArgumentDescription   (ArgParity ts)
           -> Method

data PropertyAccess = Read
                    | Write
                    | ReadWrite
                    deriving (Eq, Show, Data, Typeable)

data PropertyEmitsChangedSignal = PECSTrue
                                | PECSInvalidates
                                | PECSFalse

data Property t where
    Property :: forall t . (SingI t) =>
                { propertyPath :: ObjectPath
                , propertyInterface :: Text
                , propertyName :: Text
                , propertyGet :: Maybe (MethodHandlerT IO (DBusValue t))
                , propertySet :: Maybe (DBusValue t -> MethodHandlerT IO Bool)
                , propertyEmitsChangedSignal :: PropertyEmitsChangedSignal
                } -> Property t

data SomeProperty where
    SomeProperty :: forall t . (SingI t) =>
                    {fromSomeProperty :: Property t} -> SomeProperty

propertyType :: SingI t => Property t -> DBusType
propertyType (_ :: Property t) = fromSing (sing :: Sing t)

data RemoteProperty a = RP { rpEntity :: Text
                           , rpObject :: ObjectPath
                           , rpInterface :: Text
                           , rpName :: Text
                           } deriving (Show, Eq)


data Annotation = Annotation { annotationName :: Text
                             , annotationValue :: Text
                             } deriving (Eq, Show, Data, Typeable)


data SignalArgument =
    SignalArgument { signalArgumentName :: Text
                   , signalArgumentType :: DBusType
                   }

data Interface = Interface { interfaceMethods :: [Method]
                           , interfaceAnnotations :: [Annotation]
                           , interfaceSignals :: [SomeSignalDescription]
                           , interfaceProperties :: [SomeProperty]
                           }

instance Monoid Interface where
    mempty = Interface [] [] [] []
    (Interface m1 a1 s1 p1) `mappend` (Interface m2 a2 s2 p2) =
        Interface (m1 <> m2) (a1 <> a2) (s1 <> s2) (p1 <> p2)

newtype Object = Object {interfaces :: Map Text Interface }
instance Monoid Object where
    mempty = Object Map.empty
    mappend (Object o1) (Object o2) = Object $ Map.unionWith (<>) o1 o2

object interfaceName iface = Object $ Map.singleton interfaceName iface


newtype Objects = Objects {unObjects :: Map ObjectPath Object}

instance Monoid Objects where
    mempty = Objects Map.empty
    mappend (Objects o1) (Objects o2) = Objects $ Map.unionWith (<>) o1 o2

root path object = Objects $ Map.singleton path object

--------------------------------------------------
-- Connection and Message
--------------------------------------------------

data MsgError = MsgError { errorName :: Text
                         , errorText :: Maybe Text
                         , errorBody :: [SomeDBusValue]
                         } deriving (Show, Typeable)

instance Ex.Exception MsgError

data MethodError = MethodErrorMessage [SomeDBusValue]
                 | MethodSignatureMissmatch [SomeDBusValue]
                   deriving (Show, Typeable)

instance Ex.Exception MethodError

type Serial = Word32
type Slot = Either [SomeDBusValue] [SomeDBusValue] -> STM ()
type AnswerSlots = Map.Map Serial Slot

data Match a = Match a | MatchAny
             deriving (Show)

checkMatch :: Eq a => Match a -> Match a -> Bool
checkMatch MatchAny _ = True
checkMatch _ MatchAny = True
checkMatch (Match x) (Match y) = x == y

maybeToMatch :: Maybe a -> Match a
maybeToMatch Nothing = MatchAny
maybeToMatch (Just x) = Match x

data MatchSignal = MatchSignal { matchInterface :: Maybe Text
                               , matchMember :: Maybe Text
                               , matchPath :: Maybe ObjectPath
                               , matchSender :: Maybe Text
                               } deriving (Show, Eq, Ord)

anySignal = MatchSignal Nothing Nothing Nothing Nothing

type SignalSlots = [ (( Match Text
                      , Match Text
                      , Match ObjectPath
                      , Match Text)
                     , (SomeSignal -> IO ())) ]

type PropertySlots = Map ( ObjectPath
                         , Text -- Interface
                         , Text -- Member
                         )
                         [Maybe SomeDBusValue -> IO ()]


data DBusConnection =
    DBusConnection
        { dBusCreateSerial :: STM Serial
        , dBusAnswerSlots :: TVar AnswerSlots
        , dbusSignalSlots :: TVar SignalSlots
        , dbusPropertySlots :: TVar PropertySlots
        , dBusWriteLock :: TMVar (BS.Builder -> IO ())
        , dBusConnectionName :: Text
        , connectionAliveRef :: TVar Bool
        }

data MethodDescription args rets where
    MD ::
        { methodObjectPath :: ObjectPath
        , methodInterface :: Text
        , methodMember :: Text
        , methodArgs :: ArgumentDescription (ArgParity args)
        , methodResult :: ArgumentDescription (ArgParity rets)
        } -> MethodDescription args rets

data SomeMethodDescription where
    SMD :: (SingI args, SingI rets) => MethodDescription args rets
           -> SomeMethodDescription

instance Show (MethodDescription args rets) where
    show md = "Method " ++ show (methodObjectPath md)
              ++ " / " ++ Text.unpack (methodInterface md)
              ++ "." ++ Text.unpack (methodMember md)
