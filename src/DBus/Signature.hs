{-# LANGUAGE OverloadedStrings #-}
module DBus.Signature where

import           Control.Applicative ((<$>))
import           Control.Monad
import qualified Data.Attoparsec as AP
import qualified Data.Attoparsec.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BS
import           Data.Char
import qualified Data.IntMap as IMap
import           Data.Monoid
import qualified Data.Text as Text

import           DBus.Types

stToSignature :: DBusSimpleType -> Char
stToSignature TypeByte       = 'y'
stToSignature TypeBoolean    = 'b'
stToSignature TypeInt16      = 'n'
stToSignature TypeUInt16     = 'q'
stToSignature TypeInt32      = 'i'
stToSignature TypeUInt32     = 'u'
stToSignature TypeInt64      = 'x'
stToSignature TypeUInt64     = 't'
stToSignature TypeDouble     = 'd'
stToSignature TypeUnixFD     = 'h'
stToSignature TypeString     = 's'
stToSignature TypeObjectPath = 'o'
stToSignature TypeSignature  = 'g'

toSignature :: DBusType -> BS.ByteString
toSignature = BS.concat . BSL.toChunks . BS.toLazyByteString . toSignature'

toSignatures :: [DBusType] -> BS.ByteString
toSignatures = BS.concat . BSL.toChunks . BS.toLazyByteString . mconcat . map toSignature'

toSignature' :: DBusType -> BS.Builder
toSignature' (DBusSimpleType t) = BS.char8 $ stToSignature t
toSignature' (TypeArray t) = BS.char8 'a' <> toSignature' t
toSignature' (TypeStruct ts) = BS.char8 '('
                              <> mconcat (toSignature' <$> ts)
                              <> BS.char8 ')'
toSignature' (TypeDict kt vt) = BS.string8 "a{"
                               <> BS.char8 (stToSignature kt)
                               <> toSignature' vt
                               <> BS.char8 '}'
toSignature' TypeVariant = BS.char8 'v'

simpleTypeMap = IMap.fromList[ (ord 'y', TypeByte       )
                             , (ord 'b', TypeBoolean    )
                             , (ord 'n', TypeInt16      )
                             , (ord 'q', TypeUInt16     )
                             , (ord 'i', TypeInt32      )
                             , (ord 'u', TypeUInt32     )
                             , (ord 'x', TypeInt64      )
                             , (ord 't', TypeUInt64     )
                             , (ord 'd', TypeDouble     )
                             , (ord 'h', TypeUnixFD     )
                             , (ord 's', TypeString     )
                             , (ord 'o', TypeObjectPath )
                             , (ord 'g', TypeSignature  )
                             ]

simpleType = do
    c <- AP.anyWord8
    case IMap.lookup (fromIntegral c) simpleTypeMap of
        Nothing -> fail "not a simple type"
        Just t -> return t

dictEntrySignature = do
    AP.char8 '{'
    kt <- simpleType
    vt <- signature
    AP.string "}"
    return $ TypeDictEntry kt vt


arraySignature = do
    AP.char8 'a'
    ((do TypeDictEntry kt vt <- dictEntrySignature
         return $ TypeDict kt vt)
      <> (TypeArray <$> signature))



structSignature = do
    AP.char '('
    TypeStruct <$> AP.manyTill signature (AP.char ')')


signature = AP.choice [ AP.char 'v' >> return TypeVariant
                      , arraySignature
                      , structSignature
                      , DBusSimpleType <$> simpleType
                      ]

eitherParseSig :: BS.ByteString -> Either Text.Text DBusType
eitherParseSig s = case AP.eitherResult $ AP.parse signature s of
    Left e -> Left $ Text.pack e
    Right r -> Right r

parseSig :: BS.ByteString -> Maybe DBusType
parseSig = AP.maybeResult . AP.parse signature

parseSigs :: BS.ByteString -> Maybe [DBusType]
parseSigs = AP.maybeResult . AP.parse (AP.many1 signature)

-- fromSignature (v:vs) = TypeVariant :
-- fromSignature "v" = Just TypeVariant
