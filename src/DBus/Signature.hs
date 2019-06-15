{-# LANGUAGE OverloadedStrings #-}
module DBus.Signature where

import           Control.Applicative              ((<$>))
import qualified Data.Attoparsec.ByteString       as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Builder     as BS
import           Data.Char
import qualified Data.IntMap                      as IMap
import qualified Data.Text                        as Text

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
toSignature' (TypeDictEntry kt vt) = BS.string8 "e{"
                               <> BS.char8 (stToSignature kt)
                               <> toSignature' vt
                               <> BS.char8 '}'
toSignature' TypeVariant = BS.char8 'v'
toSignature' TypeUnit = ""

simpleTypeMap :: IMap.IntMap DBusSimpleType
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

simpleType :: AP.Parser DBusSimpleType
simpleType = do
    c <- AP.anyWord8
    case IMap.lookup (fromIntegral c) simpleTypeMap of
        Nothing -> fail "not a simple type"
        Just t -> return t

dictEntrySignature :: AP.Parser DBusType
dictEntrySignature = do
    _ <- AP.char8 '{'
    kt <- simpleType
    vt <- signature
    _ <- AP.string "}"
    return $ TypeDictEntry kt vt


arraySignature :: AP.Parser DBusType
arraySignature = do
    _ <- AP.char8 'a'
    ((do TypeDictEntry kt vt <- dictEntrySignature
         return $ TypeDict kt vt)
      <> (TypeArray <$> signature))


structSignature :: AP.Parser DBusType
structSignature = do
    _ <- AP.char '('
    TypeStruct <$> AP.manyTill signature (AP.char ')')


signature :: AP.Parser DBusType
signature = AP.choice [ AP.char 'v' >> return TypeVariant
                      , arraySignature
                      , structSignature
                      , DBusSimpleType <$> simpleType
                      ]

eitherParseSig :: BS.ByteString -> Either Text.Text DBusType
eitherParseSig s = case AP.parseOnly signature s of
    Left e -> Left $ Text.pack e
    Right r -> Right r

parseSig :: BS.ByteString -> Maybe DBusType
parseSig s = case eitherParseSig s of
    Left _ -> Nothing
    Right r -> Just r

eitherParseSigs :: BS.ByteString -> Either Text.Text [DBusType]
eitherParseSigs s = case AP.parseOnly (AP.many' signature) s of
    Left e -> Left $ Text.pack e
    Right r -> Right r

parseSigs :: BS.ByteString -> Maybe [DBusType]
parseSigs s = case eitherParseSigs s of
    Left _ -> Nothing
    Right r -> Just r
