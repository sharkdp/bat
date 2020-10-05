{-# LANGUAGE OverloadedStrings #-}

-- simple parser for a Lisp-like syntax I wrote some time ago

import Data.Void (Void)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec.Char
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec hiding (State)
import qualified Text.Megaparsec.Char.Lexer as L

data LispVal
  = Symbol Text
  | List [LispVal]
  | Number Integer
  | String Text
  | LispTrue
  | LispFalse
  | Nil
  deriving (Show, Eq)

type Parser = Parsec Void Text

readStr :: Text -> Either String [LispVal]
readStr t =
  case parse pLisp "f" t of
    Right parsed -> Right parsed
    Left err -> Left $ errorBundlePretty err
{-# INLINABLE readStr #-}

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty
{-# INLINABLE sc #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

symbol :: Text -> Parser Text
symbol = L.symbol sc
{-# INLINE symbol #-}

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc
{-# INLINE symbol' #-}

pNil :: Parser LispVal
pNil = symbol' "nil" >> return Nil
{-# INLINE pNil #-}

integer :: Parser Integer
integer = lexeme L.decimal
{-# INLINE integer #-}

lispSymbols :: Parser Char
lispSymbols = oneOf ("#$%&|*+-/:<=>?@^_~" :: String)
{-# INLINE lispSymbols #-}

pLispVal :: Parser LispVal
pLispVal = choice [pList, pNumber, pSymbol, pNil, pString]
{-# INLINE pLispVal #-}

pSymbol :: Parser LispVal
pSymbol = (Symbol . T.pack <$> lexeme (some (letterChar <|> lispSymbols)))
{-# INLINABLE pSymbol #-}

pList :: Parser LispVal
pList = List <$> between (symbol "(") (symbol ")") (many pLispVal)
{-# INLINABLE pList #-}

pLisp :: Parser [LispVal]
pLisp = some pLispVal
{-# INLINE pLisp #-}

pNumber :: Parser LispVal
pNumber = Number <$> integer
{-# INLINE pNumber #-}

pString :: Parser LispVal
pString = do
  str <- char '\"' *> manyTill L.charLiteral (char '\"')
  return $ String (T.pack str)
{-# INLINABLE pString #-}
