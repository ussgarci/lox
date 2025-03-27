{-# LANGUAGE ImportQualifiedPost #-}

module MegaScanner (
  scan
)
where

import Data.Void (Void)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Map (Map)
import Data.Map qualified as M
import Token 


data MegaToken = MegaToken 
  { _type :: TokenType
   ,_lexeme :: String 
  --, _literal :: Maybe Literal
   ,_location :: M.SourcePos
  }
  deriving (Show, Eq)

type Parser = M.Parsec Void String

singleCharTokens :: [(Char, TokenType)]
singleCharTokens = 
  [('<', LESS)
  ,('/', SLASH)
  ,('>', GREATER)
  ,('=', EQUAL)
  ,('!', BANG)
  ,('(', LEFT_PAREN)
  ,(')', RIGHT_PAREN)
  ,('{', LEFT_BRACE)
  ,('}', RIGHT_BRACE)
  ,(',', COMMA)
  ,('.', DOT)
  ,('-', MINUS)
  ,('+', PLUS)
  ,(';', SEMICOLON)
  ,('*', STAR)
  ]

keywords :: [(String, TokenType)]
keywords =
  [ ("and", AND)
  , ("class", CLASS)
  , ("else", ELSE)
  , ("false", FALSE)
  , ("for", FOR)
  , ("fun", FUN)
  , ("if", IF)
  , ("nil", NIL)
  , ("or", OR)
  , ("print", PRINT)
  , ("return", RETURN)
  , ("super", SUPER)
  , ("this", THIS)
  , ("true", TRUE)
  , ("var", VAR)
  , ("while", WHILE)
  ]

singleCharTokenMap :: Map Char TokenType
singleCharTokenMap = M.fromList singleCharTokens
  
keywordsMap :: Map String TokenType
keywordsMap = M.fromList keywords

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

stringLiteral :: Parser MegaToken 
stringLiteral = do 
  sc
  -- Why didn't this work?
  --xs <- M.between (char '\"') (char '\"') (M.many L.charLiteral)
  xs <- char '\"' *> M.manyTill L.charLiteral (char '\"')
  sc
  MegaToken STRING xs <$> M.getSourcePos

numberLiteral :: Parser MegaToken
numberLiteral = do
  sc
  -- One or more 
  xs <- M.some digitChar
  sc
  MegaToken NUMBER xs <$> M.getSourcePos

singleCharToken :: Parser MegaToken
singleCharToken = do 
  sc
  txt <- M.oneOf $ map fst singleCharTokens
  sc
  MegaToken (singleCharTokenMap M.! txt) [txt] <$> M.getSourcePos

reservedWord :: Parser MegaToken
reservedWord = do
  sc
  xs <- M.choice $ map (string .fst) keywords
  sc
  MegaToken (keywordsMap M.! xs) xs <$> M.getSourcePos


identifier:: Parser MegaToken 
identifier = do 
  sc
  x <- letterChar
  xs <- M.many alphaNumChar
  sc
  MegaToken IDENTIFIER (x:xs) <$> M.getSourcePos

scanMegaTokens :: Parser [MegaToken]
scanMegaTokens = do 
  M.many $ M.choice 
    [ singleCharToken
    , reservedWord
    , numberLiteral
    , stringLiteral
    , identifier
    ]

-- https://hackage.haskell.org/package/megaparsec-9.6.1/docs/Text-Megaparsec.html#v:runParser
-- runParser
--   :: Parsec e s a -- ^ Parser to run
--   -> String     -- ^ Name of source file
--   -> s          -- ^ Input for parser
--   -> Either (ParseErrorBundle s e) ascan :: String -> IO [MegaToken]
scan xs = do
  case M.runParser scanMegaTokens "" xs of
    Left bundle -> error (show bundle)
    Right tokens -> return tokens

-- reservedWord' :: Parser MegaToken
-- reservedWord' = do
--   sc
--   xs <- M.oneOf $ map fst keywords
--   sc
--   MegaToken (keywordsMap M.! xs) xs <$> M.getSourcePos

-- what was with this error message?!!
-- run' :: String -> IO [MegaToken]
-- run' xs = do
--   case M.parse scanMegaTokens xs of
--     Left bundle -> error (show bundle)
--     Right tokens -> return tokens




     

