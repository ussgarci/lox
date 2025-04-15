{-# LANGUAGE ImportQualifiedPost #-}

module MegaScanner (
    scan,
)
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Token

sourcePosToTokenPos :: M.SourcePos -> TokenPos
sourcePosToTokenPos pos =
    TokenPos
        { _name = M.sourceName pos
        , _line = M.unPos (M.sourceLine pos)
        , _column = M.unPos (M.sourceColumn pos)
        }

type Parser = M.Parsec Void String

singleCharTokens :: [(Char, TokenType)]
singleCharTokens =
    [ ('<', LESS)
    , ('/', SLASH)
    , ('>', GREATER)
    , ('=', EQUAL)
    , ('!', BANG)
    , ('(', LEFT_PAREN)
    , (')', RIGHT_PAREN)
    , ('{', LEFT_BRACE)
    , ('}', RIGHT_BRACE)
    , (',', COMMA)
    , ('.', DOT)
    , ('-', MINUS)
    , ('+', PLUS)
    , (';', SEMICOLON)
    , ('*', STAR)
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
sc =
    L.space
        space1 -- (2)
        (L.skipLineComment "//") -- (3)
        (L.skipBlockComment "/*" "*/") -- (4)

stringLiteral :: Parser Token
stringLiteral = do
    sc
    -- Why didn't this work?
    -- xs <- M.between (char '\"') (char '\"') (M.many L.charLiteral)
    xs <- char '\"' *> M.manyTill L.charLiteral (char '\"')
    sc
    Token STRING xs (Just $ StringLiteral xs) . sourcePosToTokenPos <$> M.getSourcePos

numberLiteral :: Parser Token
numberLiteral = do
    sc
    -- One or more
    xs <- M.some digitChar
    sc
    Token NUMBER xs (Just $ NumberLiteral (read xs)) . sourcePosToTokenPos <$> M.getSourcePos

singleCharToken :: Parser Token
singleCharToken = do
    sc
    txt <- M.oneOf $ map fst singleCharTokens
    sc
    Token (singleCharTokenMap M.! txt) [txt] Nothing . sourcePosToTokenPos <$> M.getSourcePos

reservedWord :: Parser Token
reservedWord = do
    sc
    xs <- M.choice $ map (string . fst) keywords
    sc
    Token (keywordsMap M.! xs) xs Nothing . sourcePosToTokenPos <$> M.getSourcePos

identifier :: Parser Token
identifier = do
    sc
    x <- letterChar
    xs <- M.many alphaNumChar
    sc
    Token IDENTIFIER (x : xs) (Just $ IdentifierLiteral xs) . sourcePosToTokenPos <$> M.getSourcePos

scanMegaTokens :: Parser [Token]
scanMegaTokens = do
    M.many $
        M.choice
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
scan :: (Monad m) => String -> m [Token]
scan xs = do
    case M.runParser scanMegaTokens "" xs of
        Left bundle -> error (show bundle)
        Right tokens -> return tokens
