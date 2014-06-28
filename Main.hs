module Main where

import Text.ParserCombinators.Parsec hiding (spaces)

import Control.Monad
import Control.Monad.Error

import Data.Char
import Data.Maybe
import Data.List
import Data.Ratio
import Data.Complex
import qualified Data.Foldable as F

import Numeric
import System.Environment

import Debug.Trace

-- | `help` for debugging
help = flip trace

main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled

-- Read an expression
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- Parsers.hs 
-------------------------------------------------------------------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Char Char
             | String String
             | Bool Bool

-- | Parser for Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- | Parser to skip whitespace characters
spaces :: Parser ()
spaces = skipMany1 space

-- | Parser for Strings
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escaped <|> noneOf "\"")
  char '"'
  return (String x)
    where escaped = char '\\' >> choice (map convert codes)
          convert (c,x) = char c >> return x
          codes = [('\\', '\\')
                  ,('\"', '\"')
                  ,('/', '/')
                  ,('b', '\b')
                  ,('n', '\n')
                  ,('f', '\f')
                  ,('r', '\r')
                  ,('t', '\t')]

-- | Parser for Atoms
parseAtom :: Parser LispVal
parseAtom = do
  first <- (letter <|> symbol)
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ Atom atom

-- | Parser for Bool
parseBool :: Parser LispVal
parseBool = char '#' >>
            F.asum [ char 't' >> return (Bool True)
                   , char 'f' >> return (Bool False) ]

-- | Parser for Numbers
-- 
-- Prefix notation:
-- #o -> octal
-- #x -> hexadecimal
-- #d -> decimal
-- #b -> binary
-- Nothing -> decimal
--
--  Using liftM (or fmap):
-- > parseNumber = liftM (Number . read) $ many1 digit
--
-- Using monadic bind (>>=):
-- > parseNumber = many1 digit >>= return . Number . read
--
-- Using do-notation:
-- > parseNumber = do
-- >   x <- many1 digit
-- >   return $ Number $ read x

parseNumber :: Parser LispVal
parseNumber = liftM Number (
  F.asum [ char '#' >> 
           F.asum [ char 'd' >> liftNum readDec digit
                  , char 'x' >> liftNum readHex hexDigit
                  , char 'o' >> liftNum readOct octDigit
                  , char 'b' >> liftNum readBin (oneOf "01") ] 
         , liftM read $ many1 digit ])
  where convert = (fst . fromJust . listToMaybe)
        readBin = readInt 2 (`elem` "01") (fromJust . (`elemIndex` "01"))
        liftNum f t = liftM (convert . f) $ many1 t

-- parseNumber :: Parser LispVal
-- parseNumber = do
  -- number <- many1 (oneOf "ox#abcdef" <|> digit)
  -- return $ case number of
  --   '#':rest -> case map toLower rest of
  --     'o':num -> convert (readOct num)
  --     'x':num -> convert (readHex num)
  --     'd':num -> convert (readDec num)
  --     'b':num -> convert (readBin num)
  --     _ -> Atom number
  --   _ -> convert (readDec number)
  --   where convert = (Number . fst . fromMaybe (0,"") . listToMaybe)
  --         readBin = readInt 2 (`elem` "01") (fromJust . (`elemIndex` "01"))

-- | Parser for Floats
parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float (fst . fromJust . listToMaybe $ readFloat (concat [x,".",y]))

-- | Parser for Ratios
parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio ((read x) % (read y))

-- | Parser for Complex Numbers
parseComplex :: Parser LispVal
parseComplex = do
  x <- (try parseFloat <|> parseNumber)
  char '+'
  y <- (try parseFloat <|> parseNumber)
  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

-- | Parser for Characters
parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  x <- F.asum [try (F.asum [ string "space", string "newline" ]) 
              , anyChar >>= (\x -> return [x]) ]
  return $ Char $ case x of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head x

-- | Parser for Lists
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- | Parser for DottedLists
parseDottedList :: Parser LispVal
parseDottedList = do
  initL <- endBy parseExpr spaces
  tailL <- char '.' >> spaces >> parseExpr
  return $ DottedList initL tailL

-- | Parser for single quote
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Parser for quasi quote
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- | Parser for unquote
parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- Parse an expression
parseExpr :: Parser LispVal
parseExpr = F.asum [ parseAtom
                   , parseString
                   , try parseComplex
                   , try parseFloat
                   , try parseRatio
                   , try parseNumber
                   , try parseBool
                   , try parseChar
                   , parseQuoted
                   , parseQuasiQuoted
                   , parseUnQuote
                   , do char '('
                        x <- try parseList <|> parseDottedList
                        char ')'
                        return x
                   ]

-- Eval.hs
-------------------------------------------------------------------------------

instance Show LispVal where show = showVal

-- | show LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList initL lastL) = 
  concat ["(", unwordsList initL, ".", showVal lastL, ")"]
showVal (Float n) = show n
showVal (Ratio n) = show (numerator n) ++ "/" ++ show (denominator n)
showVal (Complex n) = show (realPart n) ++ "+" ++ show (imagPart n) ++ "i"
showVal (Char c) = '#':'\\':c:[]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | Evaluator
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)  = return val
eval val@(Char _)    = return val
eval val@(Number _)  = return val
eval val@(Float _)   = return val
eval val@(Ratio _)   = return val
eval val@(Complex _) = return val
eval val@(Bool _)    = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = 
  do result <- eval pred
     case result of
          Bool False -> eval alt
          otherwise -> eval conseq
eval (List (Atom f : args))     = mapM eval args >>= apply f
eval val@(DottedList _ _)       = return val
eval val@(List _)               = return val
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

-- | Function Application
apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $
                      NotFunction "Unrecognized primitive function args" f)
               ($ args)
               (lookup f primitives)

-- | Primitives
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+",         numericBinop (+))
             , ("-",         numericBinop (-))
             , ("*",         numericBinop (*))
             , ("/",         numericBinop div)
             , ("mod",       numericBinop mod)
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("=",         numBoolBinop (==))
             , ("<",         numBoolBinop (<))
             , (">",         numBoolBinop (>))
             , ("/=",        numBoolBinop (/=))
             , (">=",        numBoolBinop (>=))
             , ("<=",        numBoolBinop (<=))
             , ("&&",        boolBoolBinop (&&))
             , ("||",        boolBoolBinop (||))
             , ("string=?",  strBoolBinop (==))
             , ("string<?",  strBoolBinop (<))
             , ("string>?",  strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             ]

boolBinop :: (LispVal -> ThrowsError a) -> 
             (a -> a -> Bool) -> 
             [LispVal] -> ThrowsError LispVal
boolBinop unpacker op xs
  | length xs /= 2 = throwError $ NumArgs 2 xs
  | otherwise = do
    left <- unpacker $ head xs
    right <- unpacker $ last xs
    return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> 
                [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op x@[_] = throwError $ NumArgs 2 x
numericBinop op xs = mapM unpackNum xs >>= return . Number . foldl1 op
-- numericBinop op xs = Number $ foldl1 op $ map unpackNum xs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Error.hs
-------------------------------------------------------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar msg var) = concat [msg, ": ", var]
showError (BadSpecialForm msg form) = concat [msg, ": ", show form]
showError (NotFunction msg func) = concat [msg, ": ", show func]
showError (NumArgs expected found) = 
  concat ["Expected: ", show expected, " args\n",
          "Found values: ", unwordsList found]
showError (TypeMismatch expected found) = 
  concat ["Invalid type: expected ", expected, ", found ", show found]
showError (Parser parseErr) = concat ["Parse error at ", show parseErr]

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

