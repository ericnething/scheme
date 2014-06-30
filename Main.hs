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
-- Using liftM (or fmap):
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

-- | Convert a Float or Number to a Double
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

-- | Parse an expression
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
  concat ["(", unwordsList initL, " . ", showVal lastL, ")"]
showVal (Float n) = show n
showVal (Ratio n) = show (numerator n) ++ "/" ++ show (denominator n)
showVal (Complex n) = show (realPart n) ++ "+" ++ show (imagPart n) ++ "i"
showVal (Char c) = '#':'\\':c:[]

-- | helper for showVal
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | Evaluate an expression
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
primitives = [
             -- Arithmetic Operators
               ("+",         numericBinop (+))
             , ("-",         numericBinop (-))
             , ("*",         numericBinop (*))
             , ("/",         numericBinop div)
             , ("mod",       numericBinop mod)
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem)
               
             -- Boolean Operators
             , ("=",         numBoolBinop (==))
             , ("<",         numBoolBinop (<))
             , (">",         numBoolBinop (>))
             , ("/=",        numBoolBinop (/=))
             , (">=",        numBoolBinop (>=))
             , ("<=",        numBoolBinop (<=))
             , ("&&",        boolBoolBinop (&&))
             , ("||",        boolBoolBinop (||))

             -- String Operators
             , ("string=?",  strBoolBinop (==))
             , ("string<?",  strBoolBinop (<))
             , ("string>?",  strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))

             -- List Operators
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             ]

-- | Apply a boolean binary operator
boolBinop :: (LispVal -> ThrowsError a) -> 
             (a -> a -> Bool) -> 
             [LispVal] -> ThrowsError LispVal
boolBinop unpacker op xs
  | length xs /= 2 = throwError $ NumArgs 2 xs
  | otherwise = do
    left <- unpacker $ head xs
    right <- unpacker $ last xs
    return $ Bool $ left `op` right

-- | boolBinop for Number
numBoolBinop = boolBinop unpackNum

-- | boolBinop for String
strBoolBinop = boolBinop unpackStr

-- | boolBinop for Bool
boolBoolBinop = boolBinop unpackBool

-- | Apply an arithmetic binary operator
numericBinop :: (Integer -> Integer -> Integer) -> 
                [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op x@[_] = throwError $ NumArgs 2 x
numericBinop op xs = mapM unpackNum xs >>= return . Number . foldl1 op

-- | Unpack a Number from a LispVal into an Integer
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- | Unpack a String from a LispVal into a String
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- | Unpack a Bool from a LispVal into a Bool
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- List Primitives

-- | `car` produces the first element of the list
-- 
-- for a List:
-- > (car '(a b c)) = a
-- > (car '(a)) = a
--
-- for a DottedList:
-- > (car '(a b . c)) = a
--
-- Error checking:
-- > (car 'a) = error (not a list)
-- > (car 'a 'b) = error (`car` takes only 1 argument)

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


-- | `cdr` produces the rest of the list, excluding the first element
--
-- for a List:
-- > (cdr '(a b c)) = (b c)
-- > (cdr '(a b)) = (b)
-- > (cdr '(a)) = Nil
--
-- for a DottedList:
-- > (cdr '(a . b)) = b
-- > (cdr '(a b . c)) = (b . c)
--
-- Error checking:
-- > (cdr 'a) = error (not a list)
-- > (cdr 'a 'b) = error (`cdr` takes only 1 argument)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


-- | `cons` places an element at the front of a list
--
-- for an element and an empty List:
-- > (cons 'a 'Nil) = (a)
--
-- for an element and a non-empty List:
-- > (cons 'a '(b c)) = (a b c)
--
-- for an element and a DottedList:
-- > (cons 'a '(b . c)) = (a b . c)
--
-- for two elements:
-- > (cons 'a 'b) = (a . b)
-- > (cons '(a b) 'c) = (a b . c)
--
-- Error checking:
-- > (cons 'a) = error (`cons` takes only 2 arguments)
-- > (cons 'a '(b c) '(d e)) = error (`cons` takes only 2 arguments)

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]         = return $ List [x]
cons [x, List xs]         = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y]               = return $ DottedList [x] y
cons badArgList           = throwError $ NumArgs 2 badArgList

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

type ThrowsError = Either LispError

-- | show Error
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

-- | trap Errors and `show` them
trapError action = catchError action (return . show)

-- | Extract the value from an Error
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

