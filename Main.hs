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
import Data.IORef

import Numeric
import System.Environment
import System.IO

import Debug.Trace

-- Debug
-------------------------------------------------------------------------------

-- | `help` for debugging
help = flip trace

-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl
    else runOne $ args

-- | Read an expression (used in REPL)
readExpr = readOrThrow parseExpr

-- | Read an expression (used to load files)
readExprList = readOrThrow (endBy parseExpr spaces)

-- | helper for reading expressions
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

-- Parsers
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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
                    -- names of parameters in the function body
                      params :: [String]
                    -- name of variable-length list of arguments, if it exists
                    , vararg :: (Maybe String)
                    -- body of function
                    , body :: [LispVal]
                    -- environment local to function
                    , closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

-- | Parser for Scheme identifiers
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- | Parser to skip whitespace characters
spaces :: Parser ()
spaces = skipMany1 space

-- | Parser for Strings
parseString :: Parser LispVal
parseString = do
  char '\"'
  x <- many (escaped <|> noneOf "\"")
  char '\"'
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

-- Evaluation
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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  concat [ "(lambda ("
         , unwords (map show args)
         , (case varargs of Nothing -> ""
                            Just arg -> " . " ++ arg)
         ,") ...)" ]
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

-- | helper for showVal
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- | Evaluate an expression
eval :: Env -> LispVal -> IOThrowsError LispVal

-- literal value
eval env val@(String _)  = return val
eval env val@(Char _)    = return val
eval env val@(Number _)  = return val
eval env val@(Float _)   = return val
eval env val@(Ratio _)   = return val
eval env val@(Complex _) = return val
eval env val@(Bool _)    = return val

-- get variable
eval env (Atom var) = getVar env var

-- quoted value
eval env (List [Atom "quote", val]) = return val

-- conditional
eval env (List [Atom "if", pred, conseq, alt]) = 
  do result <- eval env pred
     case result of
          Bool False -> eval env alt
          otherwise -> eval env conseq

-- set variable
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var

-- define variable
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

-- define function
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" :
                DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body

-- load
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

-- function application
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

-- general List and DottedList
-- eval env val@(DottedList _ _) = return val
-- eval env val@(List _)         = return val

-- Error
eval env badForm = throwError (BadSpecialForm
                               "Unrecognized special form" badForm)

-- | Function Application
-- 
-- Given PrimitiveFunc:
--     Extract function and apply it to the arguments.
-- 
-- Given Func:
--     If the number of parameters is not equal to the number
-- of provided arguments and there is no variable-length
-- list of arguments, throw error (not enough arguments).
--     Otherwise, bind the arguments within the environment
-- and evaluate the body of the function.

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal

apply (PrimitiveFunc f) args = liftThrows $ f args

apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>=
          bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $
                          bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env

apply (IOFunc func) args = func args

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
               
             -- Numeric Boolean Operators
             , ("=",         numBoolBinop (==))
             , ("<",         numBoolBinop (<))
             , (">",         numBoolBinop (>))
             , ("/=",        numBoolBinop (/=))
             , (">=",        numBoolBinop (>=))
             , ("<=",        numBoolBinop (<=))

             -- Boolean Boolean Operators
             , ("&&",        boolBoolBinop (&&))
             , ("||",        boolBoolBinop (||))

             -- Equivalence Boolean Operators
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", eqv)

             -- String Boolean Operators
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

-- | Test for equivalence
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)]     = return $ Bool $ x == y

-- If the List equivalents of the DottedLists are equal,
-- then they are equal.
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x],
                                                  List $ ys ++ [y]]
-- If two Lists have equal length and all of their elements
-- are equal, then the two Lists are equal.
eqv [(List x), (List y)]     = return $ Bool $ (length x == length y) &&
                               (all eqvPair $ zip x y)
  where eqvPair (x, y) = case eqv [x, y] of
          Left err -> False
          Right (Bool val) -> val

eqv [_, _]                   = return $ Bool False
eqv badArgList               = throwError $ NumArgs 2 badArgList

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

-- Error Handling
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

-- REPL
-------------------------------------------------------------------------------

-- | Print the string and flush the stream
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Read from the prompt
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Evaluate the expression and catch any errors
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
                      (liftThrows $ readExpr expr) >>= eval env

-- | Evaluate the string and print the result
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- | This is an infinite loop that continuously reads from the prompt
-- and executes the input until the predicate applied to the input
-- is True.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

-- | Run the REPL
runRepl :: IO ()
runRepl = primitiveBindings >>=
          until_ (== "quit") (readPrompt "scheme> ") . evalAndPrint

-- | Run a program
-- 
-- Passes the primitive bindings to `bindVars` and adds a variable
-- named `args` bound to a list of string versions of everything
-- excpet the first argument, which is the name of the file to run.
-- Then creates `(load filename)` in Scheme format and evaluates it.

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars
         [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env
   (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

-- Env is a map of Strings to LispVals
type Env = IORef [(String, IORef LispVal)]

-- | Empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Wrap IO in Error
type IOThrowsError = ErrorT LispError IO

-- | lift ThrowsError into IOThrowsError
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- | convert error values into strings using `trapError`, then run the
-- computation using `runErrorT` and pass the result to `extractValue`
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- | Check if the variable is already defined
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>=
                     return . maybe False (const True) . lookup var

-- | Get the value of a variable.
--
-- Note: Env = IORef [(String, IORef LispVal)]
-- so we must first unpack the outer IORef, then `lookup` the
-- variable (key), and finally unpack the inner IORef before lifting
-- it into IOThrowsError.

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unbound variable: " var)
    (liftIO . readIORef)
    (lookup var env)

-- | Set the value of a variable.
--
-- Unpack the outer IORef, pass it to `lookup`, then write the
-- value and lift it into IOThrowsError

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unbound variable: " var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

-- | Sets a variable if it is already bound or creates a new one
-- if it is unbound.

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
     then setVar envRef var value >> return value
     else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

-- | Bind multiple variables at once.
--
-- Unpack the outer IORef, create entries for the new variables, add
-- them to the front of the Env list, then wrap it in an IORef

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBindings bindings)
        addBindings (var, value) = do ref <- newIORef value
                                      return (var, ref)

-- | Bind the primitive functions
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
                    (flip bindVars $ map (make IOFunc) ioPrimitives
                     ++ map (make PrimitiveFunc) primitives)
  where make constructor (var, func) = (var, constructor func)

-- | Helper for function creation
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

-- | Helper for creating a function without a variable-length list
-- of arguments.
makeNormalFunc = makeFunc Nothing

-- | Helper for creating a function with a variable-length list
-- of arguments.
makeVarArgs = makeFunc . Just . showVal

-- File Input/Output
-------------------------------------------------------------------------------

-- | Destructure the argument list of the IO Primitives into a
-- form that `apply` accepts.
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

-- | IO Primitives
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply",             applyProc)
               , ("open-input-file",   makePort ReadMode)
               , ("open-output-file",  makePort WriteMode)
               , ("close-input-file",  closePort)
               , ("close-output-file", closePort)
               , ("read",              readProc)
               , ("write",             writeProc)
               , ("read-contents",     readContents)
               , ("read-all",          readAll)]

-- | Open a file
-- Note: wraps `openFile`
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- | Close a file
-- Note: wraps `hClose`
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- | Read a file
-- Note: wraps `hGetLine` and sends result to `parseExpr`
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

-- | Write to a file
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- | Read entire file into memory
-- Note: wraps `readFile`
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- | helper for readAll
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

-- | readAll
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

