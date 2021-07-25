module LearnParser

import Text.Parser
import Text.Lexer
import Text.Token
--import Data.Bool.Extra
import Data.List as L
import System.File

public export
record EitherT (leftTy : Type) (m : Type -> Type) (a : Type) where
  constructor ET
  runEitherT : m (Either lefTy a)
  
public export
Functor m => Functor (EitherT l m) where
  map f (ET re) = ET (map (\ev =>
    case ev of
      Left lv => Left lv
      Right rv => Right $ f rv) re)
      
-- public export
-- implementation Monad m => Monad (EitherT 


Show k => Show (Token k) where
  showPrec _ x = "Token " ++ show (kind x) ++ " " ++ (text x)

--pytorch yaml file
data PtyKind = PKComment | PKDash | PKIdent | PKOpenParen | PKCloseParen | PKComma | PKEq | PKExclamation | PKArrow | PKQuestion | PKOpenSqrBk | PKCloseSqrBk | PKStar | PKInt | PKWhitespace

showit : PtyKind -> String
showit PKComment = "PKComment"
showit PKDash = "PKDash"
showit PKIdent = "PKIdent" 
showit PKOpenParen = "PKOpenParen"
showit PKCloseParen = "PKCloseParen"
showit PKComma = "PKComma"
showit PKEq = "PKEq"
showit PKExclamation = "PKExclamation"
showit PKArrow = "PKArrow"
showit PKQuestion = "PKQuestion"
showit PKOpenSqrBk = "PKOpenSqrBk"
showit PKCloseSqrBk = "PKCloseSqrBk"
showit PKStar = "PKStar"
showit PKInt = "PKInt"
showit PKWhitespace = "PKWhitespace"


Show PtyKind where
  showPrec _ x = showit x



TokenKind PtyKind where
  TokType PKComment = ()
  TokType PKDash = ()
  TokType PKIdent = String
  TokType PKInt = Int
  TokType PKOpenParen = ()
  TokType PKCloseParen = ()
  TokType PKComma = ()
  TokType PKEq = ()
  TokType PKExclamation = ()
  TokType PKArrow = ()
  TokType PKQuestion = ()
  TokType PKOpenSqrBk = ()
  TokType PKCloseSqrBk = ()
  TokType PKStar = ()
  TokType PKWhitespace = ()

  tokValue PKComment x = ()
  tokValue PKDash x = ()
  tokValue PKIdent x = x
  tokValue PKInt x = cast x
  tokValue PKOpenParen x = ()
  tokValue PKCloseParen x = ()
  tokValue PKComma x = ()
  tokValue PKEq x = ()
  tokValue PKExclamation x = ()
  tokValue PKArrow x = ()
  tokValue PKQuestion x = ()
  tokValue PKOpenSqrBk x = ()
  tokValue PKCloseSqrBk x = ()
  tokValue PKStar x = ()
  tokValue PKWhitespace x = ()

swap : (a,b) -> (b,a)
swap (x, y) = (y,x)

isAlphaUndScr : Lexer
isAlphaUndScr = pred (\x => isAlpha x || x == '_')

isAlphaNumUndScr : Lexer
isAlphaNumUndScr = pred (\x => isAlphaNum x || x == '_')

myIdent : Lexer
myIdent = opt isAlphaUndScr <+> some isAlphaNumUndScr


myMap : TokenMap (Token PtyKind)
myMap = toTokenMap (map swap m)
  where
    m : List (PtyKind, Lexer)
    m = [
          (PKComment, lineComment (is '#'))
          ,(PKWhitespace, spaces)
          ,(PKArrow, exact "->")
          ,(PKDash, is '-')
          ,(PKIdent, myIdent)
          ,(PKInt, digits) --TODO negatives
          ,(PKOpenParen, is '(')
          ,(PKCloseParen, is ')')
          ,(PKEq, is '=')
          ,(PKExclamation, is '!')
          ,(PKQuestion, is '?')
          ,(PKOpenSqrBk, is '[')
          ,(PKCloseSqrBk, is ']')
          ,(PKStar, is '*')
        ]
    
main : IO ()
main =
  do
    mf <- readFile "n.yaml"
    putStrLn "hello"
    (case mf of
      Left e => 
        do 
          putStrLn $ "Error: " ++ show e
          pure ()
      Right s => 
        do
          let r = lex myMap s
          let r2 = fst r
          let r3 = map tok r2
          putStrLn $ show r
          pure ())
       
