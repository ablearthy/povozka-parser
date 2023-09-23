module MyLib (someFunc, parseTL, runAlex) where

import Povozka.Parser (parseTL)
import Povozka.Lexer (runAlex)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
