{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Calc.Calc
import Calc.Compiler
import System.Console.CmdArgs

fib :: Num a => [a]
fib = 0 : 1 : zipWith (+) fib (tail fib)

fibProg :: Int -> Prog
fibProg n = Compute (fib !! n)

data Fib = Fib
     { n :: Int
     } deriving (Data, Typeable, Show)

defaultArgs
    = cmdArgsMode $ Fib
    { n = def &= name "n" &= help "The number in the sequence to compute"
    } &= summary "A fibonacci calculator using a Haskell eDSL"

main = do
     (Fib n) <- cmdArgsRun defaultArgs
     x <- run (fibProg n)
     putStrLn $ "fib " ++ show n ++ " = " ++ show x
