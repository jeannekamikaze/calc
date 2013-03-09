module Calc.Compiler
(
    run
,   dump
,   compile
)
where

import Calc.Calc
import Data.List (intercalate)
import System.Cmd (system)
import System.Exit (ExitCode(..))

newtype Prog = Compute Expr deriving Show

fib :: Num a => [a]
fib = 0 : 1 : zipWith (+) fib (tail fib)

run :: Prog -> IO Int
run prog =
    let code = compile prog
    in nasm code >>= system >>= return . readExit

nasm :: String -> IO String
nasm code
     = do writeFile "foo.s" code
          system "nasm -f elf foo.s"
          system "ld -o foo foo.o"
          return "./foo"

readExit :: ExitCode -> Int
readExit ExitSuccess = 0
readExit (ExitFailure x) = x

dump :: FilePath -> Prog -> IO ()
dump file prog = writeFile file $ compile prog

compile :: Prog -> String
compile (Compute e)
        = nconcat
        [ header
        , add
        , sub
        , mul
        , exit
        , "_start:"
        , compile' e
        , "push eax"
        , "call exit"
        ]

compile' :: Expr -> String
compile' (Lit x) = "mov eax, " ++ show x
compile' (Add x y) = binOp "add" x y
compile' (Sub x y) = binOp "sub" x y
compile' (Mul x y) = binOp "mul" x y

type Op = String

binOp :: Op -> Expr -> Expr -> String
binOp op x y
    = nconcat
    [ compile' x
    , "push eax"
    , compile' y
    , "push eax"
    , "call " ++ op
    , "add esp, 8"
    ]

add = nconcat
    [ "add:"
    , "mov eax, [esp+4]"
    , "mov ebx, [esp+8]"
    , "add eax, ebx"
    , "ret"
    , ""
    ]

sub = nconcat
    [ "sub:"
    , "mov ebx, [esp+4]"
    , "mov eax, [esp+8]"
    , "sub eax, ebx"
    , "ret"
    , ""
    ]

mul = nconcat
    [ "mul:"
    , "mov eax, [esp+4]"
    , "mov ebx, [esp+8]"
    , "mul ebx"
    , "ret"
    , ""
    ]

exit
    = nconcat
    [ "exit:"
    , "mov ebx, [esp+4]"
    , "mov eax, 1"
    , "int 0x80"
    , ""
    ]

header
    = nconcat
    [ "BITS 32"
    , "section .text"
    , "global _start"
    , ""
    ]

nconcat = intercalate "\n"
