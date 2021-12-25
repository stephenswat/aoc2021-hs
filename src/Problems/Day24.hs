module Problems.Day24 (solution) where

import Control.Monad (forM_)
import Control.Monad.State.Lazy (StateT, evalStateT, lift, get, modify)
import Control.Applicative (Alternative (many, (<|>)))
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, fromList, lookup, insert)
import Data.SBV (SBV, Symbolic, Goal, SInt64, optimize, minimize, OptimizeStyle (Lexicographic), maximize, sQuot, sRem, ite, (.==), exists, constrain, sAny)
import GHC.Int (Int64)
import System.IO.Unsafe (unsafePerformIO)

import Common.Solution (Day)
import Common.Parse (Parser, parse, readExact_, read1, readInteger)

data Operand
    = Register Char
    | Immediate Integer
    deriving (Show, Eq, Ord)

data Operator
    = Add
    | Mul
    | Div
    | Mod
    | Eql
    deriving (Show, Eq, Ord)

data Expr
    = Inp Operand Int
    | Op  Operator Operand Operand
    deriving (Show, Eq, Ord)

type Env = Map String SInt64

parseInput :: String -> [Expr]
parseInput = parse . evalStateT parseOperators $ 0
    where
        parseOperators :: StateT Int (Parser Char) [Expr]
        parseOperators = many $ do
            op <- parseOperator
            lift (readExact_ "\n")
            return op

        parseOperand :: Parser Char Operand
        parseOperand =
            (fmap Immediate readInteger) <|>
            (fmap Register read1)

        parseOperator :: StateT Int (Parser Char) Expr
        parseOperator =
            (do {lift (readExact_ "inp "); op1 <- lift parseOperand; v <- get; modify (+ 1); return (Inp op1 v)}) <|>
            (do {lift (readExact_ "add "); op1 <- lift parseOperand; lift (readExact_ " "); op2 <- (lift parseOperand); return (Op Add op1 op2)}) <|>
            (do {lift (readExact_ "mul "); op1 <- lift parseOperand; lift (readExact_ " "); op2 <- (lift parseOperand); return (Op Mul op1 op2)}) <|>
            (do {lift (readExact_ "div "); op1 <- lift parseOperand; lift (readExact_ " "); op2 <- (lift parseOperand); return (Op Div op1 op2)}) <|>
            (do {lift (readExact_ "mod "); op1 <- lift parseOperand; lift (readExact_ " "); op2 <- (lift parseOperand); return (Op Mod op1 op2)}) <|>
            (do {lift (readExact_ "eql "); op1 <- lift parseOperand; lift (readExact_ " "); op2 <- (lift parseOperand); return (Op Eql op1 op2)})

envLookup :: String -> Env -> SInt64
envLookup s e
    | isJust r = fromJust r
    | otherwise = error $ "Failed to look up " ++ s
    where
        r = Data.Map.lookup s $ e

getOp :: Operator -> SInt64 -> SInt64 -> SInt64
getOp Add = (+)
getOp Mul = (*)
getOp Div = sQuot
getOp Mod = sRem
getOp Eql = (\x y -> ite (x .== y) 1 0)

findEvalMap :: Env -> [Expr] -> Integer -> Env
findEvalMap e [] i
    = insert "w'" (envLookup ("w" ++ (show (i - 1))) e)
    . insert "x'" (envLookup ("x" ++ (show (i - 1))) e)
    . insert "y'" (envLookup ("y" ++ (show (i - 1))) e)
    . insert "z'" (envLookup ("z" ++ (show (i - 1))) e)
    $ e
findEvalMap e ((Inp (Register r) n):os) i
    = findEvalMap ne os (i + 1)
    where
        ne
            = insert (  r:(show i)) (envLookup ('i':(show n)) e)
            . insert ('w':(show i)) (envLookup ('w':(show (i - 1))) e)
            . insert ('x':(show i)) (envLookup ('x':(show (i - 1))) e)
            . insert ('y':(show i)) (envLookup ('y':(show (i - 1))) e)
            . insert ('z':(show i)) (envLookup ('z':(show (i - 1))) e)
            $ e
findEvalMap e ((Op o (Register r) (Immediate n)):os) i
    = findEvalMap ne os (i + 1)
    where
        ne
            = insert (  r:(show i)) ((getOp o) (envLookup (r:(show (i - 1))) e) (fromIntegral n))
            . insert ('w':(show i)) (envLookup ('w':(show (i - 1))) e)
            . insert ('x':(show i)) (envLookup ('x':(show (i - 1))) e)
            . insert ('y':(show i)) (envLookup ('y':(show (i - 1))) e)
            . insert ('z':(show i)) (envLookup ('z':(show (i - 1))) e)
            $ e
findEvalMap e ((Op o (Register r1) (Register r2)):os) i
    = findEvalMap ne os (i + 1)
    where
        ne
            = insert ( r1:(show i)) ((getOp o) (envLookup (r1:(show (i - 1))) e) (envLookup (r2:(show (i - 1))) e))
            . insert ('w':(show i)) (envLookup ('w':(show (i - 1))) e)
            . insert ('x':(show i)) (envLookup ('x':(show (i - 1))) e)
            . insert ('y':(show i)) (envLookup ('y':(show (i - 1))) e)
            . insert ('z':(show i)) (envLookup ('z':(show (i - 1))) e)
            $ e
findEvalMap _ _ _ = error "Invallid evaluation state!"

createEval :: (String -> SBV Int64 -> Symbolic ()) -> [Expr] -> Goal
createEval f os = do
    let vs = ["i" ++ (show x) | x <- ([0..13] :: [Int])]
    ss <- mapM exists vs
    let env' = fromList ((zip vs ss) ++ [("w0", 0), ("x0", 0), ("y0", 0), ("z0", 0)])
    let env = findEvalMap env' os 1

    forM_ ([0..13] :: [Int]) (\x -> constrain $ sAny (.== (envLookup ('i':(show x)) env)) [1,2,3,4,5,6,7,8,9])

    constrain $ ((envLookup "z'" env) .== 0)

    f "model" $ (
            (envLookup "i13" env) * 1 +
            (envLookup "i12" env) * 10 +
            (envLookup "i11" env) * 100 +
            (envLookup "i10" env) * 1000 +
            (envLookup  "i9" env) * 10000 +
            (envLookup  "i8" env) * 100000 +
            (envLookup  "i7" env) * 1000000 +
            (envLookup  "i6" env) * 10000000 +
            (envLookup  "i5" env) * 100000000 +
            (envLookup  "i4" env) * 1000000000 +
            (envLookup  "i3" env) * 10000000000 +
            (envLookup  "i2" env) * 100000000000 +
            (envLookup  "i1" env) * 1000000000000 +
            (envLookup  "i0" env) * 10000000000000
        )

solution :: Day
solution = (
        show . unsafePerformIO . optimize Lexicographic . createEval maximize . parseInput,
        show . unsafePerformIO . optimize Lexicographic . createEval minimize . parseInput
    )
