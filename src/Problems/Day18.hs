module Problems.Day18 (solution) where

import Control.Applicative ((<|>))

import Common.Solution (Day, notImplemented)
import Common.Parse (Parser, parse, read1_, readInteger)

data Number
    = Literal Integer
    | Pair Number Number
    deriving (Eq)

instance Show Number where
    show (Literal x) = show x
    show (Pair l r) = "[" ++ (show l) ++ "," ++ (show r) ++ "]"

(<+>) :: Number -> Number -> Number
(<+>) l r = reduce (Pair l r)

explode :: Number -> Number
explode = fst . go 0
    where
        go :: Integer -> Number -> (Number, Maybe (Integer, Integer))
        go 4 (Pair (Literal l) (Literal r)) = (Literal 0, Just (l, r))
        go 4 (Pair _ _) = error "Impossible state!"
        go _ n@(Literal _) = (n, Nothing)
        go d (Pair l r)
            | (Just (dl, dr)) <- dl' = ((Pair nl (addL dr $ r)), Just (dl, 0))
            | (Just (dl, dr)) <- dr' = ((Pair (addR dl $ l) nr), Just (0, dr))
            | otherwise = ((Pair nl nr), Nothing)
            where
                (nl, dl') = go (d + 1) l
                (nr, dr') = go (d + 1) r

        addL, addR :: Integer -> Number -> Number
        addL 0 n = n
        addL n (Literal m) = Literal (n + m)
        addL n (Pair lhs rhs) = Pair (addL n lhs) rhs
        addR 0 n = n
        addR n (Literal m) = Literal (n + m)
        addR n (Pair lhs rhs) = Pair lhs (addR n rhs)

split :: Number -> Number
split (Literal x)
    | x >= 10 = Pair (Literal (x `div` 2)) (Literal ((x `div` 2) + (x `mod` 2)))
    | otherwise = Literal x
split (Pair l r)
    | nl /= l = Pair nl r
    | otherwise = Pair l nr
    where
        nl = split l
        nr = split r

reduce :: Number -> Number
reduce n =
    if postExplode == n
    then (if postSplit == n
        then n
        else (reduce postSplit))
    else (reduce postExplode)
    where
        postExplode = explode n
        postSplit = split n

magnitude :: Number -> Integer
magnitude (Literal n) = n
magnitude (Pair l r) = 3 * (magnitude l) + 2 * (magnitude r)

parseNumber :: Parser Char Number
parseNumber = (fmap Literal readInteger) <|> parsePair
    where
        parsePair = do
            read1_ '['
            lhs <- parseNumber
            read1_ ','
            rhs <- parseNumber
            read1_ ']'
            return (Pair lhs rhs)

solution :: Day
solution = (
        show . magnitude . foldl1 (<+>) . map (parse parseNumber) . lines,
        show . maximum . map (magnitude . uncurry (<+>)) . (\x -> [(p1, p2) | p1 <- x, p2 <- x, p1 /= p2]) . map (parse parseNumber) . lines
    )
