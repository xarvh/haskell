
import qualified Data.Char as C



-- let's use a string: in principle, an operator could be more than two chars
operators :: [([Char], Double -> Double -> Double)]
operators = [ ("+", (+)), ("-", (-)) ]


opEq :: String -> (String, a) -> Bool
opEq string op = ((fst op) == string)


stringToOperator :: String -> Double -> Double -> Double
stringToOperator string = snd (head (filter (opEq string) operators))


isOperator :: Char -> Bool
isOperator = (`elem` (concatMap fst operators))


-- evaluates a simple expression "23 - 5" into a double
calc :: String -> Double
calc string =
  let
    trimmedString = filter (not . C.isSpace) string
    (leftOperand, remainder) = break isOperator trimmedString
    (operator, rightOperand) = break (not . isOperator) remainder
    op = stringToOperator operator
    le = read leftOperand :: Double
    ri = read rightOperand :: Double
  in op le ri
