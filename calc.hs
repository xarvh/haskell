
import qualified Data.Char as C


operators = "+-*/"

stringToOperator :: String -> Double -> Double -> Double
stringToOperator "+" = (+)
stringToOperator "-" = (-)
stringToOperator "*" = (*)
stringToOperator "/" = (/)
stringToOperator s = error ("Syntax error at operator " ++ s)


isOperator :: Char -> Bool
isOperator = (`elem` operators)


-- evaluates a simple expression "23 - 5" into a double
calc :: String -> Double
calc string =
  let trimmedString = filter (not . C.isSpace) string
      (leftOperand, remainder) = break isOperator trimmedString
      (operator, rightOperand) = break (not . isOperator) remainder
      op = stringToOperator operator
      le = read leftOperand :: Double
      ri = read rightOperand :: Double
  in op le ri
