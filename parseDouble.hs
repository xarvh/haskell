
import Data.Char as C
import Data.List as L


localeDecimalSymbol = '.'


trim :: String -> String
trim = takeWhile (not . C.isSpace) . dropWhile C.isSpace



parseDouble :: String -> Either String Integer
parseDouble string =
  let (sign, s) = extractSign $ trim string
  in case extractDecimalPosition localeDecimalSymbol s of
    Left err -> Left err
    Right (decimalPosition, numbers) ->
      if any (not . C.isNumber) numbers then Left "spurious characters"
-- else Right (sign * (fromIntegral decimalPosition ** (-10)) * foldr processSingleNumber 0.0 numbers)
      else Right (sign * (foldr processSingleNumber 0 (reverse numbers)))

  where
    extractSign :: String -> (Integer, String)
    extractSign ('-':xs) = (-1, xs)
    extractSign ('+':xs) = (1, xs)
    extractSign s = (1, s)

    extractDecimalPosition :: Char -> String -> Either String (Int, String)
    extractDecimalPosition decimalSymbol s =
      let index = L.findIndices (== decimalSymbol) s
      in case length index of
        0 -> Right (length index, s)
        1 -> Right (head index, filter (/= decimalSymbol) s)
        _ -> Left "multiple decimal symbols"

    processSingleNumber :: Char -> Integer -> Integer
    processSingleNumber number total = (toInteger $ digitToInt number) + total * 10





{-
parseDouble :: String -> Maybe (Double, String)
parseDouble str = parseDouble' initialState str
   where
      -- Handle end of input
      parseDouble' (_, []) [] = Nothing
      parseDouble' (_, num) [] = Just $ (read num, [])

      -- First drop all the whitespace
      parseDouble' st@("whitespace", "") (' ':xs) = parseDouble' st xs
      parseDouble' ("whitespace", "") (x:xs) =
         if C.isNumber x then
            parseDouble' ("int", x:[]) xs
         else
            Nothing

      -- Then extract the integer part of the number
      parseDouble' ("int", num) ('.':xs) = parseDouble' ("frac", num ++ ".") xs
      parseDouble' ("int", num) r@(' ':xs) = Just $ (read num, r)
      parseDouble' ("int", num) (x:xs) =
         if C.isNumber x then
            parseDouble' ("int", num ++ [x]) xs
         else
            Nothing

      -- Then (optionally) handle the fractional part
      parseDouble' ("frac", num) r@(x:xs) =
         if C.isNumber x then
            parseDouble' ("frac", num ++ [x]) xs
         else
            Just $ (read num, r)

      initialState = ("whitespace", "")
-}

