main = interact f
 where
  f :: String -> String
  f = unlines . takeWhile (/="42") . words