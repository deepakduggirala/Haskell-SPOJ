main =
  do
    x <- getLine
    if x == "42" then return ()
    else
      do
        putStr (x++"\n")
        main