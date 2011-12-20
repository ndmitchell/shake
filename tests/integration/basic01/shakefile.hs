import Development.Shake

main = shake shakeOptions $ do
  want ["AB.txt"]
  "AB.txt" *> \out -> do
    need ["A.txt", "B.txt"]
    text1 <- readFile' "A.txt"
    text2 <- readFile' "B.txt"
    writeFile' out (text1 ++ text2)
