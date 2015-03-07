main = do 
    putStrLn "Hi!"
    input <- getLine
    case input of 
        "what" -> putStrLn "no way"
        _      -> putStrLn "zazazaza"


changeParameters :: (String,String) -> [Double] -> [Double] 
changeParameters (newParameter, newParameterValue) parameters = b ++ (d : tail c)
    where (b,c) = splitAt a parameters
          a     = (read newParameter :: Int) - 1
          d     = read newParameterValue :: Double
