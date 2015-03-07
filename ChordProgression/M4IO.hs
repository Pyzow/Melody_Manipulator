--M4IO.hs
--Ivan Pyzow ipyzow@gmail.com

module Main where

import M4
--import System.IO (for openFile, and hClose)
import System.Environment (getArgs)
import System.Random (getStdGen)
import System.Directory (getDirectoryContents)

main :: IO ()
main = do
    args <- getArgs
    let year = case args of [y] -> y 
                            _   -> "2000"
    names <- getDirectoryContents "/home/ivan/CompSci/imaginary-book-ivan-small-2"
    let correctNames = weedOut names
    let namesWithFilePath = map ("/home/ivan/CompSci/imaginary-book-ivan-small-2/" ++) correctNames
    input <- mapM readFile namesWithFilePath
    let originalCharts = parseAndFilterYearsAndKey (read year :: Int) input

    if originalCharts /= [] then do
        let transposedCharts = map (chartTranspose . cleanUp) originalCharts
        let weightedCollection = weight transposedCharts
        let bigCollection = makeBigCollection weightedCollection
        gen <- getStdGen
        let output = generateChartBegin bigCollection gen
        let newyear   = fst (head output)
        let newchords = concat $ reconvert 4 (tail output)
        let newchart  = newyear ++ "\n" ++ newchords 
        writeFile "RandomChart" newchart
        putStrLn newchart

    else do
        putStrLn "\nThat's too early! Give me another year."
    
    where weedOut :: [String] -> [String] 
          weedOut [] = []
          weedOut (l:ls)
            | elem l [".",".."] || elem '~' l = weedOut ls
            | otherwise = l : (weedOut ls)

--If no year is given, the program just supplies a year which should work for all the chord charts in the folder.  
--weedOut finds all the file names that are copies or links to the parent folder and eliminates them so that readFile only reads the txt files that contain the chord charts.  
--originalCharts is only the chord charts that satisfy the given year.  
--transposedCharts transposes it to C.
--bigCollection makes a giant compilation of all the chords progressions, and makes more copies of the charts whose years are closer to the given year.  
-- gen is a random number generator
--newyear is just the year that corresponds to the beginning chord, I'm printing it just to see that the code is working.  
--newchords takes the generated chain of chords and puts them back into the chord notation of the text files.  


