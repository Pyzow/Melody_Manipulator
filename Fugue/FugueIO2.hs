module Main where

import FugueGen2
import System.IO
import System.Environment
import Haskore

{-
main :: IO ()
main = do
    args <- getArgs
    let g = map str2note args
    if sfugue g == []
        then do putStrLn "No fugues could be generated"
    else do 
        putStrLn $ show $ sfugue g
-}

main :: IO ()
main = do 
    putStrLn "What would you like to create?\nf - fugue\nm - mashup\nr - fractal\nh - please explain"
    input <- getLine
    pickProgram input
 

pickProgram :: String -> IO ()
pickProgram string = 
    case string of
        "f" -> runFugue
        "m" -> runMashup
        "r" -> runFractal
        "h" -> helpFugue
        _   -> main

helpFugue :: IO ()
helpFugue = do 
    putStrLn "For this program, you can enter melodies of eighth notes, separated by spaces.  The appropriate note names are: \nc\ndb\nd\neb\ne\nf\ngb\ng\nab\na\nbb\nb\nr - to indicate a rest"
    main

runFugue :: IO ()
runFugue = do 
    putStrLn "Please enter a melody to be fugued."
    input2 <- getLine
    let notes = words input2
    if checkNotes notes == True
        then do
                putStrLn "Please enter a speed as a decimal number."
                input2a <- getLine
                let h = map (convert4 (0.1)) $ sfugue $ map str2note notes
                let g = back2back h
                let ourMidiFile = testMidi g
                outputMidiFile "simple_example.mid" ourMidiFile
        
    else do
            putStrLn "Sorry! Something about that melody wasn't entered correctly. Let's try again."
            runFugue
            
        
runMashup :: IO ()
runMashup = do 
    putStrLn "Please enter the first melody to be mashed."
    input3 <- getLine
    putStrLn "Please enter the second melody to be mashed."
    input4 <- getLine
    let melody1 = map str2note $ words input3
    let melody2 = map str2note $ words input4
    putStrLn $ show $ fugueDisplay $ mashup melody1 melody2

runFractal :: IO ()
runFractal = do 
    putStrLn "Please enter a melody to be fractalled."
    input5 <- getLine
    putStrLn "Please enter the degree of fractalization you would like."
    input6 <- getLine
    let g = playfractal (0.04) $ fractal (read input6) $ map str2note $ words input5
    let ourMidiFile = testMidi g
    outputMidiFile "simple_example.mid" ourMidiFile
   
--runMashup :: IO ()

--runFractal :: IO ()


{-
takeEm :: [MusicalNote] -> IO [MusicalNote]
takeEm = do 
    input <- getLine
    if input /= '.'
        then do let input ++ notes
-}
