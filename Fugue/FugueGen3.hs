module FugueGen3 where

import Haskore

data MusicalNote = Notei String Int
    deriving (Eq)
type MelodyNum = [Int]
type MelodyNote = [MusicalNote]
data Fugue = FugueNum3 (MelodyNum, MelodyNum, MelodyNum) | FugueNote3 (MelodyNote, MelodyNote, MelodyNote) | FugueNum2 (MelodyNum,MelodyNum) | FugueNote2 (MelodyNote,MelodyNote)
    deriving (Eq)
data FugueL = FugueListed (Fugue, Int)

--Initial Parameters------------------
prohibitedIntervals :: [Int]
prohibitedIntervals = [1,2,10,11] 

--------------------------------------

str2note :: String -> MusicalNote
str2note note =
    case note of 
        "c"  -> Notei "C " 4 
        "db" -> Notei "Db" 4
        "d"  -> Notei "D " 4 
        "eb" -> Notei "Eb" 4
        "e"  -> Notei "E " 4
        "f"  -> Notei "F " 4
        "gb" -> Notei "Gb" 4
        "g"  -> Notei "G " 4
        "ab" -> Notei "Ab" 4
        "a"  -> Notei "A " 4
        "bb" -> Notei "Bb" 4
        "b"  -> Notei "B " 4
        "r"  -> Notei "--" 4
        "c2"  -> Notei "C " 5 
        "db2" -> Notei "Db" 5
        "d2"  -> Notei "D " 5 
        "eb2" -> Notei "Eb" 5
        "e2"  -> Notei "E " 5
        "f2"  -> Notei "F " 5
        "gb2" -> Notei "Gb" 5
        "g2"  -> Notei "G " 5
        "ab2" -> Notei "Ab" 5
        "a2"  -> Notei "A " 5
        "bb2" -> Notei "Bb" 5
        "b2"  -> Notei "B " 5
       
     
-------------------------------------

n2i :: MusicalNote -> Int
n2i note = 
    case note of
        
        Notei "C " a -> 0  + a*12
        Notei "Db" a -> 1  + a*12
        Notei "D " a -> 2  + a*12
        Notei "Eb" a -> 3  + a*12
        Notei "E " a -> 4  + a*12
        Notei "F " a -> 5  + a*12
        Notei "Gb" a -> 6  + a*12
        Notei "G " a -> 7  + a*12
        Notei "Ab" a -> 8  + a*12
        Notei "A " a -> 9 + a*12
        Notei "Bb" a -> 10 + a*12
        Notei "B " a -> 11  + a*12
        Notei "--" a -> 200


--converts a list of notes to list of integers
notesToInts :: MelodyNote -> MelodyNum
notesToInts = map n2i

--modulates the melody into twelve keys, maintaining rests
modulate :: Int -> Int -> Int
modulate _ 200 = 200
modulate a i = a + i

modulateMelody :: MelodyNum -> [MelodyNum]
modulateMelody ms = [ map (modulate i) ms | i <- [0..11] ]   


--adds rests to the end or the beginning of the melody, so that each melody does not start at the same beat

generateRestsShorter :: Int -> MelodyNum -> [MelodyNum]
generateRestsShorter i ns = [ (replicate b 200) ++ ns ++ (replicate (i-b) 200) | b <- [0..i] ]

--presents all possible modulated melodies and starting times
allMelodies :: Int -> MelodyNum -> [MelodyNum]
allMelodies i m = concat $ map (generateRestsShorter i) (modulateMelody m)

--takes a list of melodies and groups them into threes, according to some
--rules.  The first melody will be the initial melody entered, with rests
--added to the end.  "pI" represents the intervals that are prohibited 
--from ever occurring vertically among any two parts.  The intervals now 
--prohibited are unison, a half step, and a whole step. So, it will 
--never be the case that one line is sounding a D while another is 
--sounding a C, C#, D, E, F or F#. 
--This function also specifies that Melody b starts no earlier than the second beat, 
--and Melody c starts no earlier than the third beat.

compareMelodies :: MelodyNum -> MelodyNum -> [Int] -> Bool
compareMelodies (_:as) (200:bs) pI = compareMelodies as bs pI
compareMelodies (200:as) (_:bs) pI = compareMelodies as bs pI
compareMelodies (a:as) (b:bs) pI = (not (elem (mod (abs (a - b)) 12) pI )) && (compareMelodies as bs pI)
compareMelodies _ [] _ = True
compareMelodies [] _ _ = True  

createFugues3 :: [MelodyNum] -> [Fugue]
createFugues3 ps = [ FugueNum3 (a,b,c) |
    b <- ps, c <- ps,
    head b == 200, last b == 200, take 2 c == [200,200], b /= c, last c /= 200,
    compareMelodies a b prohibitedIntervals == True,
    compareMelodies b c prohibitedIntervals == True,
    compareMelodies a c prohibitedIntervals == True ]
    where a = head ps

--converts the list of integers back into notes
i2n :: Int -> MusicalNote
i2n integer
    | integer == 200       = Notei "--" 4   
    | rem integer 12 == 0  = Notei "C " (div integer 12)
    | rem integer 12 == 1  = Notei "Db" (div integer 12)
    | rem integer 12 == 2  = Notei "D " (div integer 12)
    | rem integer 12 == 3  = Notei "Eb" (div integer 12)
    | rem integer 12 == 4  = Notei "E " (div integer 12)
    | rem integer 12 == 5  = Notei "F " (div integer 12)
    | rem integer 12 == 6  = Notei "Gb" (div integer 12)
    | rem integer 12 == 7  = Notei "G " (div integer 12)
    | rem integer 12 == 8  = Notei "Ab" (div integer 12)
    | rem integer 12 == 9 = Notei "A " (div integer 12)
    | rem integer 12 == 10 = Notei "Bb" (div integer 12)
    | rem integer 12 == 11 = Notei "B " (div integer 12)
    

intsToNotes :: MelodyNum -> MelodyNote
intsToNotes = map i2n

convert :: Fugue -> Fugue
convert (FugueNum3 (x,y,z)) = FugueNote3 (intsToNotes x, intsToNotes y, intsToNotes z)
convert (FugueNum2 (x,y)) = FugueNote2 (intsToNotes x, intsToNotes y)

convertAllIntsToNotes :: [Fugue] -> [Fugue]
convertAllIntsToNotes list = map convert list

--GRAND MASTER FUNCTION.  The functions are composed in the order they are defined.  To use this function, simply feed it a list of notes.
{-
fugue3 :: MelodyNote -> [Fugue]
fugue3 m = convertAllIntsToNotes $ createFugues3 $ (allMelodies i) $ notesToInts m
    where i = restNumber m
-}
---Functions for creating a fugue with only two parts
createFugues2 :: [MelodyNum] -> [Fugue]
createFugues2 ps = [ FugueNum2 (a,b) |
    b <- ps, head b == 200,
    compareMelodies a b prohibitedIntervals == True ]
    where a = head ps
     
---Functions for writing the shortest possible fugues

fugueShortA :: Int -> MelodyNote -> [Fugue]
fugueShortA i = convertAllIntsToNotes . createFugues2 . (allMelodies i) . notesToInts

fugueShortB :: Int -> MelodyNote -> [Fugue]
fugueShortB i m
    | i == (length m)-1 = []
    | fugueShortA i m == [] = fugueShortB (i+1) m
    | otherwise = fugueShortA i m 

sfugue :: MelodyNote -> [Fugue]
sfugue = fugueShortB 3

fugueDisplay :: [Fugue] -> [FugueL]
fugueDisplay fugue =map fugueL_ify $ zip fugue [1..(length fugue)]

fugueL_ify :: (Fugue, Int) -> FugueL
fugueL_ify (fugue, int) = FugueListed (fugue, int)

--Display Settings-----------------
instance Show MusicalNote where
  show (Notei x int) = (show x) 

instance Show Fugue where 
  show (FugueNum3 (x,y,z))  = "\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n\n" 
  show (FugueNote3 (x,y,z)) = "\n    Melody I\n" ++ (show x) ++ "\n    Melody II\n" ++ (show y) ++ "\n    Melody III\n" ++ (show z)  ++ "\n,------------\n"
  show (FugueNum2 (x,y))  = "\n" ++ show x ++ "\n" ++ show y ++  "\n\n" 
  show (FugueNote2 (x,y)) = "\n" ++ (show x) ++ "\n" ++ (show y) ++ "\n,----------\n"

instance Show FugueL where 
  show (FugueListed (fugue, int)) = "----------\nNumber " ++ (show int) ++ "\n" ++ show fugue



--Chords------------

type Chord = [MusicalNote]

makeChord :: [Int] -> MusicalNote -> Chord
makeChord chord note = map (i2n . modulate noteInt) chord
    where noteInt = n2i note

majr :: MusicalNote -> Chord
majr = makeChord [0,4,7]
minr = makeChord [0,3,7]
maj7 = makeChord [0,4,7,11]
min7 = makeChord [0,3,7,10]
dom7 = makeChord [0,4,7,10]
dim7 = makeChord [0,3,6,9]
hfd7 = makeChord [0,3,6,10]
mnmj = makeChord [0,3,7,11]
augm = makeChord [0,4,8]
shp11 = makeChord [0,4,6,10]

--Scales-----------------------

rotate :: Int -> [a] -> [a]
rotate 0 x = x 
rotate y (x:xs) = rotate (y-1) ((xs) ++ [x])

ionian :: MusicalNote -> Chord
ionian = makeChord [0,2,4,5,7,9,11]

scale :: Int -> Int -> MusicalNote -> Chord
scale shift typeOfScaleShift noteNote = rotate shift $ ionian $ i2n $ modulate (n2i noteNote) typeOfScaleShift

dorian = scale 1 10 
phrygian = scale 2 8
lydian = scale 3 7 
mixolydian = scale 4 5
aolian = scale 5 3
locrian = scale 6 1  

harmonicMinor = makeChord [0,2,3,5,7,8,11]
------------------------------

--FRACTAL GENERATOR (MelodyIterator)

--applies modulate to a list, flipping the order of arguments to allow for possible future eta-reduction.  
flippedMapModulate :: MelodyNum -> Int -> MelodyNum
flippedMapModulate ys x = map (modulate x) ys  

--finds the intervals between notes in a melody
findIntervals :: [Int] -> [Int]
findIntervals notes = map (addSpecial negativeHead) notes 
    where negativeHead = head notes * (-1)

addSpecial x y
    | y == 200 = 200
    | otherwise = x + y 

--creates a melody fractal, which i'm defining to be repetitions of the melody in various keys, according to the intervals contained in the melody, then concatenates everything together. The integer argument will tell the function how many times you want to repeat the process using only the original melody.  
fractalWithNum :: Int -> MelodyNum -> MelodyNum
fractalWithNum 1 notes = notes 
fractalWithNum i notes  = concatMap (flippedMapModulate (fractalWithNum (i-1) notes)) (findIntervals notes)

--converts the melody to integers, makes a fractal, then converts it back
fractal :: Int -> MelodyNote -> MelodyNote 
fractal i = intsToNotes . (fractalWithNum i) . notesToInts

------------------------------------

mightSwap :: (MelodyNum, MelodyNum) -> (MelodyNum, MelodyNum)
mightSwap (a,b)
    | length a >= length b = (a,b)
    | otherwise = (b,a)

generateRestsMash :: (MelodyNum, MelodyNum) -> [MelodyNum]
generateRestsMash (mel1, mel2) = [ (replicate b 200) ++ mel2 ++ (replicate (k-b) 200) | b <- [0..k] ] 
    where k = length mel1 - length mel2
    
allMelodiesMash :: (MelodyNum, MelodyNum) -> [MelodyNum]
allMelodiesMash (a,b) = concat $ map modulateMelody (generateRestsMash (a,b))

premashup :: (MelodyNum, MelodyNum) -> [Fugue]
premashup (mel1, mel2) = [ FugueNum2 (mel1,b) |
    b <- allMelodiesMash (mel1,mel2),
    compareMelodies mel1 b prohibitedIntervals == True ]

mashup :: MelodyNote -> MelodyNote -> [Fugue]
mashup mel1 mel2 = convertAllIntsToNotes $ premashup $ mightSwap (notesToInts mel1, notesToInts mel2) 

----------------------
checkNotes :: [String] -> Bool
checkNotes [] = True
checkNotes (n:ns) = elem n ["c","db","d","eb","e","f","gb","g","ab","a","bb","b","r","rs","c2","db2","d2","eb2","e2","f2","gb2","g2","ab2","a2","bb2"] && checkNotes ns

-----Haskore--------

n2h dur note = 
    case note of
        Notei "B " octave  -> Note (B, octave) (dur) []
        Notei "C " octave  -> Note (C, octave) (dur) []
        Notei "Db" octave -> Note (Df, octave) (dur) []
        Notei "D " octave  -> Note (D, octave) (dur) []
        Notei "Eb" octave -> Note (Ef, octave) (dur) []
        Notei "E " octave  -> Note (E, octave) (dur) []
        Notei "F " octave  -> Note (F, octave) (dur) []
        Notei "Gb" octave -> Note (Gf, octave) (dur) []
        Notei "G " octave  -> Note (G, octave) (dur) []
        Notei "Ab" octave -> Note (Af, octave) (dur) []
        Notei "A " octave  -> Note (A, octave) (dur) []
        Notei "Bb" octave -> Note (Bf, octave) (dur) []
        Notei "--" _      -> Rest (dur)

downOctave :: MusicalNote -> MusicalNote 
downOctave (Notei a i) = Notei a (i-1) 

upOctave :: MusicalNote -> MusicalNote 
upOctave (Notei a i) = Notei a (i+1) 

convert1 int notes= map (n2h int) notes 

--convert :: Fugue -> 
convert4 int (FugueNote2 (a,b)) = ( convert3 (convert1 int (map upOctave a) ) ) :=: ( convert3 (convert1 int (map downOctave b) ) )

-- convert3 [(a,b)] ->
convert3 [] = Rest (0.1)
convert3 (a:as) = a :+: (convert3 as)

back2back [] = Rest (0.5)
back2back (m:ms) = m :+: (Rest (0.5)) :+: (back2back ms)

playfractal int notes = back2backFast $ convert1 int notes

back2backFast [] = Rest (0.5)
back2backFast (n:ns) = n :+: back2backFast ns









{-
compareMelodies :: MelodyNum -> MelodyNum -> [Int] -> Bool
compareMelodies (70:as) (70:bs) pI = (compareMelodies as bs pI)
compareMelodies (a:as) (b:bs) pI = (not (elem (abs (a - b)) pI )) && (compareMelodies as bs pI)
compareMelodies _ [] _ = True
compareMelodies [] _ _ = True  

createFugues3 :: [MelodyNum] -> [Fugue]
createFugues3 ps = [ FugueNum3 (a,b,c) |
    b <- ps, c <- ps,
    head b == 70, last b == 70, take 2 c == [70,70], b /= c, last c /= 70,
    compareMelodies a b prohibitedIntervals == True,
    compareMelodies b c prohibitedIntervals == True,
    compareMelodies a c prohibitedIntervals == True ]
    where a = head ps
-}






{-

-------------SCRATCH WORK--------------------------------

--fractalWithNum 2 notes  = concatMap (flippedMapModulate notes) (findIntervals notes)

fractal notesNotes = intsToNotes $ concatMap (mapModulate notes) (findIntervals notes)  
    where notes = notesToInts notesNotes
-}

{-fractal notes = mapModulate notes 0 ++ map (modulate 1) notes ++ map (modulate 0) notes -}








{-
dorn :: MusicalNote -> Chord 
dorn noteNote = rotate 1 $ ionn $ i2n $ modulate (n2i noteNote) 10
-}

--phrg :: MusicalNote -> Chord


{-
preFugue1 :: [MelodyNum] -> [Fugue]
preFugue1 ps = [ FugueNum (a,b,c) |
    b <- ps, c <- ps,
    head b == 70, take 2 c == [70,70],
    j <- [0..(length ps + 1 )],
    not $ elem (abs ( (a !! j) - (b !! j))) g,
    not $ elem (abs ( (b !! j) - (c !! j))) g,
    not $ elem (abs ( (c !! j) - (a !! j))) g ]
    where a = head ps
          g = [0,1,2,10,11]
-}


{-
intToNote ints = 
    case ints of
        0  -> "B "
        1  -> "C "
        2  -> "Db"
        3  -> "D "
        4  -> "Eb"
        5  -> "E "
        6  -> "F "
        7  -> "Gb"
        8  -> "G "
        9  -> "Ab"
        10 -> "A "
        11 -> "Bb"
        70 -> "rs"


noteToInt notes = 
    case notes of
        "B "  -> 0
        "C "  -> 1
        "Db" -> 2
        "D "  -> 3
        "Eb" -> 4
        "E "  -> 5
        "F "  -> 6
        "Gb" -> 7
        "G "  -> 8
        "Ab" -> 9
        "Bb" -> 11
        "A "  -> 10
        "rs" -> 70

n2i = map noteToInt

i2n = map intToNote

b  = "B "
c  = "C "
db = "Db"
d  = "D "
eb = "Eb"
e  = "E "
f  = "F "
gb = "Gb"
g  = "G "
ab = "Ab"
a  = "A "
bb = "Bb"
rs = "rs"

-}


{-
preFugue1 :: [MelodyNum] -> [Fugue]
preFugue1 ps = [ FugueNum (a,b,c) |
    b <- ps, c <- ps,
    head b == 70, take 2 c == [70,70],
    not $ elem (abs ( (a !! 1) - (b !! 1))) g,
    not $ elem (abs ( (b !! 1) - (c !! 1))) g,
    not $ elem (abs ( (c !! 1) - (a !! 1))) g ]
    not $ elem (abs ( (a !! 2) - (b !! 2))) g,
    not $ elem (abs ( (b !! 2) - (c !! 2))) g,
    not $ elem (abs ( (c !! 2) - (a !! 2))) g ]
    where a = head ps
          g = [0,1,2,10,11]

{-
newtype Test = Hey String 
newtype Test2 = Hey2 (Test,Test,Test)

i = Hey "you"

instance Show Test where
  show (Hey a) = (show a) ++"/n" ++ "d"

-}

-}
