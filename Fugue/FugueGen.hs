module FugueGen where

import Haskore

newtype MusicalNote = Note String
    deriving (Eq)
type MelodyNum = [Int]
type MelodyNote = [MusicalNote]
data Fugue = FugueNum3 (MelodyNum, MelodyNum, MelodyNum) | FugueNote3 (MelodyNote, MelodyNote, MelodyNote) | FugueNum2 (MelodyNum,MelodyNum) | FugueNote2 (MelodyNote,MelodyNote)
    deriving (Eq)
data FugueL = FugueListed (Fugue, Int)

--Initial Parameters------------------
prohibitedIntervals :: [Int]
prohibitedIntervals = [0,1,2,10,11] 

restNumber :: [a] -> Int
restNumber xs = div (length xs) 2 + 1
--------------------------------------

b  = Note "B "
c  = Note "C "
db = Note "Db"
d  = Note "D "
eb = Note "Eb"
e  = Note "E "
f  = Note "F "
gb = Note "Gb"
g  = Note "G "
ab = Note "Ab"
a  = Note "A "
bb = Note "Bb"
rs = Note "--"
r  = Note "--"

str2note :: String -> MusicalNote
str2note note =
    case note of 
        "b"  -> Note "B "
        "c"  -> Note "C "  
        "db" -> Note "Db" 
        "d"  -> Note "D "  
        "eb" -> Note "Eb"
        "e"  -> Note "E " 
        "f"  -> Note "F "
        "gb" -> Note "Gb"
        "g"  -> Note "G "
        "ab" -> Note "Ab"  
        "a"  -> Note "A " 
        "bb" -> Note "Bb"  
        "r"  -> Note "--" 
     


-------------------------------------

n2i :: MusicalNote -> Int
n2i note = 
    case note of
        Note "B "  -> 0
        Note "C "  -> 1
        Note "Db" -> 2
        Note "D "  -> 3
        Note "Eb" -> 4
        Note "E "  -> 5
        Note "F "  -> 6
        Note "Gb" -> 7
        Note "G "  -> 8
        Note "Ab" -> 9
        Note "A "  -> 10
        Note "Bb" -> 11
        Note "--" -> 70


--converts a list of notes to list of integers
notesToInts :: MelodyNote -> MelodyNum
notesToInts = map n2i

--modulates the melody into twelve keys, maintaining rests
modulate :: Int -> Int -> Int
modulate _ 70 = 70
modulate a i = mod (a + i) 12

modulateMelody :: MelodyNum -> [MelodyNum]
modulateMelody ms = [ map (modulate i) ms | i <- [0..11] ]   


--adds rests to the end or the beginning of the melody, so that each melody does not start at the same beat
generateRests :: MelodyNum -> [MelodyNum]
generateRests ns = [ (replicate b 70) ++ ns ++ (replicate (k-b) 70) | b <- [0..k] ] 
    where k = restNumber ns

generateRestsShorter :: Int -> MelodyNum -> [MelodyNum]
generateRestsShorter i ns = [ (replicate b 70) ++ ns ++ (replicate (i-b) 70) | b <- [0..i] ]

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

--converts the list of integers back into notes
i2n :: Int -> MusicalNote
i2n integer = 
    case integer of
        0  -> Note "B "
        1  -> Note "C "
        2  -> Note "Db"
        3  -> Note "D "
        4  -> Note "Eb"
        5  -> Note "E "
        6  -> Note "F "
        7  -> Note "Gb"
        8  -> Note "G "
        9  -> Note "Ab"
        10 -> Note "A "
        11 -> Note "Bb"
        70 -> Note "--"

intsToNotes :: MelodyNum -> MelodyNote
intsToNotes = map i2n

convert :: Fugue -> Fugue
convert (FugueNum3 (x,y,z)) = FugueNote3 (intsToNotes x, intsToNotes y, intsToNotes z)
convert (FugueNum2 (x,y)) = FugueNote2 (intsToNotes x, intsToNotes y)

convertAllIntsToNotes :: [Fugue] -> [Fugue]
convertAllIntsToNotes list = map convert list

--GRAND MASTER FUNCTION.  The functions are composed in the order they are defined.  To use this function, simply feed it a list of notes.

fugue3 :: MelodyNote -> [Fugue]
fugue3 m = convertAllIntsToNotes $ createFugues3 $ (allMelodies i) $ notesToInts m
    where i = restNumber m

---Functions for creating a fugue with only two parts
createFugues2 :: [MelodyNum] -> [Fugue]
createFugues2 ps = [ FugueNum2 (a,b) |
    b <- ps, head b == 70,
    compareMelodies a b prohibitedIntervals == True ]
    where a = head ps
     
fugue2 :: MelodyNote -> [Fugue]
fugue2 m = convertAllIntsToNotes $ createFugues2 $ (allMelodies i) $ notesToInts m
    where i = restNumber m

---Functions for writing the shortest possible fugues
fugueShortA :: Int -> MelodyNote -> [Fugue]
fugueShortA i = convertAllIntsToNotes . createFugues2 . (allMelodies i) . notesToInts
  

fugueShortB :: Int -> MelodyNote -> [Fugue]
fugueShortB i m
    | fugueShortA i m == [] = fugueShortB (i+1) m
    | otherwise = fugueShortA i m 

sfugue :: MelodyNote -> [Fugue]
sfugue = fugueShortB 1

fugueDisplay :: [Fugue] -> [FugueL]
fugueDisplay fugue =map fugueL_ify $ zip fugue [1..(length fugue)]

fugueL_ify :: (Fugue, Int) -> FugueL
fugueL_ify (fugue, int) = FugueListed (fugue, int)
--Display Settings-----------------
instance Show MusicalNote where
  show (Note x) = (show x)

instance Show Fugue where 
  show (FugueNum3 (x,y,z))  = "\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n\n" 
  show (FugueNote3 (x,y,z)) = "\n    Melody I\n" ++ (show x) ++ "\n    Melody II\n" ++ (show y) ++ "\n    Melody III\n" ++ (show z)  ++ "\n,------------\n"
  show (FugueNum2 (x,y))  = "\n" ++ show x ++ "\n" ++ show y ++  "\n\n" 
  show (FugueNote2 (x,y)) = "\n" ++ (show x) ++ "\n" ++ (show y) ++ "\n,----------\n"

instance Show FugueL where 
  show (FugueListed (fugue, int)) = "----------\nNumber " ++ (show int) ++ "\n" ++ show fugue



---Library of Songs-------------------------
--If fugue3 doesn't produce results, fugue2 most likely will.

melody1 = [e,r,r,eb,e,r,g,r,a]
--Melody1 

--Melody2 
melody2 = [c,r,e,r,f,r,gb,g]

--Freddie Freeloader
freddie = [g,r,r,f,r,r,r,r,r,r,r,r,r,r,r,r,g,r,r,f,r,r,r,r,r,r,r,r,r,r,r,r,c,r,r,bb,r,r,r,r,r,r,r,r,r,r,r,r,g,r,r,f,r,r,r,r,r,r,r,r,r,r,ab,r,a,r,r,r,r,r,ab,r,g,r,r,r,r,r,f,r,gb]

--Entertainer
entertainer = [d,eb,e,c,r,e,c,r,e,c,r,r,r,r,r,c,d,eb,e,c,d,e,r,b,d,r,c]

--You Are My Sunshine
sunshine = [c,f,g,a,rs,a,rs,rs,a,ab,a,f,rs,f,rs,rs,f,g,a,bb,rs,d,rs,rs,d,c,bb,a,rs,rs,rs,rs,f,g,a,bb,rs,d,rs,rs,d,c,bb,a,rs,f,rs,rs,c,f,g,a,rs,rs,bb,g,rs,g,a,f]

--Mary Had a Little Lamb
mary = [e,d,c,d,e,e,e,rs,d,d,d,rs,e,g,g,rs,e,d,c,d,e,e,e,e,d,d,e,d,c,rs,rs]

--Theme of "Music of the Night"
theme_music_of_the_night = [g,f,g,a,a,c]


--Pokemon Theme Song Verse
pokemon = [g,g,g,g,rs,g,f,rs,d,bb,rs,rs,rs,rs,g,g,rs,rs,f,rs,eb,f,rs,rs,rs,rs,rs,rs,rs,rs,rs,ab,ab,ab,ab,rs,rs,ab,g,rs,f,eb,rs,rs,rs,f,g,g,rs,rs,f,rs,eb,g]

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
flippedMapModulate y x = map (modulate x) y  

--finds the intervals between notes in a melody
findIntervals :: [Int] -> [Int]
findIntervals notes = map (+negativeHead) notes 
    where negativeHead = head notes * (-1)

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
generateRestsMash (mel1, mel2) = [ (replicate b 70) ++ mel2 ++ (replicate (k-b) 70) | b <- [0..k] ] 
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
checkNotes (n:ns) = elem n ["b","c","db","d","eb","e","f","gb","g","ab","a","bb","r","rs"] && checkNotes ns

-----Haskore--------

n2h note = 
    case note of
        Note "B "  -> Note (B, 4) 1 []
        Note "C "  -> Note (C, 4) 1 []
        Note "Db" -> Note (Db, 4) 1 []
        Note "D "  -> Note (D, 4) 1 []
        Note "Eb" -> Note (Eb, 4) 1 []
        Note "E "  -> Note (E, 4) 1 []
        Note "F "  -> Note (F, 4) 1 []
        Note "Gb" -> Note (Gb, 4) 1 []
        Note "G "  -> Note (G, 4) 1 []
        Note "Ab" -> Note (Ab, 4) 1 []
        Note "A "  -> Note (A, 4) 1 []
        Note "Bb" -> Note (Bb, 4) 1 []
        Note "--" ->  Note (C, 4) 1 []

convert1 notes = map n2h notes 

--convert :: Fugue -> 
convert2 FugueNote2 (a,b) = zip (convert1 a) (convert1 b)

-- convert3 [(a,b)] -> 
convert3 ((a,b):rest) = (a :+: b) :=: (convert3 rest)

convert4 = convert3 . convert2 
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
