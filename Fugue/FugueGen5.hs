--FugueGen5.hs
--Ivan Pyzow pyzow@uchicago.edu

-------------------------------------
--DECLARATIONS ----------------------
-------------------------------------

module FugueGen5 where

import Haskore
import Data.Char (isDigit)

data MusicalNote = Notei String Int
    deriving (Eq)
type MelodyNum = [Int]
type MelodyNote = [MusicalNote]
data Fugue = FugueNum3 (MelodyNum, MelodyNum, MelodyNum) | FugueNote3 (MelodyNote, MelodyNote, MelodyNote) | FugueNum2 (MelodyNum,MelodyNum) | FugueNote2 (MelodyNote,MelodyNote)
    deriving (Eq)
data FugueL = FugueListed (Fugue, Int)

--------------------------------------
--INITIAL PARAMETERS------------------
--------------------------------------

prohibitedIntervals :: [Int]
prohibitedIntervals = [1,2,10,11] 


--------------------------------------
--CONVERSION FUNCTIONS ---------------
--------------------------------------

ci  = Notei "C " 4
dbi = Notei "Db" 4
di  = Notei "D " 4
ebi = Notei "Eb" 4
ei  = Notei "E " 4
fi  = Notei "F " 4
gbi = Notei "Gb" 4
gi  = Notei "G " 4
abi = Notei "Ab" 4
ai  = Notei "A " 4
bbi = Notei "Bb" 4
bi  = Notei "B " 4


str2note :: String -> MusicalNote
str2note note =
    case note of 
        "c"   -> Notei "C " 4 
        "db"  -> Notei "Db" 4
        "d"   -> Notei "D " 4 
        "eb"  -> Notei "Eb" 4
        "e"   -> Notei "E " 4
        "f"   -> Notei "F " 4
        "gb"  -> Notei "Gb" 4
        "g"   -> Notei "G " 4
        "ab"  -> Notei "Ab" 4
        "a"   -> Notei "A " 4
        "bb"  -> Notei "Bb" 4
        "b"   -> Notei "B " 4
        "r"   -> Notei "--" 4
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
        Notei "A " a -> 9  + a*12
        Notei "Bb" a -> 10 + a*12
        Notei "B " a -> 11 + a*12
        Notei "--" a -> 200

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
    | rem integer 12 == 9  = Notei "A " (div integer 12)
    | rem integer 12 == 10 = Notei "Bb" (div integer 12)
    | rem integer 12 == 11 = Notei "B " (div integer 12)

--n2h :: Haskore.Basics.Dur -> MusicalNote -> Haskore.Basics.Music
n2h dur note =    {- dur = duration -}
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

--converts a list of notes to list of integers
notesToInts :: MelodyNote -> MelodyNum
notesToInts = map n2i

intsToNotes :: MelodyNum -> MelodyNote
intsToNotes = map i2n

convert :: Fugue -> Fugue
convert (FugueNum3 (x,y,z)) = FugueNote3 (intsToNotes x, intsToNotes y, intsToNotes z)
convert (FugueNum2 (x,y)) = FugueNote2 (intsToNotes x, intsToNotes y)

convertAllIntsToNotes :: [Fugue] -> [Fugue]
convertAllIntsToNotes list = map convert list


-------------------------------------------
--GENERAL FUNCTIONS FOR THE GENERATORS-----
-------------------------------------------

--modulates the melody into twelve keys, maintaining rests
modulate :: Int -> Int -> Int
modulate _ 200 = 200
modulate a i = a + i

--creates copies of the melody in all twelve keys
modulateMelody :: MelodyNum -> [MelodyNum]
modulateMelody ms = [ map (modulate i) ms | i <- [0..11] ]   

--adds rests to the end or the beginning of the melody, so that each melody does not start at the same beat
generateRestsShorter :: Int -> MelodyNum -> [MelodyNum]
generateRestsShorter i ns = [ (replicate b 200) ++ ns ++ (replicate (i-b) 200) | b <- [0..i] ]

--presents all possible modulated melodies and starting times
allMelodies :: Int -> MelodyNum -> [MelodyNum]
allMelodies i m = concat $ map (generateRestsShorter i) (modulateMelody m)

--checks to see if two melodies violate the prohibited intervals when played together
compareMelodies :: MelodyNum -> MelodyNum -> [Int] -> Bool
compareMelodies (_:as) (200:bs) pI = compareMelodies as bs pI
compareMelodies (200:as) (_:bs) pI = compareMelodies as bs pI
compareMelodies (a:as) (b:bs) pI = (not (elem (mod (abs (a - b)) 12) pI )) && (compareMelodies as bs pI)
compareMelodies _ [] _ = True
compareMelodies [] _ _ = True  


------------------------------------------------
--FUGUE GENERATORS------------------------------
------------------------------------------------

--createFugue3 and createFugue2 take a list of melodies and group them into twos or threes, according to some
--rules.  The first melody will be the initial melody entered, with rests
--added to the end.  "pI" represents the intervals that are prohibited 
--from ever occurring vertically among any two parts.  The intervals now 
--prohibited are unison, a half step, and a whole step. So, it will 
--never be the case that one line is sounding a D while another is 
--sounding a C, C#, D, E, F or F#. 
--This function also specifies that Melody b starts no earlier than the second beat, 
--and Melody c starts no earlier than the third beat.

createFugues2 :: [Int] -> [MelodyNum] -> [Fugue]
createFugues2 pI ps = [ FugueNum2 (a,b) |
    b <- ps, head b == 200,
    compareMelodies a b pI == True ]
    where a = head ps

createFugues3 :: [Int] -> [MelodyNum] -> [Fugue]
createFugues3 pI ps = [ FugueNum3 (a,b,c) |
    b <- ps, c <- ps,
    head b == 200, last b == 200, take 2 c == [200,200], b /= c, last c /= 200,
    compareMelodies a b pI == True,
    compareMelodies b c pI == True,
    compareMelodies a c pI == True ]
    where a = head ps
     
---Functions for writing the shortest possible fugues
fugueShortA :: String -> [Int] -> Int -> MelodyNote -> [Fugue]
fugueShortA "3" pI r = convertAllIntsToNotes . (createFugues3 pI) . (allMelodies r) . notesToInts
fugueShortA _ pI r = convertAllIntsToNotes . (createFugues2 pI) . (allMelodies r) . notesToInts

fugueShortB :: String -> [Int] -> Int -> MelodyNote -> [Fugue]
fugueShortB dOF pI r m
    | r == (length m)-1 = []
    | fugueShortA dOF pI r m == [] = fugueShortB dOF pI (r+1) m
    | otherwise = fugueShortA dOF pI r m 

sfugue :: String -> [Int] -> MelodyNote -> [Fugue]
sfugue dOF pI = fugueShortB dOF pI 3


---------------------------------------
--MASHUP GENERATOR---------------------
---------------------------------------

mightSwap :: (MelodyNum, MelodyNum) -> (MelodyNum, MelodyNum)
mightSwap (a,b)
    | length a >= length b = (a,b)
    | otherwise = (b,a)

generateRestsMash :: (MelodyNum, MelodyNum) -> [MelodyNum]
generateRestsMash (mel1, mel2) = [ (replicate b 200) ++ mel2 ++ (replicate (k-b) 200) | b <- [0..k] ] 
    where k = length mel1 - length mel2
    
allMelodiesMash :: (MelodyNum, MelodyNum) -> [MelodyNum]
allMelodiesMash (a,b) = concat $ map modulateMelody (generateRestsMash (a,b))

premashup :: [Int] -> (MelodyNum, MelodyNum) -> [Fugue]
premashup pI (mel1, mel2) = [ FugueNum2 (mel1,b) |
    b <- allMelodiesMash (mel1,mel2),
    compareMelodies mel1 b pI == True ]

mashup :: [Int] -> MelodyNote -> MelodyNote -> [Fugue]
mashup pI mel1 mel2 = convertAllIntsToNotes $ premashup pI $ mightSwap (notesToInts mel1, notesToInts mel2) 

---------------------------------------------------------
--FRACTAL GENERATOR (MelodyIterator) --------------------
---------------------------------------------------------

--applies modulate to a list, flipping the order of arguments to allow for possible future eta-reduction.  
flippedMapModulate :: MelodyNum -> Int -> MelodyNum
flippedMapModulate ys x = map (modulate x) ys  

--finds the intervals between notes in a melody
findIntervals :: [Int] -> [Int]
findIntervals notes = map (addSpecial negativeHead) notes 
    where negativeHead = head notes * (-1)

addSpecial :: (Eq a, Num a) => a -> a -> a
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


--------------------------
-----HASKORE STUFF--------
--------------------------

downOctave :: MusicalNote -> MusicalNote 
downOctave (Notei a i) = Notei a (i-1) 

upOctave :: MusicalNote -> MusicalNote 
upOctave (Notei a i) = Notei a (i+1)

--convert1 :: Haskore.Basics.Dur -> [MusicalNote] -> [Haskore.Basics.Music]
convert1 int notes= map (n2h int) notes 

--convert3 :: [Haskore.Basics.Music] -> Haskore.Basics.Music
convert3 [] = Rest (0.1)
convert3 (a:as) = a :+: (convert3 as)

--convert4 :: Haskore.Basics.Dur -> Fugue -> Haskore.Basics.Music
convert4 int (FugueNote2 (a,b)) = ( convert3 (convert1 int a) ) :=: ( convert3 (convert1 int (map upOctave b) ) )
convert4 int (FugueNote3 (a,b,c)) = ( convert3 (convert1 int (map downOctave a)) ) :=: ( convert3 (convert1 int (map upOctave b) ) ) :=: ( convert3 (convert1 int (map (upOctave . upOctave) c) ) )

--back2back :: [Haskore.Basics.Music] -> Haskore.Basics.Music
back2back [] = Rest (0.5)
back2back (m:ms) = m :+: (Rest (0.5)) :+: (back2back ms)

--back2backFast :: [Haskore.Basics.Music] -> Haskore.Basics.Music
back2backFast [] = Rest (0.5)
back2backFast (n:ns) = n :+: back2backFast ns

--playfractal :: Haskore.Basics.Dur -> [MusicalNote] -> Haskore.Basics.Music
playfractal int notes = back2backFast $ convert1 int notes

--------------------------------------------
--VALIDITY-CHECKING FUNCTIONS---------------
--------------------------------------------

checkNotes :: [String] -> Bool
checkNotes [] = True
checkNotes (n:ns) = elem n ["c","db","d","eb","e","f","gb","g","ab","a","bb","b","r","c2","db2","d2","eb2","e2","f2","gb2","g2","ab2","a2","bb2","b2"] && checkNotes ns

checkDigits :: String -> Bool
checkDigits [] = True
checkDigits (n:ns) = isDigit n && checkDigits ns

--------------------
--CHORDS------------
--------------------

type Chord = [MusicalNote]

reset :: MusicalNote -> MusicalNote
reset (Notei a _) = Notei a 4

makeChord :: [Int] -> MusicalNote -> Chord
makeChord chord note = map reset $ map (i2n . modulate noteInt) chord
    where noteInt = n2i note

majr :: MusicalNote -> Chord
majr = makeChord [0,4,7]
minr = makeChord [0,3,7]
maj7 = makeChord [0,4,7,11]
min7 = makeChord [0,3,7,10]
dom7 = makeChord [0,4,7,10]
dim7 = makeChord [0,3,6,9]
hfd7 = makeChord [0,3,6,10]
mnm7 = makeChord [0,3,7,11]
augm = makeChord [0,4,8]

allTypesOfChords = [majr, minr, maj7, min7, dom7, dim7, hfd7, mnm7, augm]
allNotes = makeChord [0,1,2,3,4,5,6,7,8,9,10,11] ci
    
mapAll :: [a] -> [(a -> b)] -> [b]
mapAll notes chords = [ c n | c <- chords, n <- notes ]

allChords = mapAll allNotes allTypesOfChords

allTypesofChordsNames = ["maj7","min7","dom7","hfd7","mnm7","augm"]
allNoteNames   = ["C ","Db","D ","Eb","E ","F ","Gb","G ","Ab","A ","Bb","B "]

allChordsNames = [ChordName a b | a <- allNoteNames, b <- allTypesofChordsNames ]

chord2Notes :: ChordName -> Chord
chord2Notes (ChordName note tonality) = (convertTonality tonality) (convertNote note)

convertTonality a = case a of
    "majr" -> majr
    "minr" -> minr
    "maj7" -> maj7
    "min7" -> min7
    "dom7" -> dom7
    "hfd7" -> hfd7
    "mnm7" -> mnm7
    "augm" -> augm

convertNote a = Notei a 4

 
   
-------------------------------
--SCALES-----------------------
-------------------------------

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

-------------------------------------------------
--Chord Generator
-------------------------------------------------
data ChordName = ChordName String String 
    deriving Eq

everyOther :: [a] -> [a]
everyOther [] = []
everyOther [x] = [x]
everyOther (n:ns) = n : (everyOther (tail ns))

type ChordList = ( [(ChordName, Int)], MelodyNote)

findChord :: Int -> [ChordName] -> ChordList -> [ChordList]
findChord _ _ (a,[]) = [(a,[])]
findChord i chords (a,melody) = [ (a++[(c,i)],m) | c <- chords, notesBelong (take i melody) (chord2Notes c) == True ]
    where m = takeAway melody i

notesBelong :: MelodyNote -> Chord -> Bool
notesBelong [] _ = True
notesBelong (m:ms) chord = elem m chord && notesBelong ms chord

findChord2 :: Int -> [ChordName] -> ChordList -> [ChordList] 
findChord2 j chords (a,melody)
    | j == 0                              = []
    | j > length melody || 
      findChord j chords (a,melody) == [] = findChord2 (j-1) chords (a,melody) 
    | otherwise                           = findChord   j    chords (a,melody)

findChord3 = findChord2 8

takeAway :: [a] -> Int -> [a]
takeAway [] _ = []
takeAway list 0 = list
takeAway list i = takeAway (tail list) (i-1)

take2 :: Int -> [a] -> [a]
take2 i a
    | i > length a = take2 (i-1) a
    | otherwise    = take i a

keepFinding :: [ChordName] -> [ChordList] -> [ChordList]
keepFinding [] _ = []
keepFinding ichords chordLists
    | areAllDone chordLists == True = chordLists
    | otherwise = keepFinding ichords $ concatMap (findChord3 ichords) chordLists 

areAllDone :: [ChordList] -> Bool
areAllDone [] = True
areAllDone ((_,[]):ms) = areAllDone ms
areAllDone ((_,other):_) = False 

initMelody4Chords :: MelodyNote -> [ChordList]
initMelody4Chords melody = [ ( [], melody) ]

writeChart :: MelodyNote -> [ChordList]
writeChart melody = keepFinding allChordsNames (initMelody4Chords melody)

chordList2written :: ChordList -> [ChordName]
chordList2written ([],[]) = []
chordList2written (((a,int):as), _) = [a] ++ (replicate (int-1) (ChordName "  " "Rest")) ++ (chordList2written (as,[])) 

writeChart2 :: MelodyNote -> [[ChordName]]
writeChart2 = (map chordList2written) . writeChart 

-------------------------------------------------
--DISPLAY SETTINGS AND FUNCTIONS-----------------
-------------------------------------------------

fugueDisplay :: [Fugue] -> [FugueL]
fugueDisplay fugue =map fugueL_ify $ zip fugue [1..(length fugue)]

fugueL_ify :: (Fugue, Int) -> FugueL
fugueL_ify (fugue, int) = FugueListed (fugue, int)

instance Show ChordName where
  show (ChordName "  " "Rest") = "------"
  show (ChordName a b) = a ++ b

instance Show MusicalNote where
  show (Notei x int) = x 

instance Show Fugue where 
  show (FugueNum3 (x,y,z))  = "\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n\n" 
  show (FugueNote3 (x,y,z)) = "\n" ++ (show x) ++ "\n" ++ (show y) ++ "\n" ++ (show z)  ++ "\n,------------\n"
  show (FugueNum2 (x,y))  = "\n" ++ show x ++ "\n" ++ show y ++  "\n\n" 
  show (FugueNote2 (x,y)) = "\n" ++ (show x) ++ "\n" ++ (show y) ++ "\n,----------\n"

instance Show FugueL where 
  show (FugueListed (fugue, int)) = "----------\nNumber " ++ (show int) ++ "\n" ++ show fugue

