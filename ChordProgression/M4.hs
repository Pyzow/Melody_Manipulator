module M4 where

import FugueGen8
import System.Random hiding (split)
import Data.Char
import Text.ParserCombinators.ReadP
--import Data.List.Split

type Chunk = (String, Int)
type Year = Int
type Meter = Int
newtype M = M {
    unpack :: [[String]] 
}

parser = readP_to_S $ parse where 
        
        checkyear = do
            skipSpaces 
            string "year"
            skipSpaces
            munch (/= ')') 
        specialNestedParser i = do
            beginParen i  <++ endParen i 
        beginParen i = do
            char '('
            yearOrNot <- checkyear <++ lookie
            munch (noneOf [')','('])
            more <- parenSearch (i+1)
            return (yearOrNot : more )
        endParen 1 = do 
            char ')'
            parenSearch 0 
        endParen i = do 
            char ')'
            munch (noneOf [')','('])
            parenSearch (i-1)
        lookie = do 
            look
            return [] 
        parenSearch 0 = do
            skipSpaces
            beginParen 0 <++ lookie
        parenSearch i = do
            skipSpaces
            specialNestedParser i 
          
        parseChord = do 
            skipSpaces            
            munch (noneOf ['|','\r','\n',' '])
        parseMeasure   = parseChord `sepBy` char ' ' 
        newline  = string "|"        
        parseProgression = parseMeasure `endBy` newline
        parse  = do
            year <- parenSearch 0
            answer <- parseProgression
            munch (const True)
            return ([concat year] : answer)
        
        noneOf :: [Char] -> Char -> Bool   
        noneOf []     _ = True
        noneOf (c:cs) a = (c /= a) && noneOf cs a 
        -- noneOf is a copy of one supplied in Parsec, which succeeds if the consumed character is in the given list




parseAndFilterYearsAndKey :: Year -> [String] -> [(String,[[String]])] 
parseAndFilterYearsAndKey year strings = sortOutYear year $ zip keys chords
    where 
    chords = map parseM strings
    keys = map (fst . last . (readP_to_S parse4Key)) strings

    parseM :: String -> [[String]]
    parseM string = fst $ last (parser string)

    sortOutYear :: Year -> [(String,[[String]])] -> [(String,[[String]])]
    sortOutYear year [] = []
    sortOutYear year (c:chordchart)
        | year >= read (head (head (snd c))) = c:next
        | otherwise      = next
        where next = sortOutYear year chordchart

    parse4Key = do 
    getKey <++ gettie 
    where gettie = do 
            g <- satisfy (const True) 
            parse4Key
          getKey = do
            string "songkey "
            songkey <- munch (/= ')')
            munch (const True)
            return songkey 
     
    
cleanUp (key,chords) = (key,result)
    where     
    result = consolidateSlashes $ concat $ map (addTimeInfo 4) $ mapRemoveBlanks $ chords

    mapRemoveBlanks ::  [[String]] -> [[String]]
    mapRemoveBlanks = map removeBlanks 
        where
        removeBlanks [] = []
        removeBlanks (a:as)
            | a == ""   = removeBlanks as
            | otherwise = a:(removeBlanks as)    

    addTimeInfo :: Meter -> [String] -> [Chunk]   --the first Int is the meter of the song, the [String] are the chords
    addTimeInfo _ [] = [([],0)] --unnecessary?
    addTimeInfo meter [a] = [(a,meter)]
    addTimeInfo _ (a:[b]) = [(a,2),(b,2)]
    addTimeInfo _ a = [(c,1) | c <- a]

    consolidateSlashes :: [Chunk] -> [Chunk] 
    consolidateSlashes chords = reverse $ goThroughChords 0 (reverse chords)
    
    --In goThroughChords, the Int is a counter that increases for every '/', so that when a chord is processed it gets assigned a duration that includes slashes.  
    goThroughChords :: Int -> [Chunk] -> [Chunk]
    goThroughChords _ [] = [] 
    goThroughChords i ((b,c):as) 
        | b == "/"   = goThroughChords (i+c) as
        | otherwise  = (b,i+c):(goThroughChords 0 as)
    

chartTranspose :: (String, [Chunk]) -> [Chunk]
chartTranspose (key, chunks) = chartTranspose2 shift chunks
    where 
    shift = 48 - (n2i . str2note) key

    chartTranspose2 :: Int -> [Chunk] -> [Chunk]
    chartTranspose2 _ [] = []
    chartTranspose2 shift ((chord, length):chunks)
        | isDigit hc || not (elem hc alltheNotes) = (chord,length):next 
        | otherwise                       = (transposeChord shift chord, length):next
        where next = chartTranspose2 shift chunks
              alltheNotes = ['a'..'g'] ++ ['A'..'G']
              hc = head chord

    transposeChord :: Int -> String -> String
    transposeChord shift [c] = b
            where Notei b _ = i2n (  modulate ((n2i . str2note) [c]    ) shift )
    transposeChord shift f@(c:(d:chars))  
            | elem '/' f       = concat $ strip (head e) : (["/"] ++ [strip (last e)])
            | elem d ['b','#'] = strip $ a ++ chars
            | otherwise        = strip $ b ++ (d:chars)
            where Notei a _ = i2n (  modulate ((n2i . str2note) (c:[d])) shift )
                  Notei b _ = i2n (  modulate ((n2i . str2note) [c]    ) shift )
                  e = map (transposeChord shift) $ splitOn '/' f
    
    -- transposeChord deals with different cases.  In the first case, the note is a single character.  The next case is if we're dealing with a polychord or a chord with a non-root bass note.  The next deals with chords with roots that are a black note (expressed with either # or b), and the last deals with chords with roots that are a single digit but have tonality.  The necessity of these cases is that chords can be any number of characters in length, and we only want to transpose the note, and nothing else.     

    splitOn :: (Eq a) => a -> [a] -> [[a]]
    splitOn a b = splitOn2 a [] b

    splitOn2 :: (Eq a) => a -> [a] -> [a] -> [[a]] 
    splitOn2 a b [] = [b,[]]
    splitOn2 a b (c:cs)
        | a == c = [b,cs]
        | otherwise = splitOn2 a (b++[c]) cs

    --these splitOn functions deal with transposing poly chords and chords with different notes in the bass.  It basically goes through and finds the chord before the slash and the chord/note after the slash and puts them into a two element list.  

weight :: [[Chunk]] -> [[Chunk]]
weight [] = []
weight (c:chart) = replicate frequency c ++ weight chart
    where frequency = year - 1919 :: Int
          year = read $ fst $ head c 


makeBigCollection :: [[Chunk]] -> [(Chunk, Chunk)]
makeBigCollection chordCharts = concat $ map breakupWithBeginning chordCharts
    where 
    breakupWithBeginning :: [Chunk] -> [(Chunk, Chunk)]
    breakupWithBeginning = breakup . noteBeginning

    breakup :: [Chunk] -> [(Chunk, Chunk)]
    breakup [] = []  
    breakup [c] = [(c,("End ", 0))]
    breakup (c:chunks) = (c, head chunks):(breakup chunks)

    noteBeginning :: [Chunk] -> [Chunk]
    noteBeginning cs = ("Strt",0):cs
    

generateChartBegin :: RandomGen b => [(Chunk, Chunk)] -> b -> [Chunk]
generateChartBegin pairs gen = generateChart pairs a b
    where 
    (a,b) = randomSelect2 beginningPairs gen
    beginningPairs = [ d | (c,d) <- pairs, c == ("Strt",0) ]

    generateChart :: RandomGen b => [(Chunk, Chunk)] -> Chunk -> b -> [Chunk]
    generateChart pairs c gen
        | c == ("Strt",0) = next   
        | c == ("End ",0) = []
        | otherwise       = c:next
        where   next  = generateChart pairs e gen'
                (e,gen') = randomSelect2 [ b | (a,b) <- pairs, a == c ] gen

    randomSelect2 pairs int = (pairs !! a, b)
        where (a,b) = randomR (0, length pairs -1) int


reconvert :: Int -> [Chunk] -> [String]
reconvert meter = (intersperseEveryMeasure meter 4) . ready
    where
    ready :: [Chunk] -> [String]
    ready [] = []
    ready (c:cs) = ([a ++ " "] ++ (replicate (b-1) "/ ")) ++ ready cs 
        where   (a,b) = c 

    intersperseEveryMeasure :: Int -> Int -> [String] -> [String]
    intersperseEveryMeasure meter 0 list = "\n": intersperseEveryMeasure meter 4 list
    intersperseEveryMeasure meter count list
        | l < (meter +1)     = list ++ (replicate (meter - l) "/ ") ++ ["|"] 
        | otherwise = cleanSlashes meter list ++ intersperseEveryMeasure meter (count-1) (drop meter list) 
        where l = length list

    cleanSlashes meter list = case measure of 
        [c,"/ ","/ ","/ "] -> [c,  "| "]
        [c,"/ ","/ "]      -> [c,  "| "]
        [c,"/ ", d  ,"/ "] -> case c of 
                                    "/ " -> [c,"/ ", d  ,"/ ","| "]
                                    _    -> [c,d,"| "]
        _ -> measure ++ ["| "]
        where measure = take meter list 


-----------------------------------------------

pin = randomR (0::Int,6::Int) (mkStdGen 334)
inn = randomR (0::Int,6::Int) (snd pin)
the = randomR (0::Int,6::Int) (snd inn)
sky = randomR (0::Int,6::Int) (snd the) 

sample :: [Chunk]
sample = [("Dmaj7",4),("B7",4),("Em7",4),("A7",4),("Dmaj7",4),("Am7",4),("Dm7",4),("Gmaj7",8),("Gbm7",4),("B7",4),("Em7",4),("A7",4),("Dmaj7",8)]


samplesheet = "(title Am I That Easy To Forget?) (composer Carl Belew and W.S. Stevenson) (year 1963) (songkey Eb)   (type chords)    (title )    (composer )    (instrument 0)  (  (volume 65)   ( (key -3) )  ) (section style ballad) Eb | Eb7 | Ab | / | Eb | / | Bb7 | / | Eb | Eb7 | Ab | / | Eb | Bb7 | Eb Ab | Eb Eb13 | Ab | / | Eb | / / Bbm7 Eb7 | Ab | Bb7 | Eb | / | asdf asdf a f"

sampleBetty = "(title Along Came Betty) (composer Benny Golson)(year 1958) (?)(comments )(meter 4 4)(key 0)(songkey Ab) Bbm7 | Bm7 E7 F | Bbm7 | Bm7 E7 | AM7 | Ab7 | GM7 | F#7 | F#m7 | Gm7 C7 | F#m7 | Gm7 C7 | FM7 | A7 | Dm7 | G7 | Cm7 | F7 | Am7b5 D7 | Gm7 | Em7b5 | A7 | Fm7 | Bb7 | Bbm7 | Bm7 E7 | Bbm7 | Bm7 E7 | Cm7b5 | F7 | Bbm7 | Eb7 | AbM7 | Bm7 E7 |"

