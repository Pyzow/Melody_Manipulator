everyOther :: [a] -> [a]
everyOther [] = []
everyOther (n:ns) = n : (tail everyOther ns)

findChord :: MelodyNote -> [Chord] -> [(Chord, Int, MelodyNote)] 
findChord i melody chords = [ (c,i,m) | c <- chords, notesBelong (take i melody)) c == True, m == takeAway i melody ] 

notesBelong :: MelodyNote -> Chord -> Bool
notesBelong (m:ms) chord = elem m chord && notesBelong ms chord
    
findChord2 j melody chords
    | j = 0 == []
    | findChord melody chords j == [] = findChord melody chords j-1

findChord3 = findChord2 8

takeAway :: [a] -> Int -> [a]
takeAway [] _ = []
takeAway _ 0 = []
takeAway list i = takeAway (tail list) (i-1)

allChords :: MelodyNote -> [Chords] -> [[(Chords,Int)]]
allChords melody chords = [ [c,i] | a <- findChord3 melody chords) 

keepFinding :: MelodyNote -> [(Chord, Int)]
keepFinding [] _ = []
keepFinding melody ichords =
    [ (x,y):(keepFinding z ichords) | (x,y,z) <- findChord3 melody ichords ]
