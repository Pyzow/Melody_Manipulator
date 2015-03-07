import Haskore

n1 = Note (C, 4) 1 []
n2 = Note (F, 4) 1 []

ourChord = n1 :=: n2
ourMidiFile = testMidi ourChord
main = outputMidiFile "simple_example.mid" ourMidiFile
