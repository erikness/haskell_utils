import System.Random

data NoteLetter = A | B | C | D | E | F | G deriving(Eq, Enum, Show)
data NoteSign = Flat | Natural | Sharp deriving(Eq, Enum, Show)

data Note = (NoteLetter, NoteSign)

randomNoteLetter :: StdGen -> (NoteLetter, StdGen)
randomNoteLetter gen = let (randInt, newGen) = randomR (0, 6) gen
                       in (toEnum randInt, newGen)

randomNoteSign :: StdGen -> (NoteSign, StdGen)
randomNoteSign gen = let (randInt, newGen) = randomR (0, 2) gen
                     in (toEnum randInt, newGen)
    
randomNote :: StdGen -> (Note, StdGen)
randomNote gen = let (letter, newGen) = randomNoteLetter gen
                     (sign, _) = randomNoteSign gen
                 in ((letter, sign), newGen)
                 
randomNotes :: StdGen -> [Note]
randomNotes gen = let (note, newGen) = randomNote gen
                  in note:(randomNotes newGen)

-- "succ", and "prev" wrappers to make the sequence circular
-- due to the business logic of music, this has to be explicit

noteEq :: Note -> Note -> Bool
noteEq (B, Sharp) (C, Natural) = True
noteEq (C, Natural) (B, Sharp) = True
noteEq (B, Natural) (C, Flat) = True
noteEq (C, Flat) (B, Natural) = True
noteEq (E, Sharp) (F, Natural) = True
noteEq (F, Natural) (E, Sharp) = True
noteEq (E, Natural) (F, Flat) = True
noteEq (F, Flat) (E, Natural) = True
noteEq (nl, ns) (ml, ms)
    | (ml == (nextNoteLetter nl)) && (ns == Sharp) && (ms == Flat) = True
    | (nl == (nextNoteLetter ml)) && (ms == Sharp) && (ns == Flat) = True
    | otherwise = False

nextNoteLetter :: NoteLetter -> NoteLetter
nextNoteLetter G = A
nextNoteLetter n = succ n

prevNoteLetter :: NoteLetter -> NoteLetter
prevNoteLetter A = G
prevNoteLetter n = prev n


majorScaleGaps = [2,2,1,2,2,2,1]
--majorScale :: Note -> [Note]
--majorScale tonic = 