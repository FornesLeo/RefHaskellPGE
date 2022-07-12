module Rule where

data Cell = Live | Die
    deriving (Eq)

instance Show Cell where
    show Live = "*"
    show Die = " "
    
showC :: Cell -> Char
showC Live = '*'
showC Die = ' '

type Rule = (Cell, Cell, Cell) -> Cell

rule30 :: (Cell, Cell, Cell) -> Cell
rule30 (Live, Live, Live) = Die
rule30 (Live, Live, Die) = Die
rule30 (Live, Die, Live) = Die
rule30 (Live, Die, Die) = Live
rule30 (Die, Live, Live) = Live
rule30 (Die, Live, Die) = Live
rule30 (Die, Die, Live) = Live
rule30 (Die, Die, Die) = Die

rule90 :: (Cell, Cell, Cell) -> Cell
rule90 (Live, Live, Live) = Die
rule90 (Live, Live, Die) = Live
rule90 (Live, Die, Live) = Die
rule90 (Live, Die, Die) = Live
rule90 (Die, Live, Live) = Live
rule90 (Die, Live, Die) = Die
rule90 (Die, Die, Live) = Live
rule90 (Die, Die, Die) = Die


rule110 :: (Cell, Cell, Cell) -> Cell
rule110 (Live, Live, Live) = Die
rule110 (Live, Live, Die) = Live
rule110 (Live, Die, Live) = Live
rule110 (Live, Die, Die) = Die
rule110 (Die, Live, Live) = Live
rule110 (Die, Live, Die) = Live
rule110 (Die, Die, Live) = Live
rule110 (Die, Die, Die) = Die

setRule :: Int -> Maybe Rule
setRule 30 = Just rule30
setRule 90 = Just rule90
setRule 110 = Just rule110
setRule _ = Nothing


-- Bonus pour gérer toute les rules
-- rule :: Int -> Rule
-- rule x (False,False,False) = isOn 0 x 
-- rule x (False,False,True ) = isOn 1 x
-- rule x (False,True, False) = isOn 2 x
-- rule x (False,True, True ) = isOn 3 x
-- rule x (True, False,False) = isOn 4 x
-- rule x (True, False,True ) = isOn 5 x
-- rule x (True, True, False) = isOn 6 x
-- rule x (True, True, True ) = isOn 7 x

-- isOn :: Int -> Int -> Bool
-- isOn b x = 0x01 .&. shiftR x b == 1