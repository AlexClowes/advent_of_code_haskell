import Data.Array
import Text.Regex

data Instruction = Instruction { operation :: Int -> Int
                               , xmin :: Int
                               , xmax :: Int
                               , ymin :: Int
                               , ymax :: Int
                               }

type LightGrid = Array (Int, Int) Int

blankLightGrid :: LightGrid
blankLightGrid = array ((0, 0), (999, 999)) [((i, j), 0) | i <- [0..999], j <- [0..999]]

instructionRegex :: Regex
instructionRegex = mkRegex "(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"

parseInstruction :: String -> Instruction
parseInstruction instruction =
    let Just [optext, xmin, ymin, xmax, ymax] = matchRegex instructionRegex instruction
        op | optext == "turn on" = (\x -> x + 1)
           | optext == "turn off" = (\x -> max 0 (x - 1))
           | optext == "toggle" = (\x -> x + 2)
    in Instruction op (read xmin) (read xmax) (read ymin) (read ymax)

applyInstruction :: LightGrid -> Instruction -> LightGrid
applyInstruction array (Instruction op xmin xmax ymin ymax) =
    array // [((i, j), op (array ! (i, j))) | i <- [xmin..xmax], j <- [ymin..ymax]]

main = do
    instructions <- readFile "instructions.txt"
    print . sum . foldl applyInstruction blankLightGrid . map parseInstruction $ lines instructions
