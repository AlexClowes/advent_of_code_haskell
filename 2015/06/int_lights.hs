import Data.Array.IO
import Text.Regex

type LightGrid = IOArray (Int, Int) Int

data Instruction = Instruction { operation :: Int -> Int
                               , xmin :: Int
                               , xmax :: Int
                               , ymin :: Int
                               , ymax :: Int
                               }

instructionRegex :: Regex
instructionRegex = mkRegex "(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"

parseInstruction :: String -> Instruction
parseInstruction instruction =
    let Just [optext, xmin, ymin, xmax, ymax] = matchRegex instructionRegex instruction
        op | optext == "turn on" = (\x -> x + 1)
           | optext == "turn off" = (\x -> max 0 (x - 1))
           | optext == "toggle" = (\x -> x + 2)
    in Instruction op (read xmin) (read xmax) (read ymin) (read ymax)

mutateElement :: LightGrid -> (Int -> Int) -> (Int, Int) -> IO ()
mutateElement lights op (i, j) = do
    element <- readArray lights (i, j)
    writeArray lights (i, j) (op element)

applyInstruction :: LightGrid -> Instruction -> IO ()
applyInstruction lights (Instruction op xmin xmax ymin ymax) =
    sequence_ [mutateElement lights op (i, j) | i <- [xmin..xmax], j <- [ymin..ymax]]

main = do
    instructions <- fmap (map parseInstruction . lines) $ readFile "instructions.txt"
    lights <- newArray ((0, 0), (999, 999)) 0 :: IO LightGrid
    sequence_ $ map (applyInstruction lights) instructions
    values <- getElems lights
    print $ sum values
