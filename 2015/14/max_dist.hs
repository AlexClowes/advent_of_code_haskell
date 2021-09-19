import Data.List
import Data.Maybe
import Text.Regex

type Reindeer = (Int, Int, Int)

lineRegex :: Regex
lineRegex = mkRegex "[a-zA-Z]+ can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds."

parseLine :: String -> Reindeer
parseLine line = (flySpeed, flyTime, restTime)
    where [flySpeed, flyTime, restTime] = map read . fromJust $ matchRegex lineRegex line

totalDist :: Int -> Reindeer -> Int
totalDist time (flySpeed, flyTime, restTime) =
    let nCycles = div time (flyTime + restTime)
        leftoverTime = mod time (flyTime + restTime)
    in flySpeed * (nCycles * flyTime + min flyTime leftoverTime)

position :: Reindeer -> [Int]
position (flySpeed, flyTime, restTime) =
    scanl1 (+) . cycle $ (replicate flyTime flySpeed) ++ (replicate restTime 0)

main = do
    input <- readFile "reindeer.txt"
    print . maximum . map ((!! 2503) . position . parseLine) . lines $ input
