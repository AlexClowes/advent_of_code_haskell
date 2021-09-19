import Data.List
import Data.Maybe
import Text.Regex

type Reindeer = (Int, Int, Int)

lineRegex :: Regex
lineRegex = mkRegex "[a-zA-Z]+ can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds."

parseLine :: String -> Reindeer
parseLine line = (flySpeed, flyTime, restTime)
    where [flySpeed, flyTime, restTime] = map read . fromJust $ matchRegex lineRegex line

position :: Reindeer -> [Int]
position (flySpeed, flyTime, restTime) =
    scanl1 (+) . cycle $ (replicate flyTime flySpeed) ++ (replicate restTime 0)

points :: [[Int]] -> [[Int]]
points positions =
    scanl updatePoints (replicate (length positions) 0) (transpose positions) where
    updatePoints points positions =
        let leadPos = maximum positions
        in map (\(pts, pos) -> if pos == leadPos then pts + 1 else pts) $ zip points positions

main = do
    input <- readFile "reindeer.txt"
    print . maximum . (!! 2503) . points . map (position . parseLine) . lines $ input
