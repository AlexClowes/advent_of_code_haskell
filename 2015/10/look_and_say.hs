import Data.Char
import Data.List

lookAndSay :: [Int] -> [Int]
lookAndSay = concat . map (\g -> [length g, head g]) . group

main = do
    let input = map digitToInt "3113322113"
    print . length . (!! 40) $ iterate lookAndSay input
    print . length . (!! 50) $ iterate lookAndSay input
