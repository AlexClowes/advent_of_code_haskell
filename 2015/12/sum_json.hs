import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as M
import Data.Maybe
import qualified Data.Scientific as S
import qualified Data.Vector as V

flatten :: Value -> [Int]
flatten (Object obj) = concat . map flatten $ M.elems obj
flatten (Array arr) = concat . map flatten $ V.toList arr
flatten (Number num) = [fromJust . S.toBoundedInteger $ num]
flatten _ = []

main = do
    json <- B.readFile "input.json"
    print . sum . flatten . fromJust $ (decode json :: Maybe Value)
