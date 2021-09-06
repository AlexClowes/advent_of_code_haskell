import Data.Hash.MD5
import Data.List
import Data.Maybe


main = do
    let hashes = map (md5s . Str . ("bgvyzdsv"++) . show) [0..]
    print . fromJust . findIndex (isPrefixOf "00000") $ hashes
    print . fromJust . findIndex (isPrefixOf "000000") $ hashes
