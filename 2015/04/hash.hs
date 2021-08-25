import Data.Hash.MD5
import Data.List


main = do
    let hashes = map (md5s . Str . ("bgvyzdsv"++) . show) [0..]
    print . findIndex (isPrefixOf "00000") $ hashes
    print . findIndex (isPrefixOf "000000") $ hashes
