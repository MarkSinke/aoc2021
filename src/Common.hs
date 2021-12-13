module Common (readInt, split, chunks, toCoord)
where

readInt :: String -> Int
readInt = read

split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : split delimiter remain

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

toCoord :: String -> (Int, Int)
toCoord str =
  let coords = split ',' str
  in (readInt(head coords), readInt(head(tail coords)))

