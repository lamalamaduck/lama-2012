import Control.Monad

subseq start end seq = take (end - start) $ drop start seq

splits str = do
    let len = length str
    i1 <- [0..len]
    i2 <- [i1..len]
    i3 <- [i2..len]
    i4 <- [i3..len]
    let u = subseq 0 i1 str
        v = subseq i1 i2 str
        x = subseq i2 i3 str
        y = subseq i3 i4 str
        z = subseq i4 len str
    guard $ validsplit v x y
    return (u,v,x,y,z)
  where pumpingLength = 4
        validsplit v x y = (length (v ++ x ++ y) <= pumpingLength) &&
                           length (v ++ y) > 0 &&
                           sameAmountOf '1' '0' (v ++ y)
        sameAmountOf c1 c2 str = (countOf c1 str) == (countOf c2 str)
        countOf char str = length $ filter (== char) str
