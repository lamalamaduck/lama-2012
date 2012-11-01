import Control.Monad
import Data.List

kielioppi = do
    i <- [0..4]
    j <- [0..4]
    guard $ i /= j
    let loppuosalista = keskikohat i j
        loppuosa = intercalate " \\mid " loppuosalista
    return $ "A_{" ++ (show i) ++ (show j) ++ "} & \\to " ++ loppuosa

keskikohat i j = do
    k <- [0..4]
    guard $ i /= k
    guard $ j /= k
    return $ "A_{" ++ (show i) ++ (show k) ++"}" ++
             "A_{" ++ (show k) ++ (show j) ++"}"

