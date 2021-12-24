module Lib
    ( someFunc
    ) where
import Nat(Nat, beside)

someFunc :: IO ()
someFunc = print $ beside 1 2
