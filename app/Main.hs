module Main where
import qualified Streams as S

stream :: S.Stream Int
stream = S.iterate (2 *) 1

-- stream' = S.map (\x -> x - 1) stream

main :: IO ()
main = print ( S.take stream 10 )