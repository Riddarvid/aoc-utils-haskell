module Lib (someFun) where

someFun :: IO ()
someFun = putStrLn "Hello" >> putStrLn "GoodBye"
