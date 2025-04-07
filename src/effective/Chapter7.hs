{-# LANGUAGE GADTSyntax #-}

module Chapter7 (
    --test
)
where
import Control.Monad (join) 

ioFunc1 :: IO (IO String)
ioFunc1 = (pure . pure) "doggo"

ioFunc2 :: IO (IO String) -> IO String
ioFunc2 = join
--ioFunc2 x = do
--    y <- x
--    z <- y
--    return z

test = "doggo"