{-# Language BangPatterns #-}

module Loop where

loop1, loop2 :: Bool -> Int -> Int -> Int

loop1 b 0 acc = if b then acc else 0
loop1 b n acc = loop1 b (n-1) $ acc*2

loop2 b 0 !acc = if b then acc else 0
loop2 b n acc = loop2 b (n-1) $ acc*2
