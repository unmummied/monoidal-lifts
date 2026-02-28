module Main where

import Control.Applicative (liftA3)

liftA4 :: Applicative t => (a -> b -> c -> d -> e) -> t a -> t b -> t c -> t d -> t e
liftA4 x y z w v = liftA3 x y z w <*> v

f0X0 :: Applicative t => c -> t c
f0X1 :: Applicative t => c -> t a1 -> t c
f0X2 :: Applicative t => c -> t a1 -> t a2 -> t c
f0X3 :: Applicative t => c -> t a1 -> t a2 -> t a3 -> t c
f0X0 p          = pure p
f0X1 p x1       = f0X0 (pure p) <*> x1
f0X2 p x1 x2    = f0X1 (pure p)     x1 <*> x2
f0X3 p x1 x2 x3 = f0X2 (pure p)     x1     x2 <*> x3

f1X0 :: Applicative t => (b1 -> c) ->                    b1                          -> t c
f1X1 :: Applicative t => (b1 -> c) -> (a1             -> b1) -> t a1                 -> t c
f1X2 :: Applicative t => (b1 -> c) -> (a1 -> a2       -> b1) -> t a1 -> t a2         -> t c
f1X3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> b1) -> t a1 -> t a2 -> t a3 -> t c
f1X0 p f1          = f0X0 (p f1)
f1X1 p f1 x1       = f1X0 (fmap p) f1 <*> x1
f1X2 p f1 x1 x2    = f1X1 (fmap p) f1     x1 <*> x2
f1X3 p f1 x1 x2 x3 = f1X2 (fmap p) f1     x1     x2 <*> x3

f2X0 :: Applicative t => (b1 -> b2 -> c)                    -> b1                     -> b2                          -> t c
f2X1 :: Applicative t => (b1 -> b2 -> c) -> (a1             -> b1) -> (a1             -> b2) -> t a1                 -> t c
f2X2 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> t a1 -> t a2         -> t c
f2X3 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> t a1 -> t a2 -> t a3 -> t c
f2X0 p f1 f2          = f1X0 (p f1) f2
f2X1 p f1 f2 x1       = f2X0 (liftA2 p) f1 f2 <*> x1
f2X2 p f1 f2 x1 x2    = f2X1 (liftA2 p) f1 f2     x1 <*> x2
f2X3 p f1 f2 x1 x2 x3 = f2X2 (liftA2 p) f1 f2     x1     x2 <*> x3

f3X0 :: Applicative t => (b1 -> b2 -> b3 -> c)                    -> b1                     -> b2                     -> b3                          -> t c
f3X1 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> t a1                 -> t c
f3X2 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> t a1 -> t a2         -> t c
f3X3 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> t a1 -> t a2 -> t a3 -> t c
f3X0 p f1 f2 f3          = f2X0 (p f1) f2 f3
f3X1 p f1 f2 f3 x1       = f3X0 (liftA3 p) f1 f2 f3 <*> x1
f3X2 p f1 f2 f3 x1 x2    = f3X1 (liftA3 p) f1 f2 f3     x1 <*> x2
f3X3 p f1 f2 f3 x1 x2 x3 = f3X2 (liftA3 p) f1 f2 f3     x1     x2 <*> x3

f4X0 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c)                    -> b1                     -> b2                     -> b3                     -> b4                          -> t c
f4X1 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> (a1             -> b4) -> t a1                 -> t c
f4X2 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> (a1 -> a2       -> b4) -> t a1 -> t a2         -> t c
f4X3 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> (a1 -> a2 -> a3 -> b4) -> t a1 -> t a2 -> t a3 -> t c
f4X0 p f1 f2 f3 f4          = f3X0 (p f1) f2 f3 f4
f4X1 p f1 f2 f3 f4 x1       = f4X0 (liftA4 p) f1 f2 f3 f4 <*> x1
f4X2 p f1 f2 f3 f4 x1 x2    = f4X1 (liftA4 p) f1 f2 f3 f4     x1 <*> x2
f4X3 p f1 f2 f3 f4 x1 x2 x3 = f4X2 (liftA4 p) f1 f2 f3 f4     x1     x2 <*> x3


crossF0X0 :: Applicative t => c                         -> t c
crossF0X1 :: Applicative t => c -> t a1                 -> t c
crossF0X2 :: Applicative t => c -> t a1 -> t a2         -> t c
crossF0X3 :: Applicative t => c -> t a1 -> t a2 -> t a3 -> t c
crossF0X0 p          = pure p
crossF0X1 p x1       = crossF0X0 (const p) <*> x1
crossF0X2 p x1 x2    = crossF0X1 (const p) x1 <*> x2
crossF0X3 p x1 x2 x3 = crossF0X2 (const p) x1 x2 <*> x3

crossF1X0 :: Applicative t => (b1 -> c) ->                    b1                          -> t c
crossF1X1 :: Applicative t => (b1 -> c) -> (a1             -> b1) -> t a1                 -> t c
crossF1X2 :: Applicative t => (b1 -> c) -> (a1 -> a2       -> b1) -> t a1 -> t a2         -> t c
crossF1X3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> b1) -> t a1 -> t a2 -> t a3 -> t c
crossF1X0 p f1          = pure (p f1)
crossF1X1 p f1 x1       = crossF1X0 (p .) f1 <*> x1
crossF1X2 p f1 x1 x2    = crossF1X1 (p .) f1 x1 <*> x2
crossF1X3 p f1 x1 x2 x3 = crossF1X2 (p .) f1 x1 x2 <*> x3

crossF2X0 :: Applicative t => (b1 -> b2 -> c)                    -> b1                     -> b2                          -> t c
crossF2X1 :: Applicative t => (b1 -> b2 -> c) -> (a1             -> b1) -> (a1             -> b2) -> t a1                 -> t c
crossF2X2 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> t a1 -> t a2         -> t c
crossF2X3 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> t a1 -> t a2 -> t a3 -> t c
crossF2X0 p f1 f2          = crossF1X0 p f1          <*> crossF1X0 id f2
crossF2X1 p f1 f2 x1       = crossF1X1 p f1 x1       <*> crossF1X1 id f2 x1
crossF2X2 p f1 f2 x1 x2    = crossF1X2 p f1 x1 x2    <*> crossF1X2 id f2 x1 x2
crossF2X3 p f1 f2 x1 x2 x3 = crossF1X3 p f1 x1 x2 x3 <*> crossF1X3 id f2 x1 x2 x3

crossF3X0 :: Applicative t => (b1 -> b2 -> b3 -> c)                    -> b1                     -> b2                     -> b3                          -> t c
crossF3X1 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> t a1                 -> t c
crossF3X2 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> t a1 -> t a2         -> t c
crossF3X3 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> t a1 -> t a2 -> t a3 -> t c
crossF3X0 p f1 f2 f3          = crossF2X0 p f1 f2          <*> crossF1X0 id f3
crossF3X1 p f1 f2 f3 x1       = crossF2X1 p f1 f2 x1       <*> crossF1X1 id f3 x1
crossF3X2 p f1 f2 f3 x1 x2    = crossF2X2 p f1 f2 x1 x2    <*> crossF1X2 id f3 x1 x2
crossF3X3 p f1 f2 f3 x1 x2 x3 = crossF2X3 p f1 f2 x1 x2 x3 <*> crossF1X3 id f3 x1 x2 x3

crossF4X0 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c)                    -> b1                     -> b2                     -> b3                     -> b4                          -> t c
crossF4X1 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> (a1             -> b4) -> t a1                 -> t c
crossF4X2 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> (a1 -> a2       -> b4) -> t a1 -> t a2         -> t c
crossF4X3 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> (a1 -> a2 -> a3 -> b4) -> t a1 -> t a2 -> t a3 -> t c
crossF4X0 p f1 f2 f3 f4          = crossF3X0 p f1 f2 f3          <*> crossF1X0 id f4
crossF4X1 p f1 f2 f3 f4 x1       = crossF3X1 p f1 f2 f3 x1       <*> crossF1X1 id f4 x1
crossF4X2 p f1 f2 f3 f4 x1 x2    = crossF3X2 p f1 f2 f3 x1 x2    <*> crossF1X2 id f4 x1 x2
crossF4X3 p f1 f2 f3 f4 x1 x2 x3 = crossF3X3 p f1 f2 f3 x1 x2 x3 <*> crossF1X3 id f4 x1 x2 x3

main :: IO ()
main = putStrLn "Hello, Haskell!"
