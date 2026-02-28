module Main (main) where

f1x0 :: Applicative t => (b1 -> c) ->                    b1                          -> t c
f1x1 :: Applicative t => (b1 -> c) -> (a1             -> b1) -> t a1                 -> t c
f1x2 :: Applicative t => (b1 -> c) -> (a1 -> a2       -> b1) -> t a1 -> t a2         -> t c
f1x3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> b1) -> t a1 -> t a2 -> t a3 -> t c
f1x0 p f1          = pure (                  p f1)
f1x1 p f1 x1       = pure (             (.)  p f1) <*> x1               -- == pure p <*> (pure f1 <*> x1)
f1x2 p f1 x1 x2    = pure ((      (.) . (.)) p f1) <*> x1 <*> x2
f1x3 p f1 x1 x2 x3 = pure (((.) . (.) . (.)) p f1) <*> x1 <*> x2 <*> x3

f2x0 :: Applicative t => (b1 -> b2 -> c)                    -> b1                     -> b2                          -> t c
f2x1 :: Applicative t => (b1 -> b2 -> c) -> (a1             -> b1) -> (a1             -> b2) -> t a1                 -> t c
f2x2 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> t a1 -> t a2         -> t c
f2x3 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> t a1 -> t a2 -> t a3 -> t c
f2x0 p f1 f2          = f1x0 p f1          <*> f1x0 id f2          -- = ap0'0 (p f1) f2
f2x1 p f1 f2 x1       = f1x1 p f1 x1       <*> f1x1 id f2 x1
f2x2 p f1 f2 x1 x2    = f1x2 p f1 x1 x2    <*> f1x2 id f2 x1 x2
f2x3 p f1 f2 x1 x2 x3 = f1x3 p f1 x1 x2 x3 <*> f1x3 id f2 x1 x2 x3

f3x0 :: Applicative t => (b1 -> b2 -> b3 -> c)                    -> b1                     -> b2                     -> b3                          -> t c
f3x1 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> t a1                 -> t c
f3x2 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> t a1 -> t a2         -> t c
f3x3 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> t a1 -> t a2 -> t a3 -> t c
f3x0 p f1 f2 f3          = f2x0 p f1 f2          <*> f1x0 id f3
f3x1 p f1 f2 f3 x1       = f2x1 p f1 f2 x1       <*> f1x1 id f3 x1
f3x2 p f1 f2 f3 x1 x2    = f2x2 p f1 f2 x1 x2    <*> f1x2 id f3 x1 x2
f3x3 p f1 f2 f3 x1 x2 x3 = f2x3 p f1 f2 x1 x2 x3 <*> f1x3 id f3 x1 x2 x3

f4x0 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c)                    -> b1                     -> b2                     -> b3                     -> b4                          -> t c
f4x1 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> (a1             -> b4) -> t a1                 -> t c
f4x2 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> (a1 -> a2       -> b4) -> t a1 -> t a2         -> t c
f4x3 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> (a1 -> a2 -> a3 -> b4) -> t a1 -> t a2 -> t a3 -> t c
f4x0 p f1 f2 f3 f4          = f3x0 p f1 f2 f3          <*> f1x0 id f4
f4x1 p f1 f2 f3 f4 x1       = f3x1 p f1 f2 f3 x1       <*> f1x1 id f4 x1
f4x2 p f1 f2 f3 f4 x1 x2    = f3x2 p f1 f2 f3 x1 x2    <*> f1x2 id f4 x1 x2
f4x3 p f1 f2 f3 f4 x1 x2 x3 = f3x3 p f1 f2 f3 x1 x2 x3 <*> f1x3 id f4 x1 x2 x3

main :: IO ()
main = putStrLn "Hello, Haskell!"
