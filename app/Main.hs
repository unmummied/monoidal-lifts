module Main (main) where

ap0'0 :: Applicative t => (b1 -> c) ->                    b1                          -> t c
ap0'1 :: Applicative t => (b1 -> c) -> (a1             -> b1) -> t a1                 -> t c
ap0'2 :: Applicative t => (b1 -> c) -> (a1 -> a2       -> b1) -> t a1 -> t a2         -> t c
ap0'3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> b1) -> t a1 -> t a2 -> t a3 -> t c
ap0'0 p f1          = pure (                  p f1)
ap0'1 p f1 x1       = pure (             (.)  p f1) <*> x1
ap0'2 p f1 x1 x2    = pure ((      (.) . (.)) p f1) <*> x1 <*> x2
ap0'3 p f1 x1 x2 x3 = pure (((.) . (.) . (.)) p f1) <*> x1 <*> x2 <*> x3

ap1'0 :: Applicative t => (b1 -> b2 -> c)                    -> b1                     -> b2                          -> t c
ap1'1 :: Applicative t => (b1 -> b2 -> c) -> (a1             -> b1) -> (a1             -> b2) -> t a1                 -> t c
ap1'2 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> t a1 -> t a2         -> t c
ap1'3 :: Applicative t => (b1 -> b2 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> t a1 -> t a2 -> t a3 -> t c
ap1'0 p f1 f2          = ap0'0 p f1          <*> ap0'0 id f2          -- = ap0'0 (p f1) f2
ap1'1 p f1 f2 x1       = ap0'1 p f1 x1       <*> ap0'1 id f2 x1
ap1'2 p f1 f2 x1 x2    = ap0'2 p f1 x1 x2    <*> ap0'2 id f2 x1 x2
ap1'3 p f1 f2 x1 x2 x3 = ap0'3 p f1 x1 x2 x3 <*> ap0'3 id f2 x1 x2 x3

ap2'0 :: Applicative t => (b1 -> b2 -> b3 -> c)                    -> b1                     -> b2                     -> b3                          -> t c
ap2'1 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> t a1                 -> t c
ap2'2 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> t a1 -> t a2         -> t c
ap2'3 :: Applicative t => (b1 -> b2 -> b3 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> t a1 -> t a2 -> t a3 -> t c
ap2'0 p f1 f2 f3          = ap1'0 p f1 f2          <*> ap0'0 id f3
ap2'1 p f1 f2 f3 x1       = ap1'1 p f1 f2 x1       <*> ap0'1 id f3 x1
ap2'2 p f1 f2 f3 x1 x2    = ap1'2 p f1 f2 x1 x2    <*> ap0'2 id f3 x1 x2
ap2'3 p f1 f2 f3 x1 x2 x3 = ap1'3 p f1 f2 x1 x2 x3 <*> ap0'3 id f3 x1 x2 x3

ap3'0 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c)                    -> b1                     -> b2                     -> b3                     -> b4                          -> t c
ap3'1 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1             -> b1) -> (a1             -> b2) -> (a1             -> b3) -> (a1             -> b4) -> t a1                 -> t c
ap3'2 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2       -> b1) -> (a1 -> a2       -> b2) -> (a1 -> a2       -> b3) -> (a1 -> a2       -> b4) -> t a1 -> t a2         -> t c
ap3'3 :: Applicative t => (b1 -> b2 -> b3 -> b4 -> c) -> (a1 -> a2 -> a3 -> b1) -> (a1 -> a2 -> a3 -> b2) -> (a1 -> a2 -> a3 -> b3) -> (a1 -> a2 -> a3 -> b4) -> t a1 -> t a2 -> t a3 -> t c
ap3'0 p f1 f2 f3 f4          = ap2'0 p f1 f2 f3          <*> ap0'0 id f4
ap3'1 p f1 f2 f3 f4 x1       = ap2'1 p f1 f2 f3 x1       <*> ap0'1 id f4 x1
ap3'2 p f1 f2 f3 f4 x1 x2    = ap2'2 p f1 f2 f3 x1 x2    <*> ap0'2 id f4 x1 x2
ap3'3 p f1 f2 f3 f4 x1 x2 x3 = ap2'3 p f1 f2 f3 x1 x2 x3 <*> ap0'3 id f4 x1 x2 x3

main :: IO ()
main = putStrLn "Hello, Haskell!"
