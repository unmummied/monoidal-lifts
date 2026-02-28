module Main where

linearF1X0 :: Applicative t => (b1 -> c) ->                          b1                                  -> t c
linearF1X1 :: Applicative t => (b1 -> c) -> (a1                   -> b1) -> t a1                         -> t c
linearF1X2 :: Applicative t => (b1 -> c) -> (a1 -> a2             -> b1) -> t a1 -> t a2                 -> t c
linearF1X3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3       -> b1) -> t a1 -> t a2 -> t a3         -> t c
linearF1X4 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> a4 -> b1) -> t a1 -> t a2 -> t a3 -> t a4 -> t c
linearF1X0 p f1             =                         pure(p                    f1)
linearF1X1 p f1 x1          =                   pure (pure p <*>                f1) <*> x1
linearF1X2 p f1 x1 x2       =             pure (pure (pure p <*>) <*>           f1) <*> x1 <*> x2
linearF1X3 p f1 x1 x2 x3    =       pure (pure (pure (pure p <*>) <*>) <*>      f1) <*> x1 <*> x2 <*> x3
linearF1X4 p f1 x1 x2 x3 x4 = pure (pure (pure (pure (pure p <*>) <*>) <*>) <*> f1) <*> x1 <*> x2 <*> x3 <*> x4

crossF1X0 :: Applicative t => (b1 -> c) ->                    b1                          -> t c
crossF1X1 :: Applicative t => (b1 -> c) -> (a1             -> b1) -> t a1                 -> t c
crossF1X2 :: Applicative t => (b1 -> c) -> (a1 -> a2       -> b1) -> t a1 -> t a2         -> t c
crossF1X3 :: Applicative t => (b1 -> c) -> (a1 -> a2 -> a3 -> b1) -> t a1 -> t a2 -> t a3 -> t c
crossF1X0 p f1          = pure (                  p f1)
crossF1X1 p f1 x1       = pure (             (.)  p f1) <*> x1
crossF1X2 p f1 x1 x2    = pure ((      (.) . (.)) p f1) <*> x1 <*> x2
crossF1X3 p f1 x1 x2 x3 = pure (((.) . (.) . (.)) p f1) <*> x1 <*> x2 <*> x3

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
