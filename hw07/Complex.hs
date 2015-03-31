module Complex where

import Data.List.Split

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) a b = Complex (real a + real b) (im a + im b)
    (*) a b = Complex (real a * real b - im a * im b) (real a * im b + im a * real b)
    fromInteger x = Complex (fromInteger x) 0.0
    negate a = Complex (-real a) (-im a)
    abs a = Complex (sqrt ((real a) ^ 2 + (im a) ^ 2)) 0.0
    
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) a b = Complex newReal newIm
        where
            newReal = (real a * real b + im a * im b) / denominator
            newIm = (im a * real b - real a * im b) / denominator
            denominator =((real b) ^ 2 + (im b) ^ 2)
    fromRational a = Complex (fromRational a) 0.0

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    show a | rep == 0.0 && imp == 0.0 = "0.0"
           | rep == 0.0 = onlyIm
           | imp == 0.0 = show rep
           | imp <  0.0 = show rep ++ " - " ++ show (-imp) ++ " * i"
           | imp >  0.0 = show rep ++ " + " ++ show imp ++ " * i"
        where
            rep = real a
            imp = im a
            onlyIm = show imp ++ " * i" 

instance Read Complex where
    readsPrec _ str = case parts of [rep] -> [(Complex (read rep) 0.0, "")]
                                    [imp, "*", "i"] -> [(Complex 0.0 (read imp), "")]
                                    [rep, "+", imp, "*", "i"] -> [(Complex (read rep) (read imp), "")]
                                    [rep, "-", imp, "*", "i"] -> [(Complex (read rep) (-(read imp)), "")]
        where
            parts = splitOn " " str

i :: Complex
i = Complex 0.0 (1.0)
