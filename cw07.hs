import cw06

data Nat = Zero | Suc Nat deriving (Eq, Ord)
data Z = Pos Nat | Neg Nat

instance Eq Z where
    (==) (Pos Zero) (Neg Zero) = True
    (==) (Neg Zero) (Pos Zero) = True
    (==) (Pos Zero) (Pos Zero) = True
    (==) (Neg x)    (Neg y)    = x == y
    (==) (Pos x)    (Pos y)    = x == y
    (==) _          _          = False

instance Ord Z where
    compare (Neg _) (Pos _) = LT
    compare (Pos _) (Neg _) = GT
    compare (Pos x) (Pos y) = compare x y
    compare (Neg x) (Neg y) = compare y x

