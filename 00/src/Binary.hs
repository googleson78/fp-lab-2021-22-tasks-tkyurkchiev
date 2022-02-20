module Binary where

data Binary = End | Binary :. Bit
  deriving Show

data Bit = Zero | One
  deriving Show

infixl 6 :.



succBinary :: Binary -> Binary
succBinary End = End :. One
succBinary (x :. Zero) = x :. One
succBinary (x :. One) = succBinary x :. Zero


integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary n = integerToBinary (n `div` 2) :. (if even n then Zero else One)


binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (x :. Zero) = 2 * binaryToInteger x
binaryToInteger (x :. One) = 2 * binaryToInteger x + 1


hasLeadingZero :: Binary -> Bool
hasLeadingZero End = False
hasLeadingZero (End :. Zero) = True
hasLeadingZero (End :. One) = False
hasLeadingZero (x :. _) = hasLeadingZero x


isEnd :: Binary -> Bool
isEnd End = True
isEnd (x :. Zero) = isEnd x
isEnd _ = False


canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise (x :. One) = canonicalise x :. One
canonicalise (x :. Zero) = if isEnd x then End else canonicalise x :. Zero


isZero :: Bit -> Bool
isZero Zero = True
isZero _ = False


addBinary :: Binary -> Binary -> Binary
addBinary End End = End
addBinary x End = x
addBinary End x = x
addBinary (x:.Zero) (y:.One) = addBinary x y :. One
addBinary (x:.Zero) (y:.Zero) =  addBinary x y :. Zero
addBinary (x:.One) (y:.Zero) =  addBinary x y :. One
addBinary (x:.One) (y:.One) =  addBinary x y :. One :. Zero
