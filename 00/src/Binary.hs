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
isEnd _ = False


flipBinary :: Binary -> Binary -> Binary
flipBinary End x = x
flipBinary (a :. b) x = flipBinary a (x:. b)


canonicalise :: Binary -> Binary
canonicalise num = flipBinary (canonicalise' $ flipBinary num End) End
  where
    canonicalise' :: Binary -> Binary
    canonicalise' End = End
    canonicalise' (b :. Zero) = canonicalise' b
    canonicalise' (b :. One) = b :. One
    

isZero :: Bit -> Bool
isZero Zero = True
isZero _ = False

addBinary :: Binary -> Binary -> Binary
addBinary a b = flipBinary (adder a b Zero End) End
  where
    adder :: Binary -> Binary -> Bit -> Binary -> Binary
    
    adder End End flag result = if isZero flag then result else result :. One
    
    adder (x :. y) End flag result 
      |isZero flag = adder x End Zero (result :. y)
      |isZero y = adder x End Zero (result :. One)
      |otherwise = adder x End One (result :. Zero)


    adder End (x :. y) flag result
      | isZero flag = adder End x Zero (result :. y)
      | isZero y = adder x End Zero (result :. One)
      | otherwise = adder End x One (result :. Zero)

    adder (x :. Zero) (y :. Zero) flag result = adder x y Zero (result :. flag)

    adder (x :. Zero) (y :. One) flag result = if isZero flag then adder x y Zero (result :. One) else adder x y One (result :. Zero)

    adder (x :. One) (y :. Zero) flag result = if isZero flag then adder x y Zero (result :. One) else adder x y One (result :. Zero)

    adder (x :. One) (y :. One) flag result = adder x y One (result :. flag)

    
    
