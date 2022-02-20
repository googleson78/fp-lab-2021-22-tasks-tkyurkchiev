module Can where

import Binary (Binary(..), Bit(..))

data LeadingOne = LeadOne | LeadingOne :.: Bit
  deriving Show

canOne :: LeadingOne
canOne = LeadOne

data Can = LeadZero | Expression LeadingOne
  deriving Show

--getLeadingOne :: Can -> Maybe LeadingOne
--getLeadingOne c
--  |c == Zero = Nothing
--  |otherwise = 


canZero :: Can
canZero = LeadZero

snoc :: Can -> Bit -> Can
snoc LeadZero Zero = LeadZero
snoc LeadZero One = Expression LeadOne
snoc (Expression x) y = Expression(x:.:y)


forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne = undefined

forget :: Can -> Binary
forget = undefined

canonicalise :: Binary -> Can
canonicalise = undefined