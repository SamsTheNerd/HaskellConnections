
module Styling where


-- formats a color and style as an ascii style code
format :: Integer -> Integer -> String
format sty c = "\ESC" ++ "[" ++ show sty ++ ";" ++ show c ++ "m"

-- returns an ascii style code for the given color
fColor :: Integer -> String
fColor = format fBright

-- formatting codes yay
fRed :: Integer
fRed = 31
fGreen :: Integer
fGreen = 32
fYellow :: Integer
fYellow = 33
fBlue :: Integer
fBlue = 34
fPurple :: Integer
fPurple = 35
fCyan :: Integer
fCyan = 36
fWhite :: Integer
fWhite = 37

fNorm :: Integer
fNorm = 0
fBright :: Integer
fBright = 1
fBold :: Integer
fBold = 3

-- resets all formatting.
fReset :: String
fReset = "\ESC[0m"

-- used for referring to different parts of a border

-- selects the horizontal border component
sL :: (a,b,c) -> a
sL (x, _, _) = x
sC :: (a,b,c) -> b
sC (_, x, _) = x
sR :: (a, b, c) -> c
sR (_, _, x) = x

-- selects the vertical border component (these are the same functions just with a different name for better readability)
sU :: (a,b,c) -> a
sU = sL
sM :: (a,b,c) -> b
sM = sC
sB :: (a, b, c) -> c
sB = sR

-- represents a box border style.
type Border = ((Char, Char, Char), (Char, Char, Char), (Char, Char, Char))
-- single line box chars
normBorder :: Border
normBorder = (('┌', '─', '┐'), ('│', ' ', '│'), ('└', '─', '┘'))
-- double line box chars
selBorder :: Border
selBorder = (('╔', '═', '╗'), ('║', ' ', '║'), ('╚', '═', '╝'))
