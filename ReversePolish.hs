type MorseCode = String
test = "-- --- .-. ... .    -.-. --- -.. ."

data Tree a = Leaf
            | Branch { tag :: a, left :: Tree a, right :: Tree a }
            deriving (Read, Show)

program :: String
program = "__5__4H___3VS__F___2 UI__L__+_ R__P___1JWAE"
     ++ "__6__=B__/_XD__C__YKN__7_Z__QG__8_ __9__0 OMT "

dict = interpret program
    where
    interpret         = head . foldl exec []
    exec xs       '_' = Leaf         : xs    -- push Leaf
    exec (x:y:xs)  c  = Branch c y x : xs    -- combine subtrees

decodeLetter :: MorseCode -> Char
decodeLetter = tag . foldl (flip step) dict
    where
    step '.' = left
    step '-' = right

decode = map decodeLetter . words
