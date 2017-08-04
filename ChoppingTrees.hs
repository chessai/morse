type MorseCode = String
test = "-- --- .-. ... .    -.-. --- -.. ."

program :: String
program = "__5__4H___3VS__F___2 UI__L__+_ R__P___1JWAE"
     ++ "__6__=B__/_XD__C__YKN__7_Z__QG__8_ __9__0 OMT "


branch c x y = \code -> case code of
    '.':ds -> x ds
    '-':ds -> y ds
    []     -> c

leaf         = undefined

dict :: MorseCode -> Char
dict = interpret program
    where
    interpret         = head . foldl exec []
    exec xs       '_' = leaf         : xs    -- push leaf
    exec (x:y:xs)  c  = branch c y x : xs    -- combine subtrees

decodeLetter :: MorseCode -> Char
decodeLetter = dict

decode = map decodeLetter . words
