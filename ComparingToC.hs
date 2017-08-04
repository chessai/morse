import Data.Strict    -- http://hackage.haskell.org/package/strict-0.3.2
import Data.List (foldl')
import Data.Array.Unboxed

type MorseCode = String
test = "-- --- .-. ... .    -.-. --- -.. ."

    -- Abstract data type representing the morse code tree,
    -- implemented as array.
type Index = Int
type Dict  = Pair (UArray Index Char) Index

left, right :: Dict -> Dict
left  (a :!: k) = (a :!: 2*k + 1)
right (a :!: k) = (a :!: 2*k + 2)

tag :: Dict -> Char
tag   (a :!: k) = a ! k

dict = (listArray (0,31) " ETIANMSURWDKGOHVF L PJBXCYZQ  " :!: 0)

    -- Formulation of dichotomic search essentially unchanged
decodeLetter :: MorseCode -> Char
decodeLetter = tag . foldl' (flip step) dict
    where
    step '.' = left
    step '-' = right

decode = map decodeLetter . words
