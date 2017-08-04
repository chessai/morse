type MorseCode = String
test = "-- --- .-. ... .    -.-. --- -.. ."

data Tree a = Leaf
            | Branch { tag :: a, left :: Tree a, right :: Tree a }
            deriving (Read, Show)

dict :: Tree Char
dict = Branch ' '
    (Branch 'E'
        (Branch 'I'
            (Branch 'S'
                (Branch 'H'
                    (Branch '5' Leaf Leaf)
                    (Branch '4' Leaf Leaf))
                (Branch 'V' Leaf (Branch '3' Leaf Leaf)))
            (Branch 'U'
                (Branch 'F' Leaf Leaf)
                (Branch ' ' Leaf (Branch '2' Leaf Leaf))))
        (Branch 'A'
            (Branch 'R'
                (Branch 'L' Leaf Leaf)
                (Branch ' ' (Branch '+' Leaf Leaf) Leaf))
        (Branch 'W'
            (Branch 'P' Leaf Leaf)
            (Branch 'J' Leaf (Branch '1' Leaf Leaf)))))
    (Branch 'T'
        (Branch 'N'
            (Branch 'D'
                (Branch 'B'
                    (Branch '6' Leaf Leaf)
                    (Branch '=' Leaf Leaf))
                (Branch 'X' (Branch '/' Leaf Leaf) Leaf))
            (Branch 'K'
                (Branch 'C' Leaf Leaf)
                (Branch 'Y' Leaf Leaf)))
        (Branch 'M'
            (Branch 'G'
                (Branch 'Z' (Branch '7' Leaf Leaf) Leaf)
                (Branch 'Q' Leaf Leaf))
            (Branch 'O'
                (Branch ' ' (Branch '8' Leaf Leaf) Leaf)
                (Branch ' '
                    (Branch '9' Leaf Leaf)
                    (Branch '0' Leaf Leaf)))))

decodeLetter :: MorseCode -> Char
decodeLetter = tag . foldl (flip step) dict
    where
    step '.' = left
    step '-' = right

decode = map decodeLetter . words
