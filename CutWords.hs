type MorseCode = String
test = "-- --- .-. ... .    -.-. --- -.. ."

program :: String
program = "__5__4H___3VS__F___2 UI__L__+_ R__P___1JWAE"
     ++ "__6__=B__/_XD__C__YKN__7_Z__QG__8_ __9__0 OMT "

decode :: MorseCode -> String
decode = interpret program
    where
    interpret         = head . foldl exec []
    exec xs       '_' = undefined : xs
    exec (x:y:xs)  c  = branch    : xs
        where
        branch (' ':ds) = c : decode ds
        branch ('-':ds) = x ds
        branch ('.':ds) = y ds
        branch []       = [c]
