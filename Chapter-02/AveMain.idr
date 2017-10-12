module Main

import Average

showAverage : (str : String) -> String
showAverage str = "The average length of words of\n" ++
                  str ++ "\n" ++
                  "is " ++ show (average str) ++ "\n"


main : IO ()
main = repl "> " showAverage
