module Main

counts : String -> (Nat, Nat)
counts str = let
              nbWords = wordCount str
              nbChars = length str
             in
              (nbWords, nbChars)
             where
              wordCount : String -> Nat
              wordCount str = length (words str)

main : IO ()
main = repl "Enter a string: " answer
  where
    answer : String -> String
    answer str = show (counts str) ++ "\n"
