module Main

palindrome : String -> Bool
palindrome str = str == reverse str

main : IO ()
main = repl "Enter a string: " answer
  where
    answer : String -> String
    answer str = show (palindrome str) ++ "\n"
