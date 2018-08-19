import Test.DocTest (doctest)

main :: IO ()
main = doctest ["./src/Task2.hs", "./src/Task3.hs", "./src/Task4.hs"]