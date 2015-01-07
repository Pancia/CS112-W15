import Test.HUnit
import Hw1

main :: IO Counts
main = runTestTT $ TestList hw1Tests

testBook = ("Herman Melville", "Moby Dick", 1851)

hw1Tests :: [Test]
hw1Tests =
        ["citeAuthor puts first name and last name in reverse order" ~:
          citeAuthor "Herman" "Melville" ~?= "Melville, Herman"
        , initials "Herman" "Melville" ~?= "H.M."
        , title testBook ~?= "Moby Dick"
        , citeBook testBook ~?= "Moby Dick (Herman Melville, 1851)"
        , bibliography_rec [testBook] ~?= "TODO"
        , bibliography_fold [testBook] ~?= "TODO"
        , averageYear [("","",1),("","",3)] ~?= 2
        , references "text" ~?= 42 --TODO
        , citeText [testBook] "text" ~?= "TODO"
        ]
