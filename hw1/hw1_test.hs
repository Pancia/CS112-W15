import Test.HUnit
import Hw1

main :: IO Counts
main = runTestTT $ TestList hw1Tests

testBook = ("Herman Melville", "Moby Dick", 1851)
testBook2 = ("Author", "Title", 2015)
bibliography = "Moby Dick (Herman Melville, 1851)\nTitle (Author, 2015)"

gatsby = ("F. Scott Fitzgerald", "The Great Gatsby", 1925)
moby = ("Herman Melville", "Moby Dick", 1851)

txt :: String
txt = "[1] and [2] both feature characters who will do whatever it takes to " ++
      "get to their goal, and in the end the thing they want the most ends " ++
      "up destroying them.  In case of [2] this is a whale..."

txtToCite = "[1] and [2] both feature"

citedTxt :: String
citedTxt = "The Great Gatsby (F. Scott Fitzgerald, 1925) and Moby Dick (Herman Melville, 1851) both feature"

hw1Tests :: [Test]
hw1Tests =
        ["citeAuthor puts first name and last name in reverse order" ~:
          citeAuthor "Herman" "Melville" ~?= "Melville, Herman"
        , initials "Herman" "Melville" ~?= "H.M."
        , title testBook ~?= "Moby Dick"
        , citeBook testBook ~?= "Moby Dick (Herman Melville, 1851)"
        , bibliography_rec [testBook, testBook2] ~?= bibliography
        , bibliography_fold [testBook, testBook2] ~?= bibliography
        , averageYear [("","",1),("","",3)] ~?= 2
        , references txt ~?= 3
        , citeText [gatsby, moby] txtToCite ~?= citedTxt
        ]
