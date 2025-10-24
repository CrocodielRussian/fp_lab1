import Test.HUnit
import First

check_correctness_task21 :: Test
check_correctness_task21 = TestLabel "Check of correct task21" $ TestList
  [ TestCase (assertEqual "Recursion version test is not correct" 63252 (amicableNumbersSumRec' 0)),
  TestCase (assertEqual "Tail Recursion version test is not correct" 63252 (amicableNumbersSumRecTail' 0 0)),
  TestCase (assertEqual "Generate/filter/reduce version test is not correct" 63252 (finalSum)),
  TestCase (assertEqual "Map version test is not correct" 63252 (targetCasesMap)),
  TestCase (assertEqual "Infinite list version version test is not correct" 63252 (amicableNumbersSum))
  ]


tests :: Test
tests = TestList
      [ 
        check_correctness_task21,
        check_correctness_task9
      ]

main :: IO Counts
main = runTestTT tests