import Test.HUnit
import First
import Second 

check_correctness_task21 :: Test
check_correctness_task21 = TestLabel "Check of correct task21" $ TestList
  [ TestCase (assertEqual "Recursion version test is not correct" 63252 (amicableNumbersSumRec' 0)),
  TestCase (assertEqual "Tail Recursion version test is not correct" 63252 (amicableNumbersSumRecTail' 0 0)),
  TestCase (assertEqual "Generate/filter/reduce version test is not correct" 63252 (finalSum)),
  TestCase (assertEqual "Map version test is not correct" 63252 (targetCasesMap)),
  TestCase (assertEqual "Infinite list version version test is not correct" 63252 (amicableNumbersSum))
  ]

check_correctness_task9 :: Test
check_correctness_task9 = TestLabel "Check of correct task9" $ TestList
  [ TestCase (assertEqual "Recursion version test is not correct" 31875000 (checkTripleRec' 0 0)),
  TestCase (assertEqual "Generate/filter/reduce version test is not correct" 31875000 (checkTripleReduce)),
  TestCase (assertEqual "Map version test is not correct" 31875000 (checkTripleMap)),
  TestCase (assertEqual "Infinite list version version test is not correct" 31875000 (computeResult checkTripleInfinite))
  ]


tests :: Test
tests = TestList
      [ 
        check_correctness_task21,
        check_correctness_task9
      ]

main :: IO Counts
main = runTestTT tests