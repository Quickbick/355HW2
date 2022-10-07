module P1_HW2tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2

{- insert tests 3% -}
p1a_test1 = TestCase (assertEqual "insert test-1"  
                                  "123a456789" 
                                  (insert 3 'a' "123456789" ) )
p1a_test2 = TestCase (assertEqual "insert test-2" 
                                  [1,2,3,4,5,6,7,8,100] 
                                  (insert 8 100 [1,2,3,4,5,6,7,8]))
p1a_test3 = TestCase (assertEqual "insert test-3" 
                                  [1,2,3,4,5,6,7,8] 
                                  (insert 9 100 [1,2,3,4,5,6,7,8]))
p1a_test4 = TestCase (assertEqual "insert test-4" 
                                  [] 
                                  (insert 3 100 []))
p1a_test5 = TestCase (assertEqual "insert test-5" 
                                  "CptS355" (insert 0 'C' "ptS355"))

{- insert_tail tests 10% -}

p1b_test1 = TestCase (assertEqual "insert_tail test-1"  
                                  "123a456789" 
                                  (insert_tail 3 'a' "123456789" ) )
p1b_test2 = TestCase (assertEqual "insert_tail test-2" 
                                  [1,2,3,4,5,6,7,8,100] 
                                  (insert_tail 8 100 [1,2,3,4,5,6,7,8]))
p1b_test3 = TestCase (assertEqual "insert_tail test-3" 
                                  [1,2,3,4,5,6,7,8] 
                                  (insert_tail 9 100 [1,2,3,4,5,6,7,8]))
p1b_test4 = TestCase (assertEqual "insert_tail test-4" 
                                  [] 
                                  (insert_tail 3 100 []))
p1b_test5 = TestCase (assertEqual "insert_tail test-5" 
                                  "CptS355" 
                                  (insert_tail 0 'C' "ptS355"))

tests = TestList [ TestLabel "Problem 1a - test1 " p1a_test1,
                   TestLabel "Problem 1a - test2 " p1a_test2,
                   TestLabel "Problem 1a - test3 " p1a_test3,                   
                   TestLabel "Problem 1a - test4 " p1a_test4, 
                   TestLabel "Problem 1a - test5 " p1a_test5, 
                   TestLabel "Problem 1b - test1 " p1b_test1,
                   TestLabel "Problem 1b - test2 " p1b_test2,                   
                   TestLabel "Problem 1b - test3 " p1b_test3,
                   TestLabel "Problem 1b - test4 " p1b_test4,
                   TestLabel "Problem 1b - test5 " p1b_test5
                 ] 

-- shortcut to run the tests
run = runTestTT  tests