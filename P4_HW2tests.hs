module P4_HW2tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2

-- Sample Tree Integer examples given in the assignment prompt; make sure to provide your own tree examples in HW2Tests.hs file.
-- Your trees should have minimum 4 levels (including the leaves). 

t1 = Node 3 (Node 10 (Node 1 (Leaf 4) (Leaf 5)) (Leaf 6)) (Node 5 (Leaf 8) (Leaf 4))

t2 =  Node 'C' (Node 'A' (Leaf 'E') (Node 'K' (Leaf 'B') (Leaf 'E'))) 
               (Node 'O' (Node 'M' (Node 'F' (Leaf 'A') (Leaf 'B')) (Leaf 'E')) (Leaf 'E'))

-----------------------------------------------------------     
{- tree_height tests 8% -}
p4a_test1 = TestCase (assertEqual "tree_height test-1"  
                                   4
                                   (tree_height t1) ) 
p4a_test2 = TestCase (assertEqual "tree_height test-2" 
                                   5 
                                   (tree_height t2) ) 

{- create_htree tests 10% -}
p4b_output1 = HNode 3 4 (HNode 10 3 (HNode 1 2 (HLeaf 4) (HLeaf 5)) (HLeaf 6)) (HNode 5 2 (HLeaf 8) (HLeaf 4))                                   
p4b_test1 = TestCase (assertEqual "create_htree test-1" 
                                   p4b_output1 
                                   (create_htree t1) ) 
p4b_output2 = HNode 'C' 5 (HNode 'A' 3 (HLeaf 'E') (HNode 'K' 2 (HLeaf 'B') (HLeaf 'E'))) (HNode 'O' 4 (HNode 'M' 3 (HNode 'F' 2 (HLeaf 'A') (HLeaf 'B')) (HLeaf 'E')) (HLeaf 'E')) 
p4b_test2 = TestCase (assertEqual "create_htree test-2" 
                                   p4b_output2 
                                   (create_htree t2) )

{- find_paths tests 13% -}
p4c_test1 = TestCase (assertEqual "find_paths test-1" 
                                  ["CAE","CAKE","COME","COE"] 
                                  (find_paths p4b_output2 'E') ) 
p4c_test2 = TestCase (assertEqual "find_paths test-2" 
                                  ["COMFA"]
                                  (find_paths p4b_output2 'A') ) 
p4c_test3 = TestCase (assertEqual "find_paths test-3" 
                                  [[3,10,1,4],[3,5,4]]
                                  (find_paths p4b_output1 4) ) 

tests = TestList [ TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4c - test1 " p4c_test1,
                   TestLabel "Problem 4c - test2 " p4c_test2,
                   TestLabel "Problem 4c - test3 " p4c_test3
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests