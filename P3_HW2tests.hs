module P3_HW2tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2

{- sum_nested tests 8% -}
p3a_test1 = TestCase (assertEqual "sum_nested_int test-1" 
                                   62 
                                   (sum_nested_int  [Item 10, Item 5, Array [7, 3, 12, 11], Item 5, Array [9], Array []]) ) 
p3a_test2 = TestCase (assertEqual "sum_nested_int  test-2" 
                                   10 
                                   (sum_nested_int  [Array [-3,-5,-6], Array [10, 11], Item (-5), Array [7], Item 1, Item 0]) ) 
p3a_test3 = TestCase (assertEqual "sum_nested_int  test-3" 
                                   0 
                                   (sum_nested_int  []) ) 

{- sum_nested_item tests 5% -}
p3b_test1 = TestCase (assertEqual "sum_nested_item test-1" 
                                   (Item 62) 
                                   (sum_nested_item [Item 10, Item 5, Array [7, 3, 12, 11], Item 5, Array [9], Array []]) )
p3b_test2 = TestCase (assertEqual "sum_nested_item test-2" 
                                   (Item 10) 
                                   (sum_nested_item [Array [-3,-5,-6], Array [10, 11], Item (-5), Array [7], Item 1, Item 0]) )
p3b_test3 = TestCase (assertEqual "sum_nested_item test-3" 
                                   (Item 0)
                                   (sum_nested_item []) )

{- sum_my_nested tests 12% -}
p3c_test1 = TestCase (assertEqual "sum_my_nested test-1" 
                                    62 
                                    (sum_my_nested [MyItem 10, MyItem 5, MyArray [MyItem 7, MyArray [MyItem 3, MyItem 12], MyItem 11], MyItem 5, MyArray [MyItem 9], MyArray[]]) )
p3c_test2 = TestCase (assertEqual "sum_my_nested test-2" 
                                    28
                                    (sum_my_nested [MyArray [MyItem (-2), MyArray [MyItem 3, MyArray [MyItem 12, MyItem (-5), MyArray [MyItem (-8), MyArray[MyItem 3]], MyArray []], MyItem 11], MyItem 5], MyArray [MyItem 9], MyArray[]]) )
p3c_test3 = TestCase (assertEqual "sum_my_nested test-3" 
                                    0 
                                    (sum_my_nested  []) )

tests = TestList [ TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,  
                   TestLabel "Problem 3a - test3 " p3a_test3,                    
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 3b - test3 " p3b_test3,
                   TestLabel "Problem 3c - test1 " p3c_test1,
                   TestLabel "Problem 3c - test2 " p3c_test2,
                   TestLabel "Problem 3c - test3 " p3c_test3
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests