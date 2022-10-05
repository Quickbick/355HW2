module P2_HW2tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2

---------------------------------------------------------
{-Test input for game_scores and wins_by_year -}
wsu_games = [
    (2019, [("NMSU",(58,7)), ("UNCO",(59,17)), ("HOU",(31,24)), ("UCLA",(63,67)), ("UTAH",(13,38)), 
            ("ASU",(34,38)), ("COLO",(41,10)), ("ORE",(35,37)), ("CAL",(20,33)), ("STAN",(49,22)), 
            ("ORST",(54,53)), ("WASH",(13,31)), ("AFA",(21,31))]),
    (2020, [("ORST",(38,28)), ("ORE",(29,43)), ("USC",(13,38)), ("UTAH",(28,45))]),
    (2021, [("USU",(23,26)), ("PORT ST.",(44,24)), ("USC",(14,45)), ("UTAH",(13,24)), ("CAL",(21,6)),
            ("ORST",(31,24)), ("STAN",(34,31)), ("BYU",(19,21)), ("ASU",(34,21)), ("ORE",(24,38)), 
            ("ARIZ",(44,18)), ("WASH",(40,13)), ("CMU",(21,24))] )
            ]
-----------------------------------------------------------     

{- game_scores tests  12% -}                                 
p2a_test1 = TestCase (assertEqual "game_scores-test1" 
                                  (sort [(13,38),(14,45)])  
                                  (sort $ game_scores wsu_games "USC") ) 
p2a_test2 = TestCase (assertEqual "game_scores-test2" 
                                  (sort [(54,53),(38,28),(31,24)]) 
                                  (sort $ game_scores wsu_games "ORST") ) 
p2a_test3 = TestCase (assertEqual "game_scores-test3" 
                                  [] 
                                  (sort $ game_scores wsu_games "YALE") ) 

{- wins_by_year tests  12% -}                                 
p2b_test1 = TestCase (assertEqual "wins_by_year-test1" 
                                  (sort [(2019,6),(2020,1),(2021,7)])  
                                  (sort $ wins_by_year wsu_games) ) 

tests = TestList [ TestLabel "Problem 2a  - test1 " p2a_test1,
                   TestLabel "Problem 2a  - test2 " p2a_test2,  
                   TestLabel "Problem 2a  - test3 " p2a_test3,
                   TestLabel "Problem 2b  - test1 " p2b_test1
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests