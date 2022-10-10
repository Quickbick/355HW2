-- CptS 355 - Fall 2022 -- Homework2 - Haskell
-- Name: Nathanael Ostheller

module HW2
     where

{- P1 - insert, insert_tail -}

-- (a) insert – 3%
insert n item [] | n == 0 = item:[]
                 | otherwise = []
insert n item (i:iL) | n == 0 = item:i:iL
                     | otherwise = i: (insert (n - 1) item iL)

-- (b) insert_tail –  10%
insert_tail n item []| n == 0 = item:[]
                     | otherwise = []
insert_tail n item list = reverse (insertHelper n item list [])
     where insertHelper n item [] buf | n == 0 = item:buf
                                      | otherwise = buf
           insertHelper n item (i:iL) buf | n == 0 = revappend iL (i:(item:buf))
                                          | otherwise = insertHelper (n - 1) item iL (i:buf)
               where  revappend [] list = list
                      revappend (x:xs) list = (revappend xs (x:list))

------------------------------------------------------
{- P2  game_scores and wins_by_year  -}

-- (a) game_scores – 12% 
game_scores [] name = []
game_scores list name = map removeName (filter (hasName name) (foldr (++) [] (map removeName list)))
     where hasName name (x,y)| name == x = True
                             | otherwise = False
           removeName (x, y) = y

-- (b) wins_by_year – 12%
wins_by_year [] = []
wins_by_year list = map makeTuple list
     where makeTuple (x, y) = (x , (length  (filter removeLoss y)))
               where removeLoss (x, (y, z)) | y > z = True
                                            | otherwise = False

------------------------------------------------------
{- P3  sum_nested_int, sum_nested_item, and sum_my_nested -}
data NestedList  = Item Int 
                 | Array [Int] 
                 deriving (Show, Read, Eq) 

data MyNested  = MyItem Int 
               | MyArray [MyNested] 
               deriving (Show, Read, Eq) 

-- (a) sum_nested_int - 8%
sum_nested_int [] = 0
sum_nested_int list = foldr addNested 0 list
     where addNested (Item x) sum = sum + x
           addNested (Array x) sum = foldr (+) sum x

-- (b) sum_nested_item - 5%
sum_nested_item [] = (Item 0)
sum_nested_item list = foldr addNested (Item 0) list
     where addNested (Item x) (Item sum) = Item (sum + x)
           addNested (Array x) (Item sum) = Item (foldr (+) sum x)


-- (c) sum_my_nested - 12%
sum_my_nested [] = 0
sum_my_nested list = foldr addNested 0 list
     where addNested (MyItem x) sum = sum + x
           addNested (MyArray x) sum = foldr addNested sum x 

------------------------------------------------------
{- P4  tree_height, create_htree, tree_paths -}

-- (a) tree_height - 8%

-- (b) create_htree - 10%

-- (c) tree_paths - 13%

