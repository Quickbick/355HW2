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

-- (b) wins_by_year – 12%

------------------------------------------------------
{- P3  sum_nested_int, sum_nested_item, and sum_my_nested -}

-- (a) sum_nested_int - 8%

-- (b) sum_nested_item - 5%

-- (c) sum_my_nested - 12%

------------------------------------------------------
{- P4  tree_height, create_htree, tree_paths -}

-- (a) tree_height - 8%

-- (b) create_htree - 10%

-- (c) tree_paths - 13%

