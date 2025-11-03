{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Eta reduce" #-}

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char =
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze

split_on :: Char -> String -> [String]
split_on c string
    | (string == []) = []
    | (c `notElem` string) = [string]
    | otherwise = (takeWhile (/= c) string) : split_on c (drop 1 (dropWhile (/= c) string))

maze_path = "lab10windows\\maze-big-3.txt"

get_maze file = do
    s <- readFile file
    let a = split_on '\n' s
    return a

print_maze maze = do
    putStrLn (unlines maze)

is_wall maze (x,y) = get maze x y == '#'

place_object :: [String] -> (Int, Int) -> Char -> [String]
place_object maze (x,y) o = set maze x y o

move (x,y) char = case char of
    'w' -> (x, y - 1)
    'a' -> (x - 1, y)
    's' -> (x, y + 1)
    'd' -> (x + 1, y)

can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze (x,y) dir = not (is_wall maze (move (x,y) dir))

game_loop :: [String] -> (Int, Int) -> (Int, Int) -> IO ()
game_loop maze (x,y) (e_x, e_y) = do
    let n_maze = place_object (place_object maze (x,y) '@') (e_x, e_y) '>'
    print_maze n_maze
    str <- getLine
    let c_m = can_move maze (x,y) (head str)
        m = move (x,y) (head str)
    if(str /= "quit") then
        if (m /= (e_x,e_y)) then
            if (c_m) then
                game_loop maze m (e_x, e_y)
            else
                game_loop maze (x,y) (e_x, e_y)
        else
            putStrLn ("You win!")
    else
        putStrLn ("")


data DTree a = Leaf a | Twig a (DTree a) | Branch a (DTree a) (DTree a) | Branch_3 a (DTree a) (DTree a) (DTree a) | Branch_4 a (DTree a) (DTree a) (DTree a) (DTree a) deriving (Show, Read)
directions = ['w','a','s','d']

get_possible_moves :: [String] -> (Int, Int) -> [Char]
get_possible_moves maze pos = filter (\x -> can_move maze pos x) directions
generate_tree_help :: [String] -> (Int, Int) -> [(Int, Int)] -> DTree (Int, Int)
generate_tree_help maze pos prevs
  | (n == 4) = Branch_4 pos (generate_tree_help maze (head moves) (pos : prevs)) (generate_tree_help maze (moves !! 1) (pos : prevs))  (generate_tree_help maze (moves !! 2) (pos : prevs)) (generate_tree_help maze (moves !! 3) (pos : prevs))
  | (n == 3) = Branch_3 pos (generate_tree_help maze (head moves) (pos : prevs)) (generate_tree_help maze (moves !! 1) (pos : prevs)) (generate_tree_help maze (moves !! 2) (pos : prevs))
  | (n == 2) = Branch pos (generate_tree_help maze (head moves) (pos : prevs)) (generate_tree_help maze (moves !! 1) (pos : prevs))
  | (n == 0) = Leaf pos
  | otherwise = Twig pos (generate_tree_help maze (head moves) (pos : prevs))
  where a = get_possible_moves maze pos
        m = map (move pos) a
        moves = filter (\x -> notElem x prevs) m
        n = length moves
generate_tree maze pos = generate_tree_help maze pos []

is_in_tree (Leaf a) i = (a == i)
is_in_tree (Twig a b) i = (a == i) || is_in_tree b i
is_in_tree (Branch a b1 b2) i = (a == i) || ((is_in_tree b1 i) || (is_in_tree b2 i))
is_in_tree (Branch_3 a b1 b2 b3) i = (a == i) || ((is_in_tree b1 i) || (is_in_tree b2 i) || (is_in_tree b3 i))
is_in_tree (Branch_4 a b1 b2 b3 b4) i = (a == i) || ((is_in_tree b1 i) || (is_in_tree b2 i) || (is_in_tree b3 i) || (is_in_tree b4 i))

get_path_help :: DTree (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
get_path_help (Leaf a) end path = if (a == end) then path ++ [a] else path
get_path_help (Twig a b) end path
    | (a == end) = path ++ [a]
    | (is_in_tree b end) = get_path_help b end (path ++ [a])
    | otherwise = path
get_path_help (Branch a b1 b2) end path
    | (a == end) = path ++ [a]
    | (is_in_tree b1 end) = get_path_help b1 end (path ++ [a])
    | (is_in_tree b2 end) = get_path_help b2 end (path ++ [a])
    | otherwise = path
get_path_help (Branch_3 a b1 b2 b3) end path
    | (a == end) = path ++ [a]
    | (is_in_tree b1 end) = get_path_help b1 end (path ++ [a])
    | (is_in_tree b2 end) = get_path_help b2 end (path ++ [a])
    | (is_in_tree b3 end) = get_path_help b3 end (path ++ [a])
    | otherwise = path
get_path_help (Branch_4 a b1 b2 b3 b4) end path
    | (a == end) = path ++ [a]
    | (is_in_tree b1 end) = get_path_help b1 end (path ++ [a])
    | (is_in_tree b2 end) = get_path_help b2 end (path ++ [a])
    | (is_in_tree b3 end) = get_path_help b3 end (path ++ [a])
    | (is_in_tree b4 end) = get_path_help b4 end (path ++ [a])
    | otherwise = path

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze start end = get_path_help (generate_tree maze start) end []

place_many_objects maze [] _ = maze
place_many_objects maze (x:xs) o = place_object (place_many_objects maze xs o) x o

solve_maze :: String -> IO()
solve_maze path = do
    maze <- get_maze path
    let solution = get_path maze (1,1) (59,19)
        maze_w_path = place_many_objects maze solution '.'
    putStr (unlines maze_w_path)