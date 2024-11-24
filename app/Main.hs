import Data.List

parseMatrix :: [String] -> [[Int]]
parseMatrix dataLines = map (map read . words) dataLines

readMatrixFromFile :: FilePath -> IO (Int, [[Int]])
readMatrixFromFile filePath = do
    contents <- readFile filePath
    let
        lines' = lines contents
        n = read (head lines') :: Int
        dataLines = tail lines'
    return (n, parseMatrix dataLines)


replaceValueInList :: Int -> Int -> [Int] -> [Int]
replaceValueInList i value list = map (\k -> if k == i then value else list !! k) [0..(length list - 1)]


bellmanFord :: Int -> [[Int]] -> Int -> [Int]
bellmanFord n mat start = result_dist
    where 
        list_n = replicate n 1000000
        dist_start = replaceValueInList start 0 list_n 
        (result_dist, _) = foldl (\ (dist_current, is_changing_current) _ -> relax n mat (dist_current, is_changing_current)) (dist_start, True) [0..n-1]

relax :: Int -> [[Int]] -> ([Int], Bool) -> ([Int], Bool)
relax n mat (dist, is_changing) =
    if is_changing 
        then (dist_new_after_all_rows, is_changed)
        else (dist, is_changing)
    where
        dist_new_after_all_rows = foldl (\ dist_current i -> relaxVertex n (mat !! i) dist_current i) dist [0..(n - 1)]
        is_changed = dist /= dist_new_after_all_rows

relaxVertex :: Int -> [Int] -> [Int] -> Int -> [Int]
relaxVertex n row dist i = dist_new_after_row
    where
        dist_new_after_row = map (\j -> 
            let
                weight_edge = row !! j
                dist_from = dist !! i
                dist_to = dist !! j
            in
                if weight_edge == 0
                then dist_to
                else min dist_to (dist_from + weight_edge)
            ) [0..(n - 1)]

main :: IO ()
main = do
    (n, mat) <- readMatrixFromFile "app/input1.txt"
    let 
        mat_t = transpose mat
    print $ bellmanFord n mat_t 5
    -- writeMatrixToFile "app/output1.txt" mat
    -- let rows = nrows mat
    --     cols = ncols mat
    -- print $ "Matrix n " ++ show rows ++ "x" ++ show cols