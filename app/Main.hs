import Data.List
import Control.Parallel.Strategies
import System.Clock
import Control.DeepSeq
import Control.Concurrent

parseMatrix :: [String] -> [[Int]]
parseMatrix dataLines = map (map read . words) dataLines

readMatrixFromFile :: FilePath -> IO (Int, Int, [[Int]])
readMatrixFromFile filePath = do
    contents <- readFile filePath
    let
        lines' = lines contents
        p_count = read (head lines') :: Int
        lines'' = tail lines'
        n = read (head lines'') :: Int
        dataLines = tail lines''
    return (p_count, n, parseMatrix dataLines)


replaceValueInList :: Int -> Int -> [Int] -> [Int]
replaceValueInList i value list = map (\k -> if k == i then value else list !! k) [0..(length list - 1)]

splitList :: [Int] -> Int -> [[Int]]
splitList list n = splitListImp list n1 chunk_size remainder []
    where
        len = length(list)
        n1 = min len n
        chunk_size = len `div` n1
        remainder = len `mod` n1

splitListImp :: [Int] -> Int -> Int -> Int -> [[Int]] -> [[Int]]
splitListImp [] n chunk_size remainder acc = reverse acc
splitListImp list n chunk_size remainder acc = 
    let
        chunk = take chunk_size list
        rest = drop chunk_size list
        acc1 = chunk : acc
    in 
        if remainder > 0
        then splitListImp rest (n - 1) (length rest `div` (n-1)) (length rest `mod` (n-1)) acc1
        else splitListImp rest (n - 1) chunk_size remainder acc1


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

calculateBellmanFord :: Int -> [[Int]] -> [Int] -> [Int]
calculateBellmanFord n mat_t vertices = dist
    where
        all_dist = map (bellmanFord n mat_t) vertices
        dist = map head all_dist

parallelBellmanFord :: Int -> [[Int]] -> Int -> [Int]
parallelBellmanFord n mat_t p_count = dist
    where
        vertices = splitList [0..(n - 1)] p_count
        dist = concat all_dist
        all_dist = map runEval $ parMap rseq (\ i -> rpar $ calculateBellmanFord n mat_t (vertices !! i)) [0..(p_count - 1)]


main :: IO ()
main = do
    (p_count, n, mat) <- readMatrixFromFile "input1.txt"
    setNumCapabilities p_count
    putStrLn $ "Число потоков: " ++ show p_count
    let mat_t = transpose mat
    
    timeStart <- getTime Monotonic
    result <- return $!! parallelBellmanFord n mat_t 5
    timeEnd <- getTime Monotonic

    let timeRun = timeEnd - timeStart
    let seconds = sec timeRun
    let milliseconds = (nsec timeRun) `div` 1000000
    print $ "Time run: " ++ show seconds ++ "." ++ show milliseconds

    writeFile "output.txt" (show result)