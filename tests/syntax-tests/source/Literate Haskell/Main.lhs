\documentclass{article}

\begin{document}

\section*{Introduction}

Text outside code environments should follow TeX/LaTeX highlighting.

The code environment delimiters themselves should be highlighted.

Text inside code environments should follow regular Haskell highlighting.

\begin{code}
import           Data.List
import           System.Environment
import           Text.Printf

twoSumN :: Int -> [Int] -> [Int]
twoSumN _ [] = []
twoSumN n (x : xs) | (n - x) `elem` xs = [x, n - x]
                   | otherwise         = twoSumN n xs

threeSumN :: Int -> [Int] -> [Int]
threeSumN _ [] = []
threeSumN n (x : xs) | null partial = threeSumN n xs
                     | otherwise    = x : partial
  where partial = twoSumN (n - x) xs
\end{code}

Text in-between code environments.
% LaTeX comment.

\begin{code}
output :: String -> IO ()
output path = do
  input <- sort . map read . filter (not . null) . lines <$> readFile path
  printf "File: %s\n" path
  printf "  Part 1: %d\n" . product . twoSumN 2020 $ input
  printf "  Part 2: %d\n" . product . threeSumN 2020 $ input

-- Haskell comment inside code environment.

main :: IO ()
main = getArgs >>= mapM_ output
\end{code}

\end{document}
