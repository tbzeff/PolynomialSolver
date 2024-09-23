module Main (main) where

import Polynomial

main :: IO ()
main = do
    -- Example terms for p1: 2x^2 y + 3xy^2 + 5
    let t1_p1 = Term 2 [(V (Just "x"), 2), (V (Just "y"), 1)]  -- 2x^2 y
    let t2_p1 = Term 3 [(V (Just "x"), 1), (V (Just "y"), 2)]  -- 3xy^2
    let c1_p1 = C 5                                            -- Constant term 5
    let p1 = Poly [t1_p1, t2_p1, c1_p1]                        -- p1 = 2x^2 y + 3xy^2 + 5
    
    -- Example terms for p2: -x^2 y + 4xy + 7
    let t1_p2 = Term (-1) [(V (Just "x"), 2), (V (Just "y"), 1)]  -- -x^2 y
    let t2_p2 = Term 4 [(V (Just "x"), 1), (V (Just "y"), 1)]     -- 4xy
    let c1_p2 = C 7                                               -- Constant term 7
    let p2 = Poly [t1_p2, t2_p2, c1_p2]                           -- p2 = -x^2 y + 4xy + 7
    putStrLn $ "p1: " ++ prettyPrintPoly p1
    putStrLn $ "p2: " ++ prettyPrintPoly p2
    putStrLn $ "p1 + p2: " ++ prettyPrintPoly (p1 + p2)
    putStrLn $ "p1 * p2: " ++ prettyPrintPoly (p1 * p2)
