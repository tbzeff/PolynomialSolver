module Main (main) where

import Polynomial

main :: IO ()
main = do
    putStrLn "Testing Polynomial Library"

    -- Test 1: Create and pretty print a simple polynomial
    let poly1 = Poly [Term 3 [(V (Just "x"), 2)], Term 5 [(V (Just "x"), 1)], C 2]
    putStrLn "Polynomial 1:"
    putStrLn $ prettyPrintPoly poly1

    -- Test 2: Create and pretty print another polynomial
    let poly2 = Poly [Term 1 [(V (Just "x"), 1)], C 4]
    putStrLn "Polynomial 2:"
    putStrLn $ prettyPrintPoly poly2

    -- Test 3: Add two polynomials
    let sumPoly = poly1 + poly2
    putStrLn "Sum of Polynomial 1 and Polynomial 2:"
    putStrLn $ prettyPrintPoly sumPoly

    -- Test 4: Multiply two polynomials
    let prodPoly = poly1 * poly2
    putStrLn "Product of Polynomial 1 and Polynomial 2:"
    putStrLn $ prettyPrintPoly prodPoly

    -- Test 5: Negate a polynomial
    let negPoly = negate poly1
    putStrLn "Negation of Polynomial 1:"
    putStrLn $ prettyPrintPoly negPoly

    -- Test 6: Extract coefficients
    putStrLn "Coefficients of Polynomial 1:"
    print $ coeffs poly1

    -- Test 7: Extract variables
    putStrLn "Variables in Polynomial 1:"
    print $ variables poly1

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
