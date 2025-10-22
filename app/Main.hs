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

    -- Test 8: Create and pretty print Polynomial 3
    let t1_poly3 = Term 2 [(V (Just "x"), 2), (V (Just "y"), 1)]  -- 2x^2 y
    let t2_poly3 = Term 3 [(V (Just "x"), 1), (V (Just "y"), 2)]  -- 3xy^2
    let c1_poly3 = C 5                                            -- Constant term 5
    let poly3 = Poly [t1_poly3, t2_poly3, c1_poly3]               -- poly3 = 2x^2 y + 3xy^2 + 5
    putStrLn "Polynomial 3:"
    putStrLn $ prettyPrintPoly poly3

    -- Test 9: Create and pretty print Polynomial 4
    let t1_poly4 = Term (-1) [(V (Just "x"), 2), (V (Just "y"), 1)]  -- -x^2 y
    let t2_poly4 = Term 4 [(V (Just "x"), 1), (V (Just "y"), 1)]     -- 4xy
    let c1_poly4 = C 7                                               -- Constant term 7
    let poly4 = Poly [t1_poly4, t2_poly4, c1_poly4]                  -- poly4 = -x^2 y + 4xy + 7
    putStrLn "Polynomial 4:"
    putStrLn $ prettyPrintPoly poly4

    -- Test 10: Add polynomials 3 and 4
    let sumPoly3Poly4 = poly3 + poly4
    putStrLn "Sum of Polynomial 3 and Polynomial 4:"
    putStrLn $ prettyPrintPoly sumPoly3Poly4

    -- Test 11: Multiply polynomials 3 and 4
    let prodPoly3Poly4 = poly3 * poly4
    putStrLn "Product of Polynomial 3 and Polynomial 4:"
    putStrLn $ prettyPrintPoly prodPoly3Poly4

    -- Test 12: Differentiate Polynomial 1 with respect to x
    let diffPoly1 = differentiate (V (Just "x")) poly1
    putStrLn "Derivative of Polynomial 1 with respect to x:"
    putStrLn $ prettyPrintPoly diffPoly1

    -- Test 13: Differentiate Polynomial 2 with respect to x
    let diffPoly2 = differentiate (V (Just "x")) poly2
    putStrLn "Derivative of Polynomial 2 with respect to x:"
    putStrLn $ prettyPrintPoly diffPoly2

    -- Test 14: Differentiate Polynomial 3 with respect to x
    let diffPoly3 = differentiate (V (Just "x")) poly3
    putStrLn "Derivative of Polynomial 3 with respect to x:"
    putStrLn $ prettyPrintPoly diffPoly3

    -- Test 15: Differentiate Polynomial 4 with respect to x
    let diffPoly4 = differentiate (V (Just "x")) poly4
    putStrLn "Derivative of Polynomial 4 with respect to x:"
    putStrLn $ prettyPrintPoly diffPoly4

    -- Test 16: Differentiate Polynomial 3 with respect to y
    let diffPoly3Y = differentiate (V (Just "y")) poly3
    putStrLn "Derivative of Polynomial 3 with respect to y:"
    putStrLn $ prettyPrintPoly diffPoly3Y

    -- Test 17: Differentiate Polynomial 4 with respect to y
    let diffPoly4Y = differentiate (V (Just "y")) poly4
    putStrLn "Derivative of Polynomial 4 with respect to y:"
    putStrLn $ prettyPrintPoly diffPoly4Y
