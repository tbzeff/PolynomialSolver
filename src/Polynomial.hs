module Polynomial
    ( 
        Vari(..), Term(..), Polynomial(..),
        coeffs, variables,
        prettyPrintTerm, prettyPrintPoly, normalize
    ) where

import Data.List (sort, sortOn, groupBy, intercalate, partition)
import Data.Function (on)

-- TODO: 
-- - multi-var Terms
-- - constant Terms

data Vari = V (Maybe String)
    deriving (Show, Eq)
data Term = Term { coefficient :: Int, vars :: [(Vari, Int)] } | C Int
    deriving (Show, Eq)
data Polynomial = Mono Term | Poly [Term]
    deriving (Show, Eq)

instance Ord Vari where
    compare (V Nothing) (V Nothing) = EQ
    compare (V Nothing) _ = LT
    compare _ (V Nothing) = GT
    compare (V (Just v1)) (V (Just v2)) = compare v1 v2

instance Ord Term where
    compare (C c1) (C c2) = compare c1 c2  -- Compare constants
    compare (C _) _ = LT                   -- Constants come before terms with variables
    compare _ (C _) = GT
    compare (Term _ vs1) (Term _ vs2) =
        let degree1 = sum $ map snd vs1  -- Total degree of the first term
            degree2 = sum $ map snd vs2  -- Total degree of the second term
        in if degree1 /= degree2
            then compare degree2 degree1  -- Sort by total degree (descending)
            else compare vs1 vs2          -- If degrees are equal, compare lexicographically by variables

tMult :: Term -> Term -> Term
tMult (C c1) (C c2) = C (c1 * c2)
tMult (Term c1 vs1) (C c2) = Term (c1 * c2) vs1
tMult (C c2) (Term c1 vs1) = Term (c1 * c2) vs1
tMult (Term c1 vs1) (Term c2 vs2) = Term (c1 * c2) (combineVars vs1 vs2)
    where 
        combineVars vs1 vs2 = 
            let allVars = vs1 ++ vs2
                grouped = groupBy ((==) `on` fst) (sortOn fst allVars)  -- Sort variables lexicographically
                sumExponents grp = (fst (head grp), sum $ map snd grp)
            in sortOn fst $ map sumExponents grouped  -- Sort the final combined variables list

instance Num Polynomial where
    (+) (Mono t1) (Mono t2) = normalize (Poly [t1, t2])
    (+) (Mono t) (Poly ts) = normalize (Poly (t : ts))
    (+) (Poly ts) (Mono t) = normalize (Poly (t : ts))
    (+) (Poly ts1) (Poly ts2) = normalize (Poly (ts1 ++ ts2))
    
    (*) (Mono t1) (Mono t2) = Mono (tMult t1 t2)
    (*) (Mono t) (Poly ts) = normalize $ Poly (map (\x -> tMult t x) ts)
    (*) (Poly ts) (Mono t) = normalize $ Poly (map (\x -> tMult t x) ts)
    (*) (Poly ts1) (Poly ts2) = normalize $ Poly termsProduct
        where termsProduct = [tMult t1 t2 | t1 <- ts1, t2 <- ts2]

    abs = id
    signum _ = 1
    fromInteger n = Mono (C (fromInteger n))
    negate (Mono (C c)) = Mono (C (negate c))
    negate (Mono (Term coeff vars)) = Mono (Term (negate coeff) vars)
    negate (Poly ts) = Poly (map negateTerm ts)

-- Helper function to negate a Term or a constant
negateTerm :: Term -> Term
negateTerm (C c) = C (negate c)
negateTerm (Term coeff vars) = Term (negate coeff) vars

prettyPrintTerm :: Term -> String
prettyPrintTerm (C coeff) = show coeff  -- Handle constant terms
prettyPrintTerm (Term coeff vars) =
    let coeffStr = 
            if coeff == 1 && not (null vars) 
            then "" 
            else if coeff == (-1) && not (null vars) 
                then "-"
                else show coeff
        
        -- Function to print a single variable and its exponent
        varToString (V (Just v), exp) = 
            if exp == 1 then v else v ++ "^" ++ show exp
        varToString (V Nothing, exp) = 
            if exp == 1 then "x" else "x^" ++ show exp
        
        -- Concatenate variable strings
        varsStr = unwords $ map varToString vars
    
    -- If no variables, return just the coefficient (handles constant terms)
    in if coeff == 0 then "" 
       else if null vars 
            then coeffStr 
            else coeffStr ++ varsStr

prettyPrintPoly :: Polynomial -> String
prettyPrintPoly (Mono t) = prettyPrintTerm t
prettyPrintPoly (Poly ts) =
    let terms = filter (not . null) $ map prettyPrintTerm ts
    in intercalate " + " terms

-- Helper function to check if a term is a constant
isConstant :: Term -> Bool
isConstant (C _) = True
isConstant _     = False

coeffs :: Polynomial -> [Int]
coeffs (Poly p) = map (\(Term c _) -> c) p

variables :: Polynomial -> [[(Vari, Int)]]
variables (Poly p) = map (\(Term _ vs) -> vs) p

normalize :: Polynomial -> Polynomial
normalize (Mono t) = Mono t
normalize (Poly ts) =
    let (constants, terms) = partition isConstant ts
        
        -- Group terms by their sorted variable lists, combining like terms
        groupedTerms = groupBy ((==) `on` (sortOn fst . vars)) (sortOn (sortOn fst . vars) terms)
        
        -- Combine groups of terms (ignoring constants)
        combineGroup grp =
            let coeffSum = sum $ map coefficient grp
                (Term _ vs) = head grp  -- Extract the variable list from the first term in the group
            in Term coeffSum (sortOn fst vs)  -- Sort the variable list in the combined term
        
        -- Sum up all constants
        combineConstants = sum $ map (\(C c) -> c) constants
        
        -- Filter out zero coefficient terms and create a list of terms
        combinedTerms = filter (\t -> coefficient t /= 0) (map combineGroup groupedTerms)
        
        -- Add the constant term if it's non-zero
        result = if combineConstants /= 0
                 then combinedTerms ++ [C combineConstants]
                 else combinedTerms
    in Poly result
