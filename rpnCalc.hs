--ghc 8.0.2
--Example 1
--Expression: 10*(2+5)-14/(1+2*(1+2))
--Reverse Polish notation: "10 2 5 + * 14 1 2 1 2 + * + / -"
--Expression result: 68
--
--Example 2
--Expression: (1+2)*4+3
--Reverse Polish notation: "1 2 + 4 * 3 +"
--Expression result: 15

--Example 3
--Expression: 2.2*10-15
--Reverse Polish notation: "2.2 10 * 15 -"
--Expression result: 7

--Example 4
--Expression: -5+5*6
--Reverse Polish notation: "-5 5 6 * +"
--Expression result: 25

main = print (calcRPN "-5 5 6 * +")
calcRPN :: String -> Float  
calcRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction xs numberString = read numberString:xs  
