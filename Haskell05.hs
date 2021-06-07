--1. indice de massa corporal
bmi :: Float -> Float -> String
bmi altura peso = 
    let    imc  = peso / altura ^ 2
    in     if imc < 18.5 then "abaixo" else if imc > 30 then "acima" else "normal"

bmi' :: Float -> Float -> String
bmi' altura peso = if imc < 18.5 then "abaixo" else if imc > 30 then "acima" else "normal"
    where  imc  = peso / altura ^ 2

--3.trocar let in por where e where por let in
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
 where 
     digits = take 9 cpf
     dv1 = cpfDV digits [10,9..]
     dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
 let expr = (sum $ zipWith (*) digits mults) `mod` 11
 in  if expr < 2 then 0 else 11-expr

--4.tabela verdade de and
andTable :: [(Bool, Bool, Bool)]
andTable = [(x,y,x&&y) | x <- [False,True] , y <- [False,True]]
