-- para fazer este trabalho, utilizei fontes destes sites
-- https://wiki.haskell.org/How_to_work_on_lists
-- https://mmhaskell.com/blog/2019/5/13/quicksort-with-haskell
-- https://devtut.github.io/haskell/sorting-algorithms.html#insertion-sort

-- 1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] que insere um inteiro na posição correta em uma lista de inteiros já ordenada.
--    Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insere x ys

-- Passo a passo:
-- Como temos uma lista de inteiros [1,2,4,5], o número 3 vai tentar ser inserido na posição [0] (onde se encontra o número 1) e vai fazer a vericação se ele é maior do que o número da posição
-- atual, 3 > 1? sim então pula para a próxima posição e repete o processo de verificação, quando chegar em um número que é menor que o index, ele vai ser inserido na posição anterior.

-- 2) Usando a função insere, defina a função ordenaInsere :: [Int] -> [Int] que ordena uma lista de inteiros em ordem crescente usando o algoritmo de ordenação por inserção.
--    Considere na sua função que uma lista vazia já está em ordem e que para ordenar basta inserir um elemento na posição correta no restante da lista que já deve estar ordenado.

ordenaInsere :: [Int] -> [Int]
ordenaInsere [] = []
ordenaInsere (x : xs) = insere x (ordenaInsere xs)

-- 3) Defina uma função recursiva uneOrdenado :: [Int] -> [Int] -> [Int] que une duas listas já ordenadas em ordem crescente uma terceira lista que também deve estar em ordem crescente.

uneOrdenado :: [Int] -> [Int] -> [Int]
uneOrdenado [] x = x
uneOrdenado x [] = x
uneOrdenado (x : xs) (y : ys) | y < x = y : uneOrdenado (x : xs) ys
uneOrdenado (x : xs) (y : ys) | otherwise = x : uneOrdenado xs (y : ys)

-- 4) Usando a função uneOrdenado, defina uma função ordenaUne :: [Int] -> [Int]
--    que particiona sucessivamente uma lista na metade até atingir partições de tamanho 1 para
--    então ordenar as partições através da  função uneOrdenado até atingir uma lista ordenada.
--    Considere uma lista vazia e a uma lista com um elemento como ordenadas na sua definição.

ordenaUne :: [Int] -> [Int]
ordenaUne [] = []
ordenaUne [x] = [x]
ordenaUne xs = uneOrdenado (ordenaUne ys) (ordenaUne zs)
  where
    (ys, zs) = splitAt ((length xs) `div` 2) xs

-- 5) Explique a função padrão zipWith cuja definição é a seguinte:
-- A função é executada ao mesmo tempo para cada elemento das duas listas, ou seja, em pares... ex: 
-- [a,b,c] [1,2,3] a função irá executar assim -> [a - 1,b - 2,c - 3] 
-- f(x,y) onde x [1,2,3] e y [0,1,2]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []

-- 6) Dê uma definição equivalente da função cresc usando a função zipWith.
crescZip :: (Ord a) => [a] -> Bool
crescZip xs = and $ zipWith' (<=) xs (tail xs)

-- 7) Dê uma definição para a função disjuntas :: (Ord a) => [a] -> [a] -> Bool que recebe duas listas em ordem crescente e determina se as mesmas não possuem nenhum elemento em comum, isto é, se são disjuntas.

disjuntas :: (Ord a) => [a] -> [a] -> Bool
disjuntas xs ys = and $ zipWith' (/=) xs ys
