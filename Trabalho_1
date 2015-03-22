-- Nome: Michael Guilherme Jordan
-- Matrícula: 201120427
-- Disciplina: Paradigmas da Computação

-- 1) Escreva uma função hasEqHeads :: [Int] -> [Int] -> Bool que verifica se as 2 listas possuem o mesmo primeiro elemento. 

hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads x y | head x == head y = True
			   | otherwise = False
			   
-- 2)  Observe a função abaixo, que eleva ao cubo cada elemento da lista, produzindo outra lista.

pot3 :: [Int] -> [Int]
pot3 [] = []
pot3 ns = (head ns)^3 : pot3 (tail ns)			   

-- 3) Escreva uma função recursiva add10, que adicione a constante 10 a cada elemento de uma lista, produzindo outra lista.

add10 :: [Int] -> [Int]
add10 [] = []
add10 x = (head x) + 10 : add10 (tail x)

-- 4)Escreva uma função recursiva addComma, que adicione uma vírgula no final de cada string contida numa lista. Dica: (1) Strings são listas de caracteres. (2) Para concatenar listas, use o operador ++.

addComma :: [String] -> [String]
addComma [] = []
addComma x = ((head x) ++ ",") : addComma (tail x) 

-- 5) Refaça os 2 exercícios anteriores usando a função de alta ordem 'map'.

-- (3)

add_10 :: [Int] -> [Int]
add_10 [] = []
add_10 x = map (+10) x

-- (4)

add_Comma :: [String] -> [String]
add_Comma [] = []
add_Comma x = map (++ ",") x

-- 6) Crie uma função htmlListItems :: [String] -> [String], que receba uma lista de strings e retorne outra lista contendo as strings formatadas como itens de lista em HTML.Dica: use map e defina uma função auxiliar a ser aplicada a cada elemento. 

htmlListItems :: [String] -> [String]
htmlListItems [] = []
htmlListItems x = ("<LI>" ++ (head x) ++ "</LI>") : htmlListItems (tail x)

-- 7) Crie uma função recursiva charFound :: Char -> String -> Bool, que verifique se o caracter (primeiro argumento) está contido na string (segundo argumento).

charFound :: Char -> String -> Bool
charFound  x y  | y == [] = False
				| x == ' ' = False
				| x /= (head y) = charFound x (tail y) -- percorre caracter por caracter pós-head
			    | x == (head y) = True -- quando o caracter for encontrado charFound retornará True
		        | otherwise = False -- caso não seja encontrado, retornará False
				
-- 8) Reescreva a função anterior sem recursão, usando outras funções pré-definidas já vistas em aula.

char_Found :: Char -> String -> Bool
char_Found x y 	|filter (== x) y /= [] = True
				|otherwise = False

-- 9)Use a função de alta ordem 'zipWith' para produzir uma função que obtenha as diferenças, par a par, dos elementos de duas listas. Por exemplo: para listas de entrada [1,2,3,4] e [2,2,1,1], o resultado será [-1,0,2,3].

diferenca_lista :: [Int] -> [Int] -> [Int]
diferenca_lista [] [] = []
diferenca_lista x y = zipWith (-) x y


-- ALTA ORDEM

-- 1) Dada uma lista de números, calcular 2*n+1 para cada número n contido na lista.

f1 :: Int -> Int
f1 x = 2*x + 1;

calculo_1 :: [Int] -> [Int]
calculo_1 [] = []
calculo_1 x = map(\x -> f1 x) x

-- 2) Dadas duas listas X e Y de números inteiros, calcular 4*x+2*y+1 para cada par de números x e y pertencentes às listas.

f2 :: Int -> Int -> Int
f2 x y = 4*x+2*y+1;

calculo_2 :: [Int] -> [Int] -> [Int]
calculo_2 [] [] = []
calculo_2 x y = zipWith(\x y -> f2 x y) x y

-- 3) Dada uma lista de strings, produzir outra lista com strings de 10 caracteres, usando o seguinte esquema: strings de entrada com mais de 10 caracteres são truncadas, strings com até 10 caracteres são completadas com '.' até ficarem com 10 caracteres.

verificadez :: [String] -> [String]
verificadez [] = []
verificadez (x:y) = take 10 (x ++ (repeat '.')) : verificadez y

-- 4) Dada uma lista de idades, selecionar as que são maiores que 20 e, para cada uma, calcular o ano de nascimento correspondente (aproximado, considerando o ano atual).

verificavinte :: [Int] -> [Int]
verificavinte [] = []
verificavinte x = filter (>20) x

verificaano :: [Int]->[Int]
verificaano [] = []
verificaano x = map(2015-)(verificavinte x)

