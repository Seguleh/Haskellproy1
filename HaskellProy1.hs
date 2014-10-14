--Incluye explicacion del codigo lo mas claro posible
--
--Part 1
--
--map_of_enemy:
--Llama a mordeK y le pasa la entrada a 'mordeK' con 'x'
map_of_enemy :: [[Int]] -> [[Int]]
map_of_enemy x = mordeK x x

--mordeK:
--Esta funcion envia a huehuE el primer elemento ('a') de la lista 
--de listas recibida por 'map_of_enemy' con la misma lista de listas en 'b'
--y va concatenando lo que devuelve 'huehuE'
mordeK :: [[Int]] -> [[Int]] -> [[Int]]
mordeK [] _ = []
mordeK (x:xs) b = [huehuE x b] ++ mordeK xs b

--huehuE:
--Revisa si el elemento (PosX,PosY,R) que recibe es igual para darle 0
--y cumplir con que el enemigo no se pueda suicidar. En el caso otherwise
--llama a 'curseOTSM' para ver si el enemigo esta en su rango y concatena.
huehuE :: [Int] -> [[Int]] -> [Int]
huehuE _ [] = []
huehuE x (y:ys) | (x == y) = [0] ++ huehuE x ys
				| otherwise = [curseOTSM x y] ++ huehuE x ys

--curseOTSM:
--Usando la ecuacion de Pitagoras un poco modificada para cumplir con 
--la regla de que los otros puntos (enemigos, segundo parametro) esten, 
--o no, dentro del rango del punto pasado por el primer parametro (centro)
curseOTSM :: [Int] -> [Int] -> Int
curseOTSM (x:y:r:xs) (a:b:as) | (x == a) && (y == b) = 0
							  | (((a - x)^2)+((b - y)^2) <= r^2) = 1
					  		  | (((a - x)^2)+((b - y)^2) > r^2) = 0

--
--Part 2
--
--i_can_survive:
--Llama a 'watWat' para devolver YES o NO.
i_can_survive :: [[Int]] -> String
i_can_survive [[]] = "NO"
i_can_survive x = watWat x

--watWat
--Solo revisa si hamSpam devolvio un camino, lo que indicaria
--que si hay forma de matarlos con un solo disparo.
watWat :: [[Int]] -> String
watWat x | (null (hamSpam x)) == True = "NO"
		 | otherwise = "YES"

--hamSpam:
--Recibe la lista de enemigos, utiliza 'map_of_enemy' con 'lamonD' para
--crear los nodos del grafo y se los pasa a magicSunshineJuice para 
--devolverme un camino o nada (que diria que no hay camino).
hamSpam :: [[Int]] -> [[Int]]
hamSpam [] = []
hamSpam [[1]] = [[1]]
hamSpam x = hamSpam (magicSunshineJuice (lamonD (map_of_enemy x) 1) (lamonD (map_of_enemy x) 1) (lamonD (map_of_enemy x) 1) (lamonD (map_of_enemy x) 1) ((length x)-1) (herpDurp (concat (lamonD (map_of_enemy x) 1))) (herpDurp (concat (lamonD (map_of_enemy x) 1))) [])

--magicSunshineJuice:
--Comprobara si existe algo llamado "El Camino".
--Comprueba varias veces si el length de p (p siendo donde se guarda el camino) es igual a la cantidad de nodos - 1
--y asi comprobar que existe el camino donde se alcanzan todos los nodos. Recibe los nodos del grafo en los primeros cuatro, 
--el segundo y tercero para moverse (que depende de el caso que se de) y el cuarto para mantener el original. 
--El quinto parametro es la cantidad de nodos que tiene el grafo. 
--El sexto y septimo es la lista de nodos para ser usada en la funcion boringFunction y mantener como original.
--El ultimo parametro es una lista vacia donde se guardara "EL CAMINO" para ser usado como metodo de comprobacion.
magicSunshineJuice :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> [Int] -> [Int] -> [[Int]] -> [[Int]]

magicSunshineJuice [] _ _ _ a t _ p | a == (length p) = [[1]]
								    | otherwise = []

magicSunshineJuice _ [] (l:ls) o a t bt p | a == (length p) = [[1]]
										  | otherwise = magicSunshineJuice ls o ls o a bt bt []

magicSunshineJuice (x:xs) (y:ys) l o a t bt p | x == y = magicSunshineJuice (x:xs) ys l o a t bt p
									          | a == (length p) = [[1]]
								   	          | funFunction x y t == True = magicSunshineJuice [y] o l o a (boringFunction x t) bt (purgarR (p++[x]++[y]))
								   	          | funFunction x y t == False = magicSunshineJuice [x] ys l o a t bt p
  
--purgarR:
--Concatena el elemento x y despues filter devuelve la lista sin ese elemento donde quiera 
--que este ya que cuando 'not(x == y)' da False no lo mete en la lista que devuelve, 
--de esa forma elimina todos los repetidos al terminar de recorrer la lista.
--Usado para la lista del camino para evitar repeticion ya que la forma que magicSunshine 
--guarda el camino despues del primer nodo se da para repeticion aunque no hace daño mientras corre pero se
--necesita quitar la repeticion de nodos al guardar en 'p' para poder comprobar el length correcto.
purgarR :: [[Int]] -> [[Int]]
purgarR [] = []
purgarR (x:xs) = x : purgarR (filter (\y -> not(x == y)) xs)

--funFunction:
--Es una funcion que revisa que cuando pasas de un nodo al otro
--cumple que el proximo nodo (_:d) del nodo del que se esta moviendo (a:_) no se devuelve
--(a /= d) y que el nodo siguiente (c:_) sea igual al segundo nodo que tiene el nodo inicial (_:b).
funFunction :: [Int] -> [Int] -> [Int] -> Bool
funFunction (a:b:_) (c:d:_) n | (a /= d) && (b == c) = checkN d n
							  | otherwise  = False

--checkN:
--Revisa que el proximo nodo a moverse no sea de los nodos visitados. Si esta en la lista significa que si puedo moverme y 
--devuelve True.
checkN :: Int -> [Int] -> Bool
checkN _ [] = False
checkN x (y:ys) | x == y = True
				| otherwise = checkN x ys

--boringFunction:
--Esta funcion recibe un nodo (Ej: [1,3]) y la lista de nodos (Ej: [1,2,3,4])
--para eliminar los nodos 1 y 3 de la lista de nodos y asi registrar los nodos
--visitados. Si estan eliminados es que ya estan visitados
boringFunction :: [Int] -> [Int] -> [Int]
boringFunction [] n = n
boringFunction (x:xs) n = boringFunction xs (dupD x n)

--dupD:
--Esta funcion recibe un entero y lo elimina de la lista que recibe como segundo
--parametro. Devuelve la lista sin el elemento
dupD :: Int -> [Int] ->[Int]
dupD _ [] = []
dupD x (y:ys) | x == y = dupD x ys
			  | otherwise = [y] ++ dupD x ys

--lamonD:
--Utiliza 'armarG' para crear la lista de nodos, que daria el grafo
--final a utilizar en 'hamSpam' y asi saber si existe o no la posibilidad
--de matar a todos los enemigos de un solo disparo.
lamonD :: [[Int]] -> Int -> [[Int]]
lamonD [] _ = []
lamonD (x:xs) p = armarG x p 1 ++ lamonD xs (p+1)

--herpDurp:
--Igual que purgarR pero solo con listas. 
--Eliminar repetidos en una lista para ser usada como lista de nodos y
--ver si ya se visitaron quitandolos de dicha lista.
herpDurp :: [Int] -> [Int]
herpDurp [] = []
herpDurp (x:xs) = x : herpDurp (filter (\y -> not(x == y)) xs)

--armarG:
--En esta funcion armamos los nodos del grafo que seran usados
--para probar que existe al menos un camino que recorra todos los nodos 
--una vez (Camino Hamiltoniano). Recibe como parametro la posicion logica 
--del enemigo y compara si es igual a 1 o 0 para despues armar la 
--direccion del nodo con numero de nodo 'p' y nodo adyacente 'a'
armarG :: [Int] -> Int -> Int -> [[Int]]
armarG [] _ _ = []
armarG (x:xs) p a | (x == 0) = armarG xs p (a+1)
				  | (x == 1) = [[p,a]] ++ armarG xs p (a+1)