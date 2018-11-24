type Stall = (Bool, Int) --Tupla en la que la 1er componente indica si esta ocupada o no, y la 2da indica su posicion en el baño.

-- //Para cada posicion de la lista, la distancia de la izquierda.
								-- //Este funciona como acumulador, hace cuanto tiempo vi un true
distancesToLeft :: [Stall] -> Int -> [Int]
distancesToLeft [] tiempoATrue = [] 
distancesToLeft ((True, n):stalls) tiempoATrue = 0 : distancesToLeft bs 0 --//acabo de ver un true
distancesToLeft ((False, n):stalls) tiempoATrue = tiempoATrue : distancesToLeft bs (tiempoATrue + 1)

-- al derecho hago un reverse a la lista

-- /zip (dist bs 0) (dist (reverse bs) 0)

getDistances :: [Stall] -> [(Stall, (Int, Int))]
getDistances stalls = zip stalls (zipDistances stalls)
	where zipDistances stalls = zip (distancesToLeft stalls 0) (distancesToLeft (reverse stalls) 0)


minDistances :: [(Stall, (Int, Int))] -> [(Stall, Int)]
minDistances [] = []
minDistances ((stall, distance):distances) = (stall, (minDistance distance)) : minDistances distances


minDistance :: (Int, Int) -> Int
minDistance tuple = min (fst tuple) (snd tuple)


maxDistances :: [(Stall, (Int, Int))] -> [(Stall, Int)]
maxDistances [] = []
maxDistances ((stall, distance):distances) = (maxDistance tuple) : maxDistances distances


maxDistance :: (Int, Int) -> Int
maxDistance tuple = max (fst tuple) (snd tuple)


maximalsOf :: [(Stall, Int)] -> [(Stall, Int)]
maximalsOf [] = []
maximalsOf ((stall, num) : distances) 
	| num > head(maximalsOf distances) = [(stall, number)]
	| num == head(maximalsOf distances) = (stall, number) : (maximalsOf distances)
	| num < head(maximalsOf distances) = maximalsOf distances


chooseLeftMost :: [(Stall, Int)] -> (Stall, Int)
chooseLeftMost xs = head xs


buildStalls :: Int -> [Stall]
buildStalls quantity = (True, 0) : buildStallsWith quantity ++ [(True, quantity + 1)]
	where 	buildStallsWith 0 = []
			buildStallsWith q = (False, q) : buildStallsWith (q - 1)

			zip ([True] ++ replicate quantity False ++ [True]) [0..]


returnLastStall :: [Stall] -> Stall
returnLastStall [st] = st
returnLastStall (st:sts) = returnLastStall sts

-- calcular minimos = map min
-- maximo = maximum
-- filter (==maximum) length == 1
-- ...

updateStalls :: (Stall, Int) -> [Stall] -> [Stall]
updateStalls ()



processForPerson :: [Stall] -> (Int, Int)
processForPerson 
	let stallDistances = getDistances stalls --(stall, (int, int))
	let maximalsOfMinDistances = maximalsOf(minDistances(stallDistances))
	-- let maximalsOfMaxDistances = maximalsOf(maxDistances(stallDistances))
	-- let leftMost = chooseLeftMost(maxDistances)
	if (length maximalsOfMinDistances) == 1
		then getDistances(updateStalls(head minDistances) stalls)
		else if (length maxDistances) == 1
				then head maxDistances
				else leftMost


--Recibe una tupla con una cantidad de stalls y una cantidad de personas, y la procesa, retornando una tupla que en la primer coordenada tiene
-- el minimo de los segmentos del stall para la última persona que ingresa, y en la segunda coordenada, el máximo de los segmentos para dicha persona.
processBathroom :: (Int, Int) -> (Int, Int)
processBathroom (stallQuantity, 0) = updateStalls(processForPerson(buildStalls(stall))) -- ???????? //ivan puto
processBathroom (stallQuantity, people) =  
		let stalls = buildStalls(stallQuantity)
		processForPerson stalls


--Recibe la lista de tuplas con n stalls, y m personas. 
--Retorna la lista de tuplas con el mínimo segmento para la última persona que ingresa a un stall, en la primer coordenada,
-- y el máximo segmento, en la segunda coordenada; esto por cada tupla procesada.
processFile :: [(Int, Int)] -> [(Int, Int)]
processFile [] = []
processFile (item : items) = processBathroom item : processFile items 

