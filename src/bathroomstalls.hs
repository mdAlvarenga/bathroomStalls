type Stall = (Bool, Int) --Tupla en la que la 1er componente indica si esta ocupada o no, y la 2da indica su posicion en el baño.

-- //Para cada posicion de la lista, la distancia de la izquierda.
								-- //Este funciona como acumulador, hace cuanto tiempo vi un true
distancesToLeft :: [Stall] -> Int -> [Int]
distancesToLeft [] tiempoATrue = [] 
distancesToLeft ((True, n):stalls) tiempoATrue = 0 : distancesToLeft bs 0 --//acabo de ver un true
distancesToLeft ((False, n):stalls) tiempoATrue = tiempoATrue : distancesToLeft bs (tiempoATrue + 1)

-- al derecho hago un reverse a la lista

-- /zip (dist bs 0) (dist (reverse bs) 0)

getDistances :: [Stall] -> [(Int, Int)]
getDistances stalls = zip (distancesToLeft stalls 0) (distancesToLeft (reverse stalls) 0)


minOfDistances :: [(Int, Int)] -> [Int]
minOfDistances [] = []
minOfDistances (tuple:tuples) = (minDistance tuple) : minOfDistances tuples

minDistance :: (Int, Int) -> Int
minDistance tuple = min (fst tuple) (snd tuple)

maxOfDistances :: [(Int, Int)] -> [Int]
maxOfDistances [] = []
maxOfDistances (tuple:tuples) = (maxDistance tuple) : maxOfDistances tuples

maxDistance :: (Int, Int) -> Int
maxDistance tuple = max (fst tuple) (snd tuple)

maximalsOf :: [Int] -> [Int]
maximalsOf [] = []
maximalsOf (number:numbers)
	| number > head(maximum numbers) = [number]
	| number == head(maximum numbers) = number : (maximum numbers)
	| number < head(maximum numbers) = maximum numbers


chooseLeftMost :: [Int] -> Int
chooseLeftMost xs = head xs

buildStalls :: Int -> [Stall]


processBathroom :: (Int, Int) -> [Stall]
processBathroom (stallQuantity, people) = buildStalls(stallQuantity)
buildStalls(stallQuantity)
processBathroom st = do
				let distances = getDistances st
				let minDistances = maximalsOf(minOfTuples(distances))
				let maxDistances = maximalsOf(maxOfTuples(distances))
				let leftMost = chooseLeftMost(maxDistances)
				if (length minDistances) == 1
					then head minDistances
					else if (length maxDistances) == 1
							then head maxDistances
							else leftMost

returnLastStall :: [Stall] -> Stall
returnLastStall [st] = st
returnLastStall (st:sts) = returnLastStall sts