type Stall = Bool

-- //Para cada posicion de la lista, la distancia de la izquierda.
								-- //Este funciona como acumulador, hace cuanto tiempo vi un true
distancesToLeft :: [Stall] -> Int -> [Int]
distancesToLeft [] tiempoATrue = [] 
distancesToLeft (True:stalls) tiempoATrue = 0 : distancesToLeft bs 0 --//acabo de ver un true
distancesToLeft (False:stalls) tiempoATrue = tiempoATrue : distancesToLeft bs (tiempoATrue + 1)

-- al derecho hago un reverse a la lista

-- /zip (dist bs 0) (dist (reverse bs) 0)

getDistances :: [Stall] -> [(Int, Int)]
getDistances stalls = zip (distancesToLeft stalls 0) (distancesToLeft (reverse stalls) 0)


minOfTuples :: [(Int, Int)] -> [Int]
minOfTuples [] = []
minOfTuples (tuple:tuples) = min (fst tuple) (snd tuple) : minOfTuples tuples


maximalsOf :: [Int] -> [Int]
maximalsOf [] = []
maximalsOf (number:numbers) = if number > (maximum numbers)
								 then [number] 
								 else if number == (maximum numbers)
										 then number : 
