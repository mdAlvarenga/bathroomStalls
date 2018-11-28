module Main where
	import           Data.Text (Text)
	import qualified Data.Text as T
	import qualified Data.Text.IO as T
	import           Data.Map (Map)
	import qualified Data.Map.Strict as Map
	import Data.List
	import Data.Void
	
	data Section = Section Integer
				 deriving (Show, Ord, Eq)
	
	type Stalls = Map Section Integer
	
	sacarUnaSeccionTipo :: Section -> Stalls -> Stalls
	sacarUnaSeccionTipo section stalls =
	  Map.update (\n -> if n <= 1 then Nothing else Just $ n - 1) section stalls
	
	agregarNuevaSeccion :: Section -> Stalls -> Stalls
	agregarNuevaSeccion section stalls =
	  Map.insertWith (\new old -> old + new) section 1 stalls
	
	findStall :: Stalls -> Stalls
	findStall stalls =
	  let
		(maximaSeccion, _) = Map.findMax stalls
		(seccionIzq, seccionDerecha) = splitSection maximaSeccion
		seccionesFinales =
		  agregarNuevaSeccion seccionDerecha $
		  agregarNuevaSeccion seccionIzq $
		  sacarUnaSeccionTipo maximaSeccion $
		  stalls
	  in seccionesFinales
	
	splitSection :: Section -> (Section, Section)
	splitSection section =
	  getDistances section
	
	getDistances :: Section -> (Section, Section)
	getDistances (Section n) =
	  (Section $ floor ((toRational n - 1) / 2), Section $ ceiling ((toRational n - 1) / 2))
	
	resolverProblema :: Integer -> Integer -> (Section, Section)
	resolverProblema cantidadDeBaños cantidadDeGente =
	  let
		stallsDespuesDeTodosMenosUno =
		  foldl' (\stalls n -> findStall stalls) (Map.singleton (Section cantidadDeBaños) 1) [1..(cantidadDeGente - 1)]
		(distanciaIzq, distanciaDerecha) = splitSection . fst . Map.findMax $ stallsDespuesDeTodosMenosUno
	  in (distanciaDerecha, distanciaIzq)
	
	main :: IO ()
	main = do
	  print $ resolverProblema 4 2
	  print $ resolverProblema 5 2
	  print $ resolverProblema 6 2
	  print $ resolverProblema 1000 1000
	  print $ resolverProblema 1000 1