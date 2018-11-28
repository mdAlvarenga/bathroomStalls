module Main where
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Void
import System.IO

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
    putStrLn "Ingresar el nombre del archivo a utilizar como input: "  
    fileName <- getLine  
    putStrLn $ "Por ahora, ingresaste el siguiente del archivo: " ++ fileName
    -- withFile abre el archivo y cierra luego de usarlo
    withFile    (fileName ++ ".txt") 
                ReadMode 
                    (\handle -> 
                        do
                            -- Obtiene el contenido del archivo
                            content <- hGetContents handle  
                            let contentLines = lines content
                            -- Manipula los datos
                                solution = processInput (drop 1 contentLines) 
                            -- Imprime el output y salva el nuevo archivo con el output
                            let newContent = unlines solution
                            putStr newContent
                            writeFile (fileName ++ "_solved.txt") newContent
                    )

