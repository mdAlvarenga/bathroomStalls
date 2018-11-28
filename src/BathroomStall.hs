module BathroomStall where
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


deleteSection :: Section -> Stalls -> Stalls
deleteSection section stalls =
  Map.update (\value -> if value <= 1 then Nothing else Just $ value - 1) section stalls


addNewSection :: Section -> Stalls -> Stalls
addNewSection section stalls =
  Map.insertWith (\new old -> old + new) section 1 stalls


findStall :: Stalls -> Stalls
findStall stalls =
  let
    (bigestSection, _) = Map.findMax stalls
    (leftSection, rightSection) = splitSection bigestSection
    newStalls =
      addNewSection rightSection $
      addNewSection leftSection $
      deleteSection bigestSection $
      stalls
  in newStalls


splitSection :: Section -> (Section, Section)
splitSection section = getDistances section


getDistances :: Section -> (Section, Section)
getDistances (Section n) =
  (Section $ floor ((toRational n - 1) / 2), Section $ ceiling ((toRational n - 1) / 2))


getLastPersonDistances :: Integer -> Integer -> (Section, Section)
getLastPersonDistances stallsNumber peopleNumber =
  let 
    allStallsExceptOne =
      foldl' (\stalls n -> findStall stalls) (Map.singleton (Section stallsNumber) 1) [1..(peopleNumber - 1)]
    (distanceToLeft, distanceToRight) = splitSection . fst . Map.findMax $ allStallsExceptOne
  in (distanceToRight, distanceToLeft)


stringListToListOfIntTuples :: [String] -> [(Integer, Integer)]
stringListToListOfIntTuples [] = []
stringListToListOfIntTuples (x:y:xs) = (read x, read y) : stringListToListOfIntTuples xs


solveForAll :: [(Integer, Integer)] -> [String]
solveForAll [] = []
solveForAll (x:xs) = getResultInText x : solveForAll xs 
      where getResultInText (y,z) = sectionTupleToString $ getLastPersonDistances y z
 
      
processInput :: [String] -> [String]
processInput pairStallsPeople = 
  let listOfPairsWithStallsPeople = stringListToListOfIntTuples $ words $ intercalate " " pairStallsPeople
  in solveForAll listOfPairsWithStallsPeople


addWordToTheBeginning :: String -> Integer -> [String] -> [String]
addWordToTheBeginning _ _ [] = []
addWordToTheBeginning word cases (x:xs) = (word ++ (show cases) ++ ": " ++ x) : addWordToTheBeginning word (cases + 1) xs


sectionToString :: Section -> String
sectionToString (Section section) = show section


sectionTupleToString :: (Section, Section) -> String
sectionTupleToString (Section x, Section y) = tupleToString (x, y)


tupleToString :: (Integer, Integer) -> String
tupleToString (x, y) = show x ++ " " ++ show y


main :: IO ()
main = do  
    putStrLn "Ingresar el nombre del archivo a utilizar como input: "  
    fileName <- getLine  
    -- withFile abre el archivo y cierra luego de usarlo
    withFile    (fileName ++ ".txt") 
                ReadMode 
                    (\handle -> 
                        do
                            -- Obtiene el contenido del archivo
                            content <- hGetContents handle  
                            let contentLines = lines content
                            let solution = processInput (drop 1 contentLines)
                            -- Imprime el output y salva el nuevo archivo con el output
                            let newContent = unlines $ addWordToTheBeginning "Case #" 1 solution
                            putStr newContent
                            writeFile (fileName ++ "_solved.txt") newContent
                    )