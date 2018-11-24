-- monada lista

-- return2 :: a -> m a
-- return2 x = [x]

-- fmap2 :: (a -> b) -> m a -> m b
-- fmap2 f xs = map f xs

-- join2 :: m(m a) -> m a
-- join2 xxs = concat xxs

-- bind :: m a -> (a -> m b) -> m b
-- bind m f = join2 (fmap2 f m)  
-- -- O  (concat Map)



-- -- ejemplo de los cofres:
-- solve keys [] result = return result
-- solve keys chests result = 
-- 	do 
--         c <- filter(canOpen keys) chests 
--         -- //Es el primer cofre que puedo abrir con la llave que tengo, y está ordenado lexicograficamente.

-- 		solve 	(updateKeys keys chests) 
--                 (chests \\ [c]) 
--                 -- // barra barra elimina las ocurrencias comparables por igual igual...
-- 				result ++ [c]


-- lo llamo con solve keys cs []


-- ////////////////////////
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
  
