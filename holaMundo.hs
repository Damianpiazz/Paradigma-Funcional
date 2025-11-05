module Main where

-- Definición de funciones simples
sumar :: Int -> Int -> Int
sumar a b = a + b

doblar :: Int -> Int
doblar x = x * 2

-- Función principal (punto de entrada)
main :: IO ()
main = do
    putStrLn "=== Programa Funcional en Haskell ==="
    putStrLn "Hola Mundo desde el Paradigma Funcional"
    putStrLn ""
    putStrLn ("La suma de 3 y 4 es: " ++ show (sumar 3 4))
    putStrLn ("El doble de 5 es: " ++ show (doblar 5))
    putStrLn ""
    putStrLn "Fin de la ejecución."