# Listas de Listas en Haskell

Las **listas de listas** son estructuras donde cada elemento es, a su vez, una lista.  
Se utilizan para representar **datos jerárquicos o matriciales**, como tablas o estructuras anidadas.

---

## Características Principales

- **Jerárquicas:** permiten organizar datos en niveles (por ejemplo, filas y columnas).  
- **Flexibles:** las sublistas pueden tener longitudes distintas.  
- **Recursivas:** se procesan fácilmente mediante funciones recursivas.

---

## Operaciones Comunes

- **Concatenación:** unir varias sublistas en una sola estructura.  
- **Acceso anidado:** obtener elementos dentro de sublistas.  
- **Mapeo y reducción:** aplicar funciones a cada sublista o combinar resultados.  

---

## Representación

Una lista de listas se denota como una lista cuyos elementos también son listas, por ejemplo: una estructura similar a una matriz o tabla.  

---

## Ejemplo en Código

```haskell
-- Ejemplo de Listas de Listas en Haskell
-- Representación y operaciones básicas sobre estructuras matriciales

main :: IO ()
main = do
    let matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]   -- Definición de una lista de listas
    let primeraFila = head matriz                    -- Acceso a la primera sublista
    let segundaColumna = map (!! 1) matriz           -- Obtiene la segunda columna
    let sumaFilas = map sum matriz                   -- Suma los elementos de cada fila
    let sumaTotal = sum (concat matriz)              -- Suma de todos los elementos de la matriz

    putStrLn ("Matriz completa: " ++ show matriz)
    putStrLn ("Primera fila: " ++ show primeraFila)
    putStrLn ("Segunda columna: " ++ show segundaColumna)
    putStrLn ("Suma por filas: " ++ show sumaFilas)
    putStrLn ("Suma total: " ++ show sumaTotal)
```

---

## Aplicaciones

- **Matrices y tablas de datos.**  
- **Estructuras jerárquicas**, como árboles o colecciones anidadas.  