# Listas en Haskell

Las **listas** son una de las estructuras de datos más utilizadas en Haskell debido a su simplicidad, flexibilidad y potencia.  
Permiten almacenar y manipular colecciones de elementos del mismo tipo, lo que las convierte en una herramienta fundamental dentro del paradigma funcional.

---

## Características de las Listas

- **Homogéneas:** todos los elementos de una lista deben ser del mismo tipo.  
- **Inmutables:** una vez creada, una lista no puede modificarse; las operaciones sobre ella generan nuevas listas.  
- **Dinámicas:** no poseen un tamaño fijo, pueden expandirse o reducirse según las operaciones aplicadas.  
- **Recursivas:** se definen de manera recursiva, ya que una lista puede estar vacía o contener un elemento seguido de otra lista.

---

## Operaciones Principales

Haskell proporciona una amplia variedad de operaciones para trabajar con listas, entre las más importantes se encuentran:

- **Concatenación:** permite combinar dos o más listas en una sola.  
- **Acceso por índice:** posibilita obtener un elemento específico de la lista según su posición.  
- **Mapeo:** aplica una función a cada elemento de la lista, generando una nueva lista con los resultados.  
- **Filtrado:** crea una lista a partir de los elementos que cumplen una condición determinada.  
- **Reducción:** combina todos los elementos de la lista en un único valor, como una suma o un producto.

---

## Representación y Notación

Las listas se representan mediante **corchetes** que encierran los elementos, separados por comas.  
La lista vacía se denota con `[]`, y las listas pueden contener tanto valores literales como expresiones generadas dinámicamente.

---

## Patrones y Operaciones Avanzadas

Haskell permite manipular listas utilizando patrones y técnicas avanzadas, como:

- **Descomposición:** dividir una lista en su primer elemento (cabeza) y el resto (cola).  
- **Comprensión de listas:** construir listas utilizando una notación declarativa basada en condiciones y generadores.  
- **Recursión:** procesar listas mediante funciones recursivas que se aplican sobre la cabeza y la cola de la lista.

---

## Funciones y Métodos Comunes

El lenguaje incluye un amplio conjunto de funciones predefinidas para el trabajo con listas, tales como:

- **length:** calcula la cantidad de elementos.  
- **reverse:** invierte el orden de los elementos.  
- **take / drop:** seleccionan o descartan una cantidad específica de elementos desde el inicio.  
- **foldr / foldl:** realizan reducciones sobre listas desde la derecha o la izquierda.

---

## Aplicaciones de las Listas

Las listas se utilizan ampliamente en Haskell para:

- Almacenar y procesar colecciones de datos.  
- Manipular secuencias de valores o texto.  
- Representar estructuras funcionales como colas, pilas y árboles.  
- Resolver problemas de forma declarativa y recursiva.