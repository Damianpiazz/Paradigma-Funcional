# Paradigma-Funcional

Repositorio correspondiente a la materia **Paradigmas de Programación (UTN)**.  
En este proyecto se estudia y ejemplifica el **Paradigma Funcional** utilizando el lenguaje **Haskell**.

## Introducción al Paradigma Funcional

El **Paradigma Funcional** pertenece a la familia de los **paradigmas declarativos**, donde se indica **qué se quiere hacer**, pero no **cómo** hacerlo paso a paso.

## Principios Fundamentales del Paradigma Funcional

### Declarativo  
El paradigma funcional pertenece a la familia de los **paradigmas declarativos**, en los cuales se especifica **qué se desea obtener** sin detallar **cómo** debe lograrse paso a paso.  
A diferencia del paradigma imperativo, no se describe una secuencia de instrucciones, sino que se definen expresiones que transforman datos y producen resultados.  
Este enfoque se centra en la **definición de relaciones y funciones**, no en el control del flujo de ejecución.  
Gracias a ello, los programas funcionales son más **expresivos**, más **fáciles de razonar** y menos propensos a errores derivados de la gestión del estado o del orden de ejecución.

### Función (Pureza)  
En el paradigma funcional, las funciones se comportan como **funciones matemáticas puras**: para los mismos argumentos, siempre producen el mismo resultado.  
Su ejecución no depende de variables externas ni genera efectos sobre el entorno.  
Esta pureza garantiza **determinismo, previsibilidad y facilidad de prueba**, ya que cada función depende únicamente de sus parámetros de entrada.  
Las funciones puras son la base para la **composición funcional** y la **transparencia referencial**, promoviendo la **reutilización** y la **modularidad** del código.

### Inmutabilidad de las Variables  
Las variables en programación funcional son **inmutables**, es decir, una vez asignado un valor, este no puede modificarse.  
En lugar de cambiar el estado de una variable, se generan nuevos valores o estructuras a partir de las existentes.  
Esta característica elimina la posibilidad de **inconsistencias** debidas a modificaciones inesperadas y facilita la **razonabilidad, concurrencia y depuración** del código.  
La inmutabilidad refuerza la pureza funcional y evita la necesidad de controlar **efectos colaterales** derivados de la mutación del estado.

### Transparencia Referencial  
Una expresión posee **transparencia referencial** cuando puede ser reemplazada por su valor sin alterar el comportamiento del programa.  
En un entorno funcional, esto significa que las llamadas a funciones pueden sustituirse por sus resultados sin generar efectos no deseados.  
Esta propiedad permite **razonamiento matemático, optimización automática y evaluación perezosa**, ya que el sistema puede manipular y reorganizar expresiones sin modificar su significado.  
La transparencia referencial es una consecuencia directa de la **pureza de las funciones** y la **inmutabilidad de los datos**.

### Efectos Laterales  
Un **efecto lateral** ocurre cuando una función realiza una acción que altera el estado del programa o del entorno externo, como modificar una variable global, escribir en un archivo o imprimir en pantalla.  
En el paradigma funcional, los efectos laterales se **evitan o aíslan** mediante estructuras de control específicas (por ejemplo, **mónadas** en Haskell).  
La ausencia de efectos laterales asegura que las funciones sean **predecibles, reutilizables y fácilmente testeables**, favoreciendo la **consistencia lógica** del sistema.

## Definición de Funciones en Haskell

Las funciones en Haskell se definen en un **script (.hs)** indicando:

- Tipo de función (dominio y codominio).
- La ecuación que la describe.

Ejemplo simple:

```haskell
cuadrado :: Int -> Int
cuadrado x = x * x
```

## Tipos de Datos

- **Estándar:** `Int`, `Float`, `Bool`, `Char`, `String`, `a` (genérico).
- **Derivados:** pares ordenados, listas y funciones.

## Tipos de Funciones

### Simples
Definidas mediante una sola ecuación.

```haskell
cuadrado x = x * x
```

### Dobles y Triples
Son funciones que aceptan dos o más argumentos.

```haskell
suma x y = x + y
promedio x y z = (x + y + z) / 3
```

### Por Guardas (Condicionales)
Permiten definir casos según condiciones lógicas.

```haskell
minimo x y
  | x < y     = x
  | otherwise = y
```

### Locales (with / where / let)
Definen subfunciones internas.

```haskell
cuarta x = cuad x * cuad x
  where cuad x = x * x
```

### Por Patrones
Permiten definir el comportamiento de la función según la estructura de los argumentos.

```haskell
and' True True = True
and' _ _       = False
```

### Recursivas
Se definen en función de sí mismas.

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

### De Alto Orden
Son funciones que reciben otras funciones como argumento o las devuelven.

```haskell
map (\x -> x * x) [1,2,3,4]
filter even [1..10]
takeWhile (<5) [1..10]
```
## Listas en Haskell

Las **listas** son colecciones homogéneas de elementos.

```haskell
listaVacia = []
listaUnitaria = [5]
listaSimple = [2, 4, 6, 8]
```

### Características

- Se pueden **concatenar** con `++`
- Obtener **cabeza**: `head lista`
- Obtener **cola**: `tail lista`
- Calcular **longitud**: `length lista`
- Acceder por índice: `lista !! n`

### Listas de Listas

```haskell
listas = [[1,2,3], [4,5], [6]]
```
Cada elemento es una lista, y pueden aplicarse funciones de alto orden para operar sobre ellas.

Ejemplo:

```haskell
map sum [[1,2,3], [4,5], [6]]
-- Resultado: [6,9,6]
```
## Entrada y Salida por Consola (I/O)

En Haskell, las operaciones de entrada/salida son **acciones del tipo IO**.

```haskell
main = do
  putStrLn "Ingrese su nombre:"
  nombre <- getLine
  putStrLn ("Hola, " ++ nombre ++ "!")
```

## Compilación y Ejecución de Archivos Haskell

### Crear y Guardar el Script

Guarda tu archivo con extensión `.hs`  
Ejemplo: `main.hs`

### Compilar con GHC (Glasgow Haskell Compiler)

```bash
ghc main.hs -o programa
```

### Ejecutar el Programa

```bash
./programa
```

### Ejecutar Directamente con GHCi (Intérprete interactivo)

```bash
ghci main.hs
```
Luego puedes probar las funciones directamente escribiendo su nombre.

## Tipos de Archivos en un Proyecto Haskell

- **.hs:** archivos de código fuente.
- **.hi:** archivos de interfaz generados por el compilador.
- **.o:** archivos objeto intermedios.
- **.exe / binario:** programa ejecutable final.

## Conceptos Clave

- **Reducción:** proceso de sustituir la función por su definición y simplificar hasta su forma normal.
- **Evaluación de funciones:** puede seguir **orden normal** (evaluar de afuera hacia adentro) u **orden aplicativo** (evaluar argumentos primero).