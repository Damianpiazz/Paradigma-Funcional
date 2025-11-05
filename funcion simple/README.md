# Funciones Simples en Haskell

Las **funciones simples** son la forma más básica y directa de definir comportamiento en Haskell.  
Se utilizan para expresar relaciones entre valores de manera declarativa, sin incluir condiciones, patrones ni estructuras recursivas.

---

## Definición General

Una función simple consta de una **firma de tipo** y un **cuerpo de definición**.  
La firma establece los tipos de los parámetros y el valor de retorno, mientras que el cuerpo describe la operación o transformación que la función realiza sobre sus argumentos.

Estas funciones son **deterministas**: para los mismos valores de entrada, siempre producen el mismo resultado.  
Esto refuerza el principio de **transparencia referencial**, uno de los pilares del paradigma funcional.

---

## Características Principales

- **Simplicidad:** se definen mediante una única ecuación, sin necesidad de estructuras condicionales o recursivas.  
- **Pureza:** no presentan efectos secundarios ni modifican el estado del programa.  
- **Inmutabilidad:** los valores utilizados dentro de la función no cambian durante su ejecución.  
- **Claridad Semántica:** su comportamiento puede entenderse completamente observando su firma de tipo.  

---

## Ventajas de las Funciones Simples

- Facilitan la **legibilidad y el mantenimiento** del código.  
- Promueven la **modularidad**, ya que pueden combinarse fácilmente con otras funciones.  
- Mejoran la **fiabilidad**, al garantizar resultados predecibles sin depender del estado externo.  
- Sirven como **bloques fundamentales** para construir funciones más complejas, como las recursivas o con guardas.  

---

## Aplicaciones

Las funciones simples se utilizan en la mayoría de los programas escritos en Haskell.  
Son ideales para realizar operaciones matemáticas básicas, transformaciones de datos y definiciones iniciales que pueden luego ampliarse mediante composición funcional.