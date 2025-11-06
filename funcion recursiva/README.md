# Funciones Recursivas en Haskell

Las **funciones recursivas** son una de las herramientas más poderosas y características de Haskell.  
La recursión es el mecanismo mediante el cual una función se llama a sí misma para resolver un problema dividiéndolo en subproblemas más simples, hasta llegar a un caso base que detiene el proceso.

---

## Definición General

En Haskell, la recursión reemplaza a las estructuras iterativas presentes en otros lenguajes.  
Una función recursiva define su comportamiento en función de sí misma, utilizando distintos patrones o condiciones que determinan cuándo continuar y cuándo detenerse.

El diseño de una función recursiva implica dos componentes esenciales:

1. **Caso base**: representa la condición de finalización que impide la recursión infinita y devuelve un resultado directo.  
2. **Caso recursivo**: define cómo se reduce el problema llamando nuevamente a la misma función con una entrada más simple.

---

## Estructura de una Función Recursiva

```haskell
-- 1. Firma de la función: define los tipos de entrada y salida
factorial :: Int -> Int

-- 2. Definición de la función con casos base y recursivo
factorial 0 = 1              -- Caso base: condición de finalización
factorial n = n * factorial (n - 1)  -- Caso recursivo: llamada a sí misma con una entrada menor
```
---

## Características Principales

- **Descomposición de problemas**: permiten dividir un problema complejo en instancias más pequeñas del mismo problema.  
- **Ausencia de estado mutable**: no requieren bucles ni variables modificables; cada llamada opera sobre nuevos valores.  
- **Uso natural de patrones**: las listas y estructuras de datos se procesan recurriendo sobre su cabeza y su cola.  
- **Determinismo y pureza**: cada llamada recursiva produce siempre el mismo resultado para los mismos argumentos.

---

## Ventajas del Enfoque Recursivo

- **Claridad y elegancia**: la recursión permite expresar soluciones de forma directa y declarativa.  
- **Compatibilidad con la evaluación perezosa**: Haskell optimiza la ejecución de llamadas recursivas sin necesidad de evaluar todo de inmediato.  
- **Fácil composición**: las funciones recursivas pueden combinarse con otras funciones puras para resolver problemas más complejos.  
- **Evita efectos laterales**: la recursión no requiere modificar el estado del programa, manteniendo la inmutabilidad del paradigma funcional.

---

## Aplicaciones Comunes

- Procesamiento y transformación de **listas** y **árboles**.  
- Implementación de **operaciones matemáticas** como factoriales, sumatorias o potencias.  
- Definición de **algoritmos declarativos**, donde el flujo de ejecución se expresa a través de la estructura del problema.  
- Modelado de **estructuras repetitivas** sin necesidad de bucles explícitos.