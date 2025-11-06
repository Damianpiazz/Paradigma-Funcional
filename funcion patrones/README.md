# Funciones con Patrones en Haskell

Las **funciones con patrones** en Haskell permiten definir el comportamiento de una función en base a la forma o estructura de sus argumentos.  
Este mecanismo, conocido como **pattern matching**, es una característica central del lenguaje que facilita escribir funciones más claras, concisas y expresivas.

---

## Definición General

El uso de patrones en Haskell consiste en comparar los valores de entrada con diferentes formas o estructuras posibles.  
Cada patrón describe un caso particular que la función puede recibir como argumento, y en función de esa coincidencia, se ejecuta una parte específica del código.  
De este modo, una misma función puede definirse mediante múltiples ecuaciones, cada una asociada a un patrón distinto.

Cuando se realiza una llamada a la función, Haskell evalúa los patrones en orden y selecciona el primero que coincida con el valor de entrada.

---

## Estructura de una Función con Patrones

Una función definida mediante patrones se compone de las siguientes partes:

1. **Firma de la función**: Define los tipos de los parámetros y el tipo de valor que retorna.  
2. **Ecuaciones con patrones**: Cada ecuación representa un caso posible del argumento de entrada, asociado a un patrón específico.  
3. **Evaluación secuencial**: Los patrones se evalúan de arriba hacia abajo hasta encontrar una coincidencia válida.  

Esta estructura permite definir el comportamiento de una función de manera declarativa, evitando el uso explícito de condicionales o estructuras de control.

```haskell
-- 1. Firma de la función: define los tipos de los parámetros y el tipo de retorno
descripcionDia :: Int -> String

-- 2. Definición mediante patrones
descripcionDia 1 = "Lunes"
descripcionDia 2 = "Martes"
descripcionDia 3 = "Miércoles"
descripcionDia 4 = "Jueves"
descripcionDia 5 = "Viernes"
descripcionDia 6 = "Sábado"
descripcionDia 7 = "Domingo"
descripcionDia _ = "Número inválido"  -- Patrón comodín: cualquier otro valor
```

---

## Características Principales

- **Definición múltiple**: Una misma función puede tener varias definiciones según el patrón que coincida con el argumento.  
- **Evaluación ordenada**: Los patrones se verifican secuencialmente hasta encontrar el primero que encaje.  
- **Correspondencia estructural**: Los patrones pueden coincidir con listas, tuplas, valores literales o estructuras más complejas.  
- **Descomposición de datos**: Permiten acceder directamente a los componentes internos de una estructura sin necesidad de operaciones adicionales.  
- **Comportamiento declarativo**: El enfoque se centra en describir qué forma debe tener el dato, no cómo procesarlo paso a paso.

---

## Ventajas del Uso de Patrones

- **Claridad y legibilidad**: El código se vuelve más comprensible al reflejar directamente la estructura de los datos.  
- **Simplicidad**: Reduce la necesidad de expresiones condicionales complejas.  
- **Facilidad de mantenimiento**: Cada caso está definido de forma independiente, lo que facilita agregar o modificar comportamientos.  
- **Correspondencia directa con los datos**: Los patrones permiten expresar la lógica del programa en función de la forma de los valores de entrada.  
- **Integración natural con la recursión**: Es especialmente útil en funciones recursivas que trabajan sobre estructuras como listas o árboles.

---

## Aplicaciones Comunes

- Procesamiento de **listas** mediante la distinción entre listas vacías y listas con elementos.  
- Manipulación de **tuplas** y **estructuras de datos** para acceder a sus componentes internos.  
- Implementación de **funciones recursivas** que operan sobre estructuras jerárquicas.  
- Definición de **casos base y casos generales** de forma clara y directa.  
- Simplificación de **funciones declarativas** mediante la descomposición de valores de entrada.