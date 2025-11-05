# Funciones con Guardas en Haskell

Las **funciones con guardas** son un tipo de función que permite expresar condiciones de manera clara y estructurada.  
En lugar de utilizar estructuras condicionales tradicionales, las guardas ofrecen una forma más declarativa de definir diferentes comportamientos de una función según los valores de entrada.

---

## Definición General

Una **guarda** es una expresión booleana que se evalúa para determinar qué resultado debe devolver una función.  
Cada guarda se asocia a una condición y a una expresión de resultado.  
Cuando una de las condiciones se cumple, la función devuelve el valor correspondiente.  
Si ninguna guarda resulta verdadera, se utiliza una condición final denominada **otherwise**, que actúa como caso por defecto.

Las guardas se evalúan de arriba hacia abajo, en el orden en que están escritas, hasta encontrar la primera que sea verdadera.

---

## Estructura de una Función con Guardas

Una función con guardas está compuesta por:

1. **Firma de la función**: Define los tipos de los parámetros y el tipo de retorno.  
2. **Definición con guardas**: Contiene una serie de condiciones que determinan el comportamiento de la función.  
3. **Caso por defecto (otherwise)**: Se utiliza cuando ninguna de las condiciones anteriores se cumple.

Esta estructura permite expresar la lógica de decisión de manera más legible y cercana al razonamiento lógico o matemático.

---

## Características Principales

- **Claridad semántica**: Las guardas expresan condiciones de manera ordenada y fácil de leer.  
- **Evaluación secuencial**: Las condiciones se verifican en orden hasta que una resulta verdadera.  
- **Uso de otherwise**: Permite definir un caso general o por defecto, evitando errores por falta de coincidencias.  
- **Alternativa al condicional**: Reemplazan la necesidad de estructuras anidadas como `if-then-else`.  
- **Legibilidad funcional**: Facilitan la comprensión de funciones con múltiples casos o decisiones complejas.

---

## Ventajas del Uso de Guardas

- **Simplifican el código**: Evitan el uso excesivo de condicionales y bloques anidados.  
- **Favorecen el estilo declarativo**: Describen qué condiciones deben cumplirse sin detallar cómo se evalúan.  
- **Mejoran la legibilidad**: Permiten expresar diferentes escenarios de manera clara y estructurada.  
- **Mantenimiento más sencillo**: Facilitan la incorporación o modificación de casos específicos sin alterar la estructura general.

---

## Aplicaciones Comunes

- Definición de **comportamientos condicionales** en funciones con múltiples resultados posibles.  
- Implementación de **clasificaciones o comparaciones** según rangos o valores.  
- Control de flujo en **funciones recursivas**, determinando cuándo continuar o detener la recursión.  
- Reemplazo de estructuras condicionales complejas por expresiones más expresivas y limpias.