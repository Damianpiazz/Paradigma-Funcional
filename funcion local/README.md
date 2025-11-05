# Funciones Locales en Haskell

Las **funciones locales** son aquellas que se definen dentro de otra función.  
Su propósito es encapsular lógica auxiliar que solo tiene sentido en el contexto de una función principal, evitando su definición global y mejorando la claridad del código.

---

## Definición General

En Haskell, las funciones locales se utilizan para realizar operaciones complementarias o intermedias dentro de una función principal.  
Se definen mediante la palabra clave `where`, ubicada al final del cuerpo de la función.  

Estas funciones tienen acceso directo a los parámetros y variables del entorno donde fueron definidas, lo que permite escribir código más modular y organizado.

El uso de funciones locales permite expresar soluciones complejas de forma más limpia, agrupando toda la lógica relacionada en un único bloque.

---

## Características Principales

- **Ámbito limitado**: solo pueden utilizarse dentro de la función que las contiene.  
- **Acceso al entorno**: pueden utilizar variables y parámetros de la función principal.  
- **Modularidad**: separan tareas auxiliares sin necesidad de crear funciones globales.  
- **Claridad**: ayudan a mantener un código más legible y estructurado.  
- **Encapsulación**: mantienen la lógica interna aislada del resto del programa.

---

## Ventajas del Uso de Funciones Locales

- **Organización**: agrupan la lógica relacionada dentro de una misma definición.  
- **Reutilización interna**: permiten ejecutar operaciones repetidas sin duplicar código.  
- **Mantenimiento sencillo**: los cambios se limitan al contexto donde la función es utilizada.  
- **Legibilidad**: facilitan la comprensión del flujo de la función principal.  

---

## Aplicaciones Comunes

- Definición de **operaciones auxiliares** necesarias para cálculos o validaciones internas.  
- Simplificación de **expresiones complejas** dentro del cuerpo de una función.  
- Creación de **subfunciones** que solo se utilizan dentro de una función mayor.  
- Organización de **código recursivo o con condiciones** en partes más manejables.  