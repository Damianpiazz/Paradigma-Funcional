# Uso de IO en Haskell

El **sistema de entrada y salida (IO)** en Haskell permite interactuar con el mundo exterior, como leer datos del teclado, escribir en la consola o manipular archivos.  
A diferencia de otros lenguajes, Haskell es un lenguaje **puramente funcional**, lo que implica que las operaciones de IO deben tratarse de forma especial, ya que introducen efectos secundarios que afectan el entorno de ejecución.

---

## Conceptos Fundamentales

- **Acciones IO**: En Haskell, una acción de IO representa una descripción de una operación de entrada o salida, pero no se ejecuta inmediatamente.  
  Estas acciones tienen el tipo `IO a`, donde `a` es el tipo de valor que la acción devuelve.  
  Por ejemplo, una lectura de texto puede representarse como `IO String`, mientras que una acción que imprime algo en pantalla puede tener el tipo `IO ()`.

- **Ejecución controlada**: Las acciones IO se ejecutan únicamente cuando el programa principal las invoca, garantizando que las funciones puras del programa permanezcan libres de efectos secundarios.  

- **Secuencialidad**: El orden de las operaciones IO es relevante.  
  Haskell permite encadenar acciones de IO para definir la secuencia exacta de ejecución, preservando la claridad y el control sobre los efectos externos.  

- **Pureza funcional**: Aun cuando las acciones IO interactúan con el entorno, estas se encapsulan para mantener el resto del código funcionalmente puro.  
  De esta forma, se conserva la integridad del modelo matemático del lenguaje.

---

## Características Principales del Sistema IO

- **Encapsulación de efectos secundarios**: Las acciones IO representan operaciones con efectos externos sin romper la pureza del lenguaje.  
- **Control declarativo del flujo**: El orden de ejecución se expresa de forma declarativa, manteniendo la legibilidad y el control del programa.  
- **Interacción con el entorno**: Permite comunicarse con archivos, la consola y otros dispositivos sin alterar el comportamiento funcional interno.  
- **Tipos seguros y explícitos**: Cada acción IO especifica el tipo exacto de valor que produce, lo que ayuda a evitar errores y mantener la coherencia del programa.

---

## Operaciones Comunes de IO

- **Entrada estándar**: Lectura de datos desde el teclado o desde otras fuentes externas.  
- **Salida estándar**: Impresión o visualización de información en la consola.  
- **Manejo de archivos**: Lectura, escritura, creación y cierre de archivos dentro del sistema.  
- **Interacción con el sistema**: Ejecución de comandos o comunicación con el entorno operativo cuando es necesario.

---

## Relación entre IO y la Pureza Funcional

El diseño de IO en Haskell se basa en la separación entre **código puro** y **acciones con efectos secundarios**.  
Las funciones puras realizan cálculos deterministas, mientras que las acciones IO representan interacciones con el mundo exterior, manteniendo ambas partes perfectamente diferenciadas.  

Esta separación garantiza que los programas en Haskell sean **predecibles, reutilizables y seguros**, al mismo tiempo que permiten la interacción necesaria con el entorno.

---

## Aplicaciones del IO en Haskell

- Desarrollo de **programas interactivos** que reciben y procesan información del usuario.  
- **Lectura y escritura de archivos** para almacenar o recuperar datos.  
- **Comunicación con otros procesos o sistemas externos**.  
- Implementación de **interfaces de usuario en consola** o entornos controlados.