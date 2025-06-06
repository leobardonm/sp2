# Resaltador de Sintaxis para C++ en Racket

## 🧾 Descripción General
Este proyecto implementa un resaltador de sintaxis para archivos fuente en C++ utilizando **Racket en paradigma funcional puro**. El programa lee un archivo `.cpp`, `.h`, etc., identifica los **tokens léxicos** usando expresiones regulares y genera un archivo HTML con estilo para visualizar el código con colores distintivos según su categoría.

## ✨ Características Principales
- Implementado 100% en estilo funcional con Racket
- Lectura y análisis léxico de código C++
- Generación automática de HTML con coloreado sintáctico
- Validación de extensiones válidas
- Ignora (omite) tokens que no coincidan con las expresiones regulares definidas

## 🧑‍💻 Requisitos
- Racket 8.16 o superior
- Ejecutar desde línea de comandos o VSCode con soporte Racket

## 📦 Instalación
1. Clona el repositorio:
   ```bash
   git clone https://github.com/leobardonm/sp2.git
   ```
2. Asegúrate de tener Racket en tu PATH.

## 🚀 Uso
1. Ejecuta el archivo desde terminal:
   ```bash
   racket sp2.rkt
   ```
2. Escribe el nombre del archivo a resaltar (ejemplo: `codigo.cpp`)
3. Se generará un archivo `codigo.html` con el resaltado sintáctico

## 📋 Validación de Archivos
El archivo debe tener extensión `.cpp`, `.cc`, `.cxx`, `.hpp`, o `.h`, de lo contrario, el programa mostrará un error.

## 🧠 Categorías Léxicas

Tabla con las categorías reconocidas por el resaltador:

| Categoría                | Color (HTML)                                            | Elementos claves o Regex                                       |
|--------------------------|---------------------------------------------------------|----------------------------------------------------------------|
| **Palabras reservadas**  | <span style="background-color:#9900ff;padding: 2px 10px;border-radius: 3px;">#9900ff</span>  | `if`, `else`, `for`, `return`, `class`, etc.                   |
| **Identificadores**      | <span style="background-color:#32CD32;padding: 2px 10px;border-radius: 3px;">#32CD32</span> | `[a-zA-Z_][a-zA-Z0-9_]*` (excluyendo palabras clave)           |
| **Literales**            | <span style="background-color:#4a86e8;padding: 2px 10px;border-radius: 3px;">#4a86e8</span>  | enteros, reales, `true`, `false`, `'a'`, `"texto"`, `nullptr`     |
| **Directivas**           | <span style="background-color:#ffaa00;padding: 2px 10px;border-radius: 3px;">#ffaa00</span>  | `#include`, `#define`, etc.                                      |
| **Cabeceras**            | <span style="background-color:#00bfff;padding: 2px 10px;border-radius: 3px;">#00bfff</span>  | `<iostream>`, `<vector>`, etc.                             |
| **Operadores**           | <span style="background-color:#ff05ec;padding: 2px 10px;border-radius: 3px;">#ff05ec</span>  | `+`, `-`, `*`, `=`, `==`, `&&`, `->`, etc.                     |
| **Delimitadores**        | <span style="background-color:#f9d208;padding: 2px 10px;border-radius: 3px;">#f9d208</span> | `;`, `,`, `(`, `)`, `{`, `}`, `[`, `]`, `.`                     |
| **Comentarios**          | <span style="background-color:#ff0000;padding: 2px 10px;border-radius: 3px;">#ff0000</span>  | `//`, `/* */`                                                  |
| **Espacios y saltos**    | *Sin color*                                             | `[ ]+` (preservados sin estilo para mantener el formato)       |










## 📁 Estructura del Proyecto
```
.
├── sp2.rkt             ; Código fuente del resaltador
├── ejemplo.cpp         ; Archivo fuente de prueba
├── ejemplo.html        ; Salida HTML resaltada
```

## 👩‍💻 Autores
- José Leobardo Navarro Márquez – A01541324
- Regina Martínez Vázquez – A01385455

## 📄 Licencia
Este proyecto fue realizado como parte de la Actividad Integradora del curso **TC2037 - Implementación de Métodos Computacionales** del Tecnológico de Monterrey.