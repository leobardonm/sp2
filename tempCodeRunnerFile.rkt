#lang racket

(require racket/file
         racket/format
         racket/set
         racket/string)

; Resaltador de sintaxis simple para C++ en Racket como lenguaje funcional
; José Leobardo Navarro Márquez A01541324
; Regina Martínez Vazquez A01385455

;; Leer archivo como cadena
(define (leer-archivo-completo ruta)
  (call-with-input-file ruta
    (lambda (in)
      (port->string in))))

;; Escapar caracteres HTML especiales
(define (escape-html text)
  (regexp-replace*
   #rx"[&<>\"]"
   text
   (lambda (ch)
     (cond
       [(string=? ch "&") "&amp;"]
       [(string=? ch "<") "&lt;"]
       [(string=? ch ">") "&gt;"]
       [(string=? ch "\"") "&quot;"]
       [else ch]))))

;; Conjuntos léxicos
(define keyword-set
  (set 
   "alignas" "alignof" "and" "and_eq" "asm"
   "atomic_cancel" "atomic_commit" "atomic_noexcept" "auto" "bitand"
   "bitor" "bool" "break" "case" "catch"
   "char" "char8_t" "char16_t" "char32_t" "class"
   "compl" "concept" "const" "consteval" "constexpr"
   "constinit" "const_cast" "continue" "co_await" "co_return"
   "co_yield" "decltype" "default" "delete" "do"
   "double" "dynamic_cast" "else" "enum" "explicit"
   "export" "extern" "false" "float" "for"
   "friend" "goto" "if" "inline" "int"
   "long" "mutable" "namespace" "new" "noexcept"
   "not" "not_eq" "nullptr" "operator" "or"
   "or_eq" "private" "protected" "public" "register"
   "reinterpret_cast" "requires" "return" "short" "signed"
   "sizeof" "static" "static_assert" "static_cast" "struct"
   "switch" "synchronized" "template" "this" "thread_local"
   "throw" "true" "try" "typedef" "typeid"
   "typename" "union" "unsigned" "using" "virtual"
   "void" "volatile" "wchar_t" "while" "xor" "xor_eq"))

(define operador-set
  (set "+" "-" "*" "/" "%" "++" "--"
       "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "|=" "^="
       "==" "!=" "<" ">" "<=" ">="
       "&&" "||" "!"
       "&" "|" "^" "~" "<<" ">>"
       "->" "." "::" "[]" "()" "," "?:"
       "sizeof" "typeid" "new" "delete"))

(define delimitador-set
  (set "{" "}" "(" ")" "[" "]" ";" "," "." "->" "::" "?" ":"))

;; Clasificadores léxicos
(define (keyword? token) (set-member? keyword-set token))
(define (operador? token) (set-member? operador-set token))
(define (delimitador? token) (set-member? delimitador-set token))
(define (identificador? token)
  (and (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token)
       (not (keyword? token))))

(define (entero? t)
  (regexp-match? #rx"^[+-]?(0[xX][0-9a-fA-F]+|[1-9][0-9]*|0)$" t))

(define (real? t)
  (regexp-match? #rx"^[+-]?([0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*)([eE][+-]?[0-9]+)?$" t))

(define (booleano? t)
  (regexp-match? #rx"^(true|false)$" t))

(define (caracter? t)
  (regexp-match? #rx"^'(\\.|[^\\'])'$" t))

(define (cadena? t)
  (regexp-match? (pregexp "^\"(\\\\.|[^\"\\\\])*\"$") t))

(define (nullptr? t)
  (string=? t "nullptr"))

(define (literal? t)
  (or (entero? t) (real? t) (booleano? t) (caracter? t) (cadena? t) (nullptr? t)))

(define (directiva-preprocesador? t)
  (regexp-match? #rx"^#(include|define|ifdef|ifndef|endif|if|elif|else|undef|pragma)$" t))

(define (cabecera? t)
  (regexp-match? #rx"^<[^>]+>$" t))

(define (cabecera-usuario? t)
  (regexp-match? #rx"^\"[^\"]+\"$" t))

;; Tokenización
(define patron-token
  (pregexp
   (string-join
    '(
      "/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/"
      "//[^\n]*"
      "\"(\\\\.|[^\"\\\\])*\""
      "'(\\\\.|[^'\\\\])'"
      "[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?"
      "0[xX][0-9a-fA-F]+"
      "[0-9]+"
      "#[a-zA-Z_]+"
      "<[^>]+>"
      "\"[^\"]+\""
      "[a-zA-Z_][a-zA-Z0-9_]*"
      "<<=|>>=|==|!=|<=|>=|->|::|&&|\\|\\||<<"
      "[+\\-*/%=<>&|^~!]"
      "[(){}\\[\\];,.?:]"
      "[ \t\n\r]+"
     )
    "|")))

(define (tokenizar-flujo flujo)
  (regexp-match* patron-token flujo))

;; HTML span
(define (span color text)
  (string-append "<span style='color:" color "'>" text "</span>"))

;; Resaltado por token
(define (resaltar-token t)
  (cond
    [(regexp-match? #rx"^/\\*" t) (span "#ff0000" (escape-html t))]
    [(regexp-match? #rx"^//" t) (span "#ff0000" (escape-html t))]
    [(regexp-match? #px"^[ \t\n\r]+$" t) t]
    [(cadena? t) (span "#4a86e8" (escape-html t))]
    [(caracter? t) (span "#4a86e8" (escape-html t))]
    [(booleano? t) (span "#4a86e8" (escape-html t))]
    [(real? t) (span "#4a86e8" (escape-html t))]
    [(entero? t) (span "#4a86e8" (escape-html t))]
    [(nullptr? t) (span "#4a86e8" (escape-html t))]
    [(keyword? t) (span "#9900ff" (escape-html t))]
    [(directiva-preprocesador? t) (span "#ffaa00" (escape-html t))]
    [(cabecera? t) (span "#00bfff" (escape-html t))]
    [(cabecera-usuario? t) (span "#00bfff" (escape-html t))]
    [(identificador? t) (span "#32CD32" (escape-html t))]
    [(operador? t) (span "#ff05ec" (escape-html t))]
    [(delimitador? t) (span "#f9d208" (escape-html t))]
    [else (escape-html t)]))

(define (resaltar-flujo flujo)
  (apply string-append (map resaltar-token (tokenizar-flujo flujo))))

(define (generar-html entrada salida)
  (define flujo (leer-archivo-completo entrada))
  (define contenido-html
    (string-append
     "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Resaltado</title></head><body style='font-family: monospace; white-space: pre;'>\n"
     (resaltar-flujo flujo)
     "</body></html>"))
  (call-with-output-file salida
    (lambda (out)
      (fprintf out "~a" contenido-html))
    #:exists 'replace))

;; Función principal
(define (main)
  (define entrada
    (let ()
      (display "Ingrese el nombre del archivo a resaltar: ")
      (flush-output)
      (read-line)))
  (define salida
    (string-append
     (path->string (path-replace-extension entrada "")) ".html"))
  (if (file-exists? entrada)
      (begin
        (generar-html entrada salida)
        (printf "Archivo resaltado generado: ~a\n" salida))
      (printf "Error: El archivo '~a' no existe\n" entrada)))

(main)