#lang racket

(require racket/file
         racket/format
         racket/set
         racket/string)

; Resaltador de sintaxis simple para C++ en Racket como lenguaje funcional
; José Leobardo Navarro Márquez A01541324
; Regina Martínez Vazquez A01385455

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilidades
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (span color text)
  (string-append "<span style='color:" color "'>" text "</span>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leer archivo línea por línea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (leer-archivo ruta)
  (call-with-input-file ruta
    (lambda (in)
      (let loop ((lines '()))
        (if (eof-object? (peek-char in))
            (reverse lines)
            (loop (cons (read-line in 'any) lines))))))) ; ← importante mantener 'any para tabs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definiciones de sets y funciones de categorías léxicas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define keyword-set
  (set "for" "int" "return" "if" "else" "while" "class"
       "public" "private" "try" "catch" "throw"))

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

(define (keyword? token)
  (set-member? keyword-set token))

(define (operador? token)
  (set-member? operador-set token))

(define (delimitador? token)
  (set-member? delimitador-set token))

(define (identificador? token)
  (and (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token)
       (not (keyword? token))))

(define (entero? t)
  (regexp-match? #rx"^(0[xX][0-9a-fA-F]+|[1-9][0-9]*|0)$" t))

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
  (or (entero? t)
      (real? t)
      (booleano? t)
      (caracter? t)
      (cadena? t)
      (nullptr? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokenización con expresión compuesta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define patron-token
  (pregexp
   (string-join
    '(
      "//.*"                                ; comentario línea
      "\"(\\\\.|[^\"\\\\])*\""              ; cadena
      "'(\\\\.|[^'\\\\])'"                  ; carácter
      "[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?"    ; flotante
      "0[xX][0-9a-fA-F]+"                   ; entero hex
      "[0-9]+"                              ; entero decimal
      "[a-zA-Z_][a-zA-Z0-9_]*"              ; identificador o keyword
      "<<=|>>=|==|!=|<=|>=|->|::|&&|\\|\\||<<" ; operadores dobles
      "[+\\-*/%=<>&|^~!]"                   ; operadores simples
      "[(){}\\[\\];,.?:]"                   ; delimitadores
      "\\s+"                                ; espacios y tabs
     )
    "|")))

(define (tokenizar-linea linea)
  (regexp-match* patron-token linea))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resaltado por token
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resaltar-token t)
  (cond
    [(regexp-match? #px"^\\s+$" t) t]                 ; espacios
    [(regexp-match? #px"^//" t) (span "#ff0000" t)]   ; comentario línea
    [(cadena? t) (span "#4a86e8" t)]
    [(caracter? t) (span "#4a86e8" t)]
    [(booleano? t) (span "#4a86e8" t)]
    [(real? t) (span "#4a86e8" t)]
    [(entero? t) (span "#4a86e8" t)]
    [(nullptr? t) (span "#4a86e8" t)]
    [(keyword? t) (span "#9900ff" t)]
    [(identificador? t) (span "#00ff00" t)]
    [(operador? t) (span "#ff05ec" t)]
    [(delimitador? t) (span "#ffff00" t)]
    [else t])) ; cualquier otro símbolo sin clasificar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procesamiento de líneas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resaltar-linea linea)
  (apply string-append (map resaltar-token (tokenizar-linea linea))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generación de HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generar-html entrada salida)
  (define lineas (leer-archivo entrada))
  (define contenido-html
    (string-append
     "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Resaltado</title></head><body style='font-family: monospace; white-space: pre;'>\n"
     (apply string-append (map (lambda (l) (string-append (resaltar-linea l) "\n")) lineas))
     "</body></html>"))
  (call-with-output-file salida
    (lambda (out)
      (fprintf out "~a" contenido-html))
    #:exists 'replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejecutar resaltador
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(generar-html "ejemplo.cpp" "resaltado.html")
