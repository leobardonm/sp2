#lang racket

(require racket/file
         racket/format
         racket/set
         racket/string)

; Resaltador de sintaxis simple para C++ en Racket como lenguaje funcional
; José Leobardo Navarro Márquez A01541324
; Regina Martínez Vazquez A01385455



;; Leer archivo línea por línea


(define (leer-archivo ruta)
  (call-with-input-file ruta
    (lambda (in)
      (let loop ((lines '()))
        (if (eof-object? (peek-char in))
            (reverse lines)
            (loop (cons (read-line in 'any) lines))))))) ; ← importante mantener 'any para tabs

;; Leer archivo como un solo flujo de caracteres
(define (leer-archivo-completo ruta)
  (call-with-input-file ruta
    (lambda (in)
      (port->string in))))

;; Definiciones de sets y funciones de categorías léxicas


(define keyword-set
  (set 
   "alignas"       "alignof"       "and"           "and_eq"        "asm"
   "atomic_cancel" "atomic_commit" "atomic_noexcept" "auto"         "bitand"
   "bitor"         "bool"          "break"         "case"          "catch"
   "char"          "char8_t"       "char16_t"      "char32_t"      "class"
   "compl"         "concept"       "const"         "consteval"     "constexpr"
   "constinit"     "const_cast"    "continue"      "co_await"      "co_return"
   "co_yield"      "decltype"      "default"       "delete"        "do"
   "double"        "dynamic_cast"  "else"          "enum"          "explicit"
   "export"        "extern"        "false"         "float"         "for"
   "friend"        "goto"          "if"            "inline"        "int"
   "long"          "mutable"       "namespace"     "new"           "noexcept"
   "not"           "not_eq"        "nullptr"       "operator"      "or"
   "or_eq"         "private"       "protected"     "public"        "register"
   "reinterpret_cast" "requires"   "return"        "short"         "signed"
   "sizeof"        "static"        "static_assert" "static_cast"   "struct"
   "switch"        "synchronized"  "template"      "this"          "thread_local"
   "throw"         "true"          "try"           "typedef"       "typeid"
   "typename"      "union"         "unsigned"      "using"         "virtual"
   "void"          "volatile"      "wchar_t"       "while"         "xor"
   "xor_eq"))


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
  (or (entero? t)
      (real? t)
      (booleano? t)
      (caracter? t)
      (cadena? t)
      (nullptr? t)))


;; Tokenización con expresión compuesta actualizada
(define patron-token
  (pregexp
   (string-join
    '(
      "/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/"  ; comentario multilínea más preciso
      "//[^\n]*"                            ; comentario línea (hasta fin de línea)
      "\"(\\\\.|[^\"\\\\])*\""             ; cadena
      "'(\\\\.|[^'\\\\])'"                 ; carácter
      "[0-9]+\\.[0-9]*([eE][+-]?[0-9]+)?"  ; flotante
      "0[xX][0-9a-fA-F]+"                  ; entero hex
      "[0-9]+"                             ; entero decimal
      "[a-zA-Z_][a-zA-Z0-9_]*"             ; identificador o keyword
      "<<=|>>=|==|!=|<=|>=|->|::|&&|\\|\\||<<" ; operadores dobles
      "[+\\-*/%=<>&|^~!]"                  ; operadores simples
      "[(){}\\[\\];,.?:]"                  ; delimitadores
      "[ \t\n\r]+"                         ; espacios y saltos de línea
     )
    "|")))

(define (tokenizar-linea linea)
  (regexp-match* patron-token linea))

(define (tokenizar-flujo flujo)
  (regexp-match* patron-token flujo))

;; Resaltado del flujo completo
(define (resaltar-flujo flujo)
  (apply string-append (map resaltar-token (tokenizar-flujo flujo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resaltado por token
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (span color text)
  (string-append "<span style='color:" color "'>" text "</span>"))


(define (resaltar-token t)
  (cond
    [(regexp-match? #rx"^/\\*" t) (span "#ff0000" t)]   ; Comentario multilínea
    [(regexp-match? #rx"^//" t) (span "#ff0000" t)]     ; Comentario línea
    [(regexp-match? #px"^[ \t\n\r]+$" t) t]            ; espacios y saltos de línea
    [(cadena? t) (span "#4a86e8" t)]                    ; cadena
    [(caracter? t) (span "#4a86e8" t)]                  ; carácter
    [(booleano? t) (span "#4a86e8" t)]                  ; booleano
    [(real? t) (span "#4a86e8" t)]                      ; real
    [(entero? t) (span "#4a86e8" t)]                    ; entero
    [(nullptr? t) (span "#4a86e8" t)]                   ; nullptr
    [(keyword? t) (span "#9900ff" t)]                   ; palabra reservada
    [(identificador? t) (span "#32CD32" t)]             ; identificador
    [(operador? t) (span "#ff05ec" t)]                  ; operador
    [(delimitador? t) (span "#f9d208" t)]               ; delimitador
    [else t])) ; cualquier otro símbolo sin clasificar

;; Generación de HTML considerando el flujo completo
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

;; Función para solicitar el nombre del archivo al usuario
(define (solicitar-archivo)
  (display "Ingrese el nombre del archivo a resaltar: ")
  (flush-output)
  (read-line))

;; Función principal del programa
(define (main)
  (let* ([archivo-entrada (solicitar-archivo)]
         [nombre-base (path->string (path-replace-extension archivo-entrada ""))]
         [archivo-salida (string-append nombre-base ".html")])
    (if (file-exists? archivo-entrada)
        (begin
          (generar-html archivo-entrada archivo-salida)
          (printf "Archivo resaltado generado: ~a\n" archivo-salida))
        (printf "Error: El archivo '~a' no existe\n" archivo-entrada))))

;; Ejecutar el programa
(main)
