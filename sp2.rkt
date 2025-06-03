#lang racket

;Resaltador de sintaxis simple para c++ en Racket como lenguaje funcional
;José Leobardo Navarro Márquez A01541324
;Regina Martínez Vazquez A01385455



;Definicion de palabras reservadas como set
(define keyword-set
  (set "for" "int" "return" "if" "else" "while" "class" "public" "private"
       "try" "catch" "throw"))

;Funcion para verificar si un token es un identificador
(define identificador? 
  (lambda (token)
    (and (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token)
         (not (member token keywords))))) ; ejemplo simple de keywords



;EXPRESIONES PARA RECONOCER CUALQUIER TIPO DE LITERAL 
(define (entero? t)
  (regexp-match? #rx"^(0[xX][0-9a-fA-F]+|0[0-7]*|[1-9][0-9]*)$" t))

(define (real? t)
  (regexp-match? #rx"^[+-]?([0-9]*\\.[0-9]+|[0-9]+\\.[0-9]*)([eE][+-]?[0-9]+)?$" t))

(define (booleano? t)
  (regexp-match? #rx"^(true|false)$" t))

(define (caracter? t)
  (regexp-match? #rx"^'(\\.|[^\\'])'$" t))

(define (cadena? t)
  (regexp-match? #rx"^\"(\\\\.|[^\"\\\\])*\"$" t))

(define (nullptr? t)
  (string=? t "nullptr"))









(define operador-set
  (set "+" "-" "*" "/" "%" "++" "--"
       "=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "|=" "^="
       "==" "!=" "<" ">" "<=" ">="
       "&&" "||" "!"
       "&" "|" "^" "~" "<<" ">>"
       "->" "." "::" "[]" "()" "," "?:"
       "sizeof" "typeid" "new" "delete"))


(define (delimitadores)
  (set "{" "}" "(" ")" "[" "]" ";" "," "." "->" "::" "?" ":"))


(define (keyword? token)
  (set-member? keyword-set token))

(define (delimitador? t)
  (set-member? (delimitadores) t))

(define (operador? t)
    (set-member? operador-set t))

(define (literal? t)
  (or (entero? t)
      (real? t)
      (booleano? t)
      (caracter? t)
      (cadena? t)
      (nullptr? t)))
      
(define (resaltar-token t)
  (cond
    [(keyword? t )(string-append "<span style='color:#8e44ad'>" t "</span>")]
    [(identificador? t)(string-append "<span style='color:#ff7b00'>" t "</span>")]
    [(literal? t)(string-append "<span style='color:#1987e1'>" t "</span>")]
    [(operador? t)(string-append "<span style='color:#e74c3c'>" t "</span>")]
    [(delimitador? t)(string-append "<span style='color:#2ecc71'>" t "</span>")]
    [else t]))

(resaltar-token "int")     ; palabra clave
(resaltar-token "for")     ; palabra clave
(resaltar-token "if")     ; palabra clave
(resaltar-token "while")     ; palabra clave
(resaltar-token "class")     ; palabra clave
(resaltar-token "public")     ; palabra clave
(resaltar-token "MAX")     ; identificador
(resaltar-token "10")      ; literal
