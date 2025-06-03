#lang racket

(require racket/file
         racket/format
         racket/set
         racket/string
        )

;Resaltador de sintaxis simple para c++ en Racket como lenguaje funcional
;José Leobardo Navarro Márquez A01541324
;Regina Martínez Vazquez A01385455


(define (span color text)
  (string-append "<span style='color:" color "'>" text "</span>"))



;Leer archivo linea por linea
(define (leer-archivo ruta)
  (call-with-input-file ruta
    (lambda (in)
      (let loop ((lines '()))
        (if (eof-object? (peek-char in))
            (reverse lines)
            (loop (cons (read-line in) lines)))))))



(define (tokenizar-linea linea)
  (regexp-split
   (pregexp "(\\s+|(?=[;(){}\\[\\],.+\\-*/=<>!&|])|(?<=[;(){}\\[\\],.+\\-*/=<>!&|]))")
   linea))


;Definicion de palabras reservadas como set
(define keyword-set
  (set "for" "int" "return" "if" "else" "while" "class" "public" "private"
       "try" "catch" "throw"))

;Funcion para verificar si un token es un identificador
(define identificador? 
  (lambda (token)
    (and (regexp-match? #rx"^[a-zA-Z_][a-zA-Z0-9_]*$" token)
         (not (set-member?  keyword-set token))))) ; ejemplo simple de keywords



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
  (regexp-match? (pregexp "^\"(\\\\.|[^\"\\\\])*\"$") t))

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


(define delimitador-set
  (set "{" "}" "(" ")" "[" "]" ";" "," "." "->" "::" "?" ":"))

(define (delimitador? t)
  (set-member? delimitador-set t))


(define (keyword? token)
  (set-member? keyword-set token))



(define (operador? t)
    (set-member? operador-set t))

(define (literal? t)
  (or (entero? t)
      (real? t)
      (booleano? t)
      (caracter? t)
      (cadena? t)
      (nullptr? t)))

(define (is-line-comment? token)
  (and (string-prefix? "//" token)))

(define (is-block-comment-start? token)
  (string-prefix? "/*" token))

(define (is-block-comment-end? token)
  (string-suffix? "*/" token))

(define (is-comment? token)
  (or (is-line-comment? token)
      (is-block-comment-start? token)
      (is-block-comment-end? token)))



(define (resaltar-token t)
  (cond
    [(regexp-match? #rx"^\\s+$" t) t] ; espacios
    [(keyword? t) (span "#9900ff" t)]
    [(identificador? t) (span "#00ff00" t)]
    [(literal? t) (span "#4a86e8" t)]
    [(operador? t) (span "#ff05ec" t)]
    [(delimitador? t) (span "#ffff00" t)]
    [(is-line-comment? t) (span "#ff0000" t)]
    [(is-block-comment-start? t) (span "#ff0000" t)]
    [(is-block-comment-end? t) (span "#ff0000" t)]
    [(is-comment? t) (span "#ff0000" t)]
    [(regexp-match? #rx"^\\s+$" t) t] ; conserva espacios sin resaltado
    [else t]))


(define (resaltar-linea linea)
  (if (string-prefix? "//" (string-trim linea))
      (string-append (span "#ff0000" linea) "\n") ; toda la línea es un comentario
      (let* ((tokens (tokenizar-linea linea))
             (resaltado (apply string-append (map resaltar-token tokens))))
        (string-append resaltado "\n"))))

(define (generar-html entrada salida)
  (define lineas (leer-archivo entrada))
  (define contenido-html
    (string-append
     "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Resaltado</title></head><body style='font-family: monospace; white-space: pre;'>\n"
     (apply string-append
            (map (lambda (l) (string-append (resaltar-linea l) "\n")) lineas))
     "</body></html>"))

  (call-with-output-file salida
    (lambda (out)
      (fprintf out "~a" contenido-html))
    #:exists 'replace))



(generar-html "ejemplo.cpp" "resaltado.html")