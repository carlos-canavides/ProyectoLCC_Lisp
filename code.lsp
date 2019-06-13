;; ---- EJERCICIO 1 ----

;; TRANS retorna la traspuesta de una matriz, o NIL si la matriz no tiene elementos

(DEFUN trans (matrix)
    (if matrix
        (CONS (firstColumn matrix) (trans (butFirst matrix)))
    )
)

;; FIRSTCOLUMN retorna, como lista, la primera columna de la matriz dada

(DEFUN firstColumn (matrix) 
    (if matrix
        (CONS (CAAR matrix) (firstColumn (CDR matrix)))
    )
)

;; BUTFIRST retorna una copia de la matriz dada pero sin su primera columna

(DEFUN butFirst (matrix)
    (if (and matrix (CDAR matrix))
        (CONS (CDAR matrix) (butFirst (CDR matrix)))
    )
)

;; ---- EJERCICIO 2 ----

;; isPrime/3
;; Retorna T (true) si el numero pasado como parametro es primo (no tiene un divisor entre 2 y stop).
;; Retorna NIL (false) si el numero pasado como parametro no es primo (tiene al menos 1 divisor entre 2 y stop).
;; Para determinar si un numero es primo, debemos asegurarnos que ningun valor entre 2 y su raiz cuadrada lo divida.
;; OBS : si "numero=2" no se considera que se tenga como divisor a si mismo.
(DEFUN isPrime (number divisor stop)
    (COND
        (
            (OR (EQUAL number divisor) (> divisor stop)) T
            ; No se encontraron divisores del numero por lo tanto es primo.
        )
        (
            (ZEROP (mod number divisor)) NIL
            ; El numero pasado como parametro no es primo, ya que "divisor" lo divide.
        )
        (
            (< divisor stop) (isPrime number (+ divisor 1) stop)
            ; Si el divisor es menor al tope de busqueda, actualizo su valor y continuo el analisis.
        )
    )
)

;; sumaPrimosAux/3
;; Retorna la suma de numeros primos entre 0 y el numero pasado como parametro.
;; Si un numero anterior (previous) al pasado como parametro es primo, entonces debe ser incluido en la sumatoria.
;; Solo se analizan los previous "impares" es decir de la forma (2*n)+1 ya que no tiene sentido analizar aquellos
;; que son pares.
(DEFUN sumaPrimosAux (number previous n)
    (COND
        (
            (> previous number) 0   ;previous ya no es un valor que se encuantra entre 0 y el numero.
        )
        (
            (isPrime previous 2 (sqrt previous)) (+ previous (sumaPrimosAux number (+ 1 (* 2 n)) (+ n 1)))
            ; Si previous es un numero primo entonces debe ser ncluido en la sumatoria.
            ; Llamada recuriva considerando a previous como (2*n)+1
        )
        (
            T (sumaPrimosAux number (+ 1 (* 2 n)) (+ n 1))
            ; previous no es un número primo, por lo tanto no es incluido en la sumatoria.
        )
    )
)

;; sumaPrimos/1
;; Retorna la suma de numeros primos entre 0 y el numero pasado como parametro.
;; Utiliza la funcion auxiliar sumaPrimosAux pasandole como parametro el primer numero que puede pertenecer
;; a la secuencia.
;; Esto se debe a que los en la funcion sumaPrimosAux solo se analizaran los numeros impares, sin embargo 2
;; es numero par primo que debe ser considerado para la sumatoria.
(DEFUN sumaPrimos (number) (sumaPrimosAux number 2 1))

;; Test
(write (sumaPrimos 0))
(write (sumaPrimos 1))
(write (sumaPrimos 2))
(write (sumaPrimos 3))
(write (sumaPrimos 4))
(write (sumaPrimos 5))
(write (sumaPrimos 6))
(write (sumaPrimos 7))
(write (sumaPrimos 8))
(write (sumaPrimos 9))
(write (sumaPrimos 10))
