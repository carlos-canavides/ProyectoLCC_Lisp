(DEFUN getFirstColumn (matrix)
    (COND
        (
            (NULL matrix) '()
        )
        (
            T
                (LET ((FirstElem (CAR (CAR matrix))))
                    (CONS FirstElem (getFirstColumn (CDR matrix)))
                )
        )
    )
)



;; isPrimeAux/3
;; Retorna T (true) si el numero pasado como parametro no tiene un divisor entre 2 y stop.
;; Retorna NIL (false) si el numero pasado como parametro tiene al menos 1 divisor entre 2 y stop.
;; Si el numero es "2" no se considera que tiene como divisor a si mismo.
(DEFUN isPrimeAux (number divisor stop)
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
            (< divisor stop) (isPrimeAux number (+ divisor 1) stop)
            ; Si el divisor es menor al tope de busqueda continuo analizando al numero.
        )
    )
)

;; isPrime/1
;; Retorna T (true) si es numero pasado como parametro es primo.
;; Retorna NIL (false) si el numero pasado como parametro no es primo.
;; Utiliza la funcion auxiliar isPrimeAux pasandole como parametro el numero y su raiz cuadrada.
;; Esto se debe a que para determinar si un numero es primo, debemos asegurarnos que ningun valor
;; entre 2 y su raiz cuadrada lo divida.
(DEFUN isPrime (number) (isPrimeAux number 2 (sqrt number)))


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
            (isPrime previous) (+ previous (sumaPrimosAux number (+ 1 (* 2 n)) (+ n 1)))
            ; Si previous es un numero primo entonces debe incluido en la sumatoria.
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
