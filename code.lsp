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




(DEFUN isPrimeAux (number divisor stop)
    (COND
        (
            (ZEROP (mod number divisor)) NIL
            ; El numero pasado como parametro no es primo, ya que "divisor" lo divide.
        )
        (
            (< divisor stop) (isPrimeAux number (+ divisor 1) stop)
            ; Si el divisor es menor al tope de busqueda continuo analizando al numero.
        )
        (   T T ; No se encontraron divisores del numero por lo tanto es primo.
        )
    )
)

(DEFUN isPrime (number) (isPrimeAux number 2 (sqrt number)))


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

(DEFUN sumaPrimos (number) (sumaPrimosAux number 2 1))

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
