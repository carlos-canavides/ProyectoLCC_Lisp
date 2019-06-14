;;;; ---- EJERCICIO 1 ----

;;; TRANS retorna la traspuesta de una matriz, o NIL si la matriz no tiene elementos

(DEFUN trans (matrix)
    ;; Se genera una lista con la primer columna de la matriz
    ;; seguida de la transpuesta de la matriz sin su primer columna
    (if matrix
        (CONS (firstColumn matrix) (trans (butFirst matrix)))
    )
)

;;; FIRSTCOLUMN retorna, como lista, la primera columna de la matriz dada

(DEFUN firstColumn (matrix)
    ;; Se genera una lista con el primer elemento de la la primer fila de la matriz
    ;; seguido de el primer elemento de la fila siguiente (o de la matriz sin su primer fila)
    (if matrix
        (CONS (CAAR matrix) (firstColumn (CDR matrix)))
    )
)

;;; BUTFIRST retorna una copia de la matriz dada pero sin su primera columna

(DEFUN butFirst (matrix)
    ;; Se genera una lista con los elementos siguientes al primero de la primer
    ;; fila de la matriz, seguido de los elementos siguientes al primero de la siguiente fila
    ;; de la matriz (o de la matriz sin su primer fila)
    (if (and matrix (CDAR matrix))
        (CONS (CDAR matrix) (butFirst (CDR matrix)))
    )
)

;;;; ---- EJERCICIO 2 ----

;;; isPrime/3
;;; Retorna T (true) si el numero pasado como parametro es primo (no tiene un divisor entre 2 y stop).
;;; Retorna NIL (false) si el numero pasado como parametro no es primo (tiene al menos 1 divisor entre 2 y stop).
;;; Para determinar si un numero es primo, debemos asegurarnos que ningun valor entre 2 y su raiz cuadrada lo divida.
;;; OBS : si "numero=2" no se considera que se tenga como divisor a si mismo.
(DEFUN isPrime (number divisor stop)
    (COND
        (
            (OR (EQUAL number divisor) (> divisor stop)) T
            ;; No se encontraron divisores del numero por lo tanto es primo.
        )
        (
            (ZEROP (mod number divisor)) NIL
            ;; El numero pasado como parametro no es primo, ya que "divisor" lo divide.
        )
        (
            (< divisor stop) (isPrime number (+ divisor 1) stop)
            ;; Si el divisor es menor al tope de busqueda, actualizo su valor y continuo el analisis.
        )
    )
)

;;; sumaPrimosAux/3
;;; Retorna la suma de numeros primos entre 0 y el numero pasado como parametro.
;;; Si un numero anterior (previous) al pasado como parametro es primo, entonces debe ser incluido en la sumatoria.
;;; Solo se analizan los previous "impares" es decir de la forma (2*n)+1 ya que no tiene sentido analizar aquellos
;;; que son pares.
(DEFUN sumaPrimosAux (number previous n)
    (COND
        (
            (> previous number) 0  ;previous ya no es un valor que se encuantra entre 0 y el numero.
        )
        (
            (isPrime previous 2 (sqrt previous)) (+ previous (sumaPrimosAux number (+ 1 (* 2 n)) (+ n 1)))
            ;; Si previous es un numero primo entonces debe ser ncluido en la sumatoria.
            ;; Llamada recuriva considerando a previous como (2*n)+1
        )
        (
            T (sumaPrimosAux number (+ 1 (* 2 n)) (+ n 1))
            ;; previous no es un número primo, por lo tanto no es incluido en la sumatoria.
        )
    )
)

;;; sumaPrimos/1
;;; Retorna la suma de numeros primos entre 0 y el numero pasado como parametro.
;;; Utiliza la funcion auxiliar sumaPrimosAux pasandole como parametro el primer numero que puede pertenecer
;;; a la secuencia.
;;; Esto se debe a que los en la funcion sumaPrimosAux solo se analizaran los numeros impares, sin embargo 2
;;; es numero par primo que debe ser considerado para la sumatoria.
(DEFUN sumaPrimos (number) (sumaPrimosAux number 2 1))

;;;; Test
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

;;;; ---- EJERCICIO 4 ----

;;; PERM obtiene todas las permuaciones de una lista en el orden lexicografico

(DEFUN perm (listt)
    (permute listt listt)
)

;;; PERMUTE obtiene todas las permutaciones de una lista en el orden lexicografico.
;;; Requiere como segundo argumento una copia de la lista original.

(DEFUN permute (listt leftToPerm)
    (LET ( (next (CAR leftToPerm)) )
    ;; Next sera el proximo elemento en la lista que hay que remover y 
    ;; añadir al comienzo de todas las permutaciones de todos los elementos sin next
        (COND 
            ;; Evaluamos si la lista a permutar tiene uno o mas elementos
            ((AND (CDR listt) leftToPerm)
                ;; Si la lista tiene mas de un elemento, todas las permutaciones posibles seran
                ;; una lista que contiene a cada elemento de la lista como primer elemento
                ;; de las permutaciones de los restantes elementos
                (APPEND
                    ;; Obtenemos las permutaciones de la lista sin un elemento y luego a estas
                    ;; agregamos el elemento removido
                    (addFirstAll next (permute (removeOnce next listt) (removeOnce next listt)))
                    ;; Concatenamos con las permutacions restantes de la lista
                    (permute listt (CDR leftToPerm))) 
            )
            
            ((AND (CAR listt) leftToPerm)
                ;; Si la lista tiene un solo elemento, todas las permutaciones posibles
                ;; son una unica lista conteniendo a ese elemento
                (LIST listt)
            )
            
        )    
    )
    
)

;; ADDFIRSTALL añade el elemento dado como primer elemento de todas las listas pertenecientes
;; a una lista contenedora que es recibida como segundo argumento

(DEFUN addFirstAll (element listt)
    (AND listt 
        ;; Si la lista contenedora no es vacia entonces añade el elemento a la primera lista y continua 
        (CONS (CONS element (CAR listt)) (addFirstAll element (CDR listt))))
)

;;; REMOVEONCE elimina la primera aparicion en una lista del elemento dado. Si el elemento no se encuentra
;;; en la lista, simplemente se retorna la lista intacta

(DEFUN removeOnce (element listt)
    ;; Verifica que la lista no sea vacia, y si la cabeza de esta es igual o distinta al elemento
    (COND
        ((AND listt (EQUAL element (CAR listt)))
            ;; Si la lista no es vacia y el elemento es igual a la cabeza de la lista
            (CDR listt)
        )
        (listt
            ;; Si la lista no es vacia y el elemento es distinta a la cabeza de la lista
            (CONS (CAR listt) (removeOnce element (CDR listt)))
        )
    )
)
