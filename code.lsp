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


;;; SUMAPRIMOS retorna la suma de numeros primos entre 0 y el numero pasado como parametro.

(DEFUN sumaPrimos (number)
    (COND
        ((< number 2)
            ;; Si el numero es menor a 2 la suma total es 0
            0
        )
        ((ZEROP (MOD number 2))
            ;; Si es par comenzamos desde el numero impar previo
            (sumaPrimosAux (1- number))
        )
        (T
            (sumaPrimosAux number)
        )
    )
)

;;; SUMAPRIMOSAUX retorna la suma de numeros primos entre 0 y el numero pasado como parametro.
;;; Se asume que number es 2 o un numero impar mayor a 2
;;; Solo se analizan los numeros impares, ya que son candidatos a ser primos.
;;; OBS : la secuencia de suma comienza desde el numero primo mas grande al numero primo mas chico,
;;; que se encuentran entre 0 y number.

(DEFUN sumaPrimosAux (number)
    (COND
        ((AND (> number 2) (isPrime number 2 (sqrt number))) ; Basta con verificar divisores entre 2 y la raiz cuadrada del numero
            ;; Si es mayor a 2 y primo, el resultado sera el numero mas los primos que haya
            ;; entre number y 0.
            (+ number (sumaPrimosAux (- number 2)))
            ;; Dado que estamos en la busqueda de numeros primos no tiene sentido analizar los numeros que son pares (mayores a 2)
            ;; ya que no son primos. Al restarle 2 a number, salteamos el analisis de un numero par.
        )

        ((> number 2)
            ;; Si es mayor a 2 y no es primo, la suma total sera la suma de los primos que haya entre
            ;; number y 0
            (sumaPrimosAux (- number 2))
            ;; Al restarle 2 a number, salteamos el analisis de un numero par.
        )
        (T
            ;; Si no es mayor a 2 entonces la suma total es 2
            ;; Se alcanza el final de la sumatoria de numeros primos.
            2
        )
    )
)

;;; ISPRIME retorna T (true) si el numero pasado como parametro es primo (no tiene un divisor entre 2 y stop).
;;; Retorna NIL (false) si el numero pasado como parametro no es primo (tiene al menos 1 divisor entre 2 y stop).
;;; OBS : si "numero=2" no se considera que se tenga como divisor a si mismo.
(DEFUN isPrime (number divisor stop)
    (COND
        ((OR (EQUAL number divisor) (> divisor stop))
            ;; No se encontraron divisores del numero por lo tanto es primo.
            T
        )
        ((ZEROP (mod number divisor))
            ;; El numero pasado como parametro no es primo, ya que "divisor" lo divide.
            NIL
        )
        ((< divisor stop)
            ;; Si el divisor es menor al tope de busqueda, actualizo su valor y continuo el analisis.
            (isPrime number (+ divisor 1) stop)
        )
    )
)


;;;; ---- EJERCICIO 3 ----


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
