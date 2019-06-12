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

(DEFUN isDivisible (number divisor)
    (COND
        (
            (EQUAL number divisor) NIL
        )
        (
            (ZEROP (mod number divisor)) divisor
        )
        (
            T
                (isDivisible number (+ divisor 1))
        )
    )
)

(write (isDivisible 7 2))
