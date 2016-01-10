; Nathan Gibson
; January 11, 2015
;
; Program solves the traveling salesman problem by taking a list of cities
; and coordinates, permuting all possible orderings of the list and then
; choosing the ordering that requires the least distance to be traveled. 
;
; example usage: (printcities (travel '((Atlanta (50 . 50))
;            (Orlando (100 . -225))(Knoxville (60 . 100))(Dothan (10 . -75)))))



; returns the minimum distance path by passing all permutations to 
; the travelhelper function
; 
(defun travel (l)
    (travelhelper (permute l) l)
)

; finds the minimum distance of all permutations
; 
(defun travelhelper (l min)
    (cond
        ((not l) min)
        ((< (totaldistancetraveled (car l)) (totaldistancetraveled min)) (travelhelper (cdr l) (car l)))
        (t (travelhelper (cdr l) min))
    )
)

; takes the detailed list and prints out the city names followed by newlines
; 
(defun printcities (l)
    (cond
        ((not l) t)
        (t (or (format t "~a~%" (getname (car l))) (printcities (cdr l))))
    )
)

; calculates the distance traveled in one trip
; 
(defun totaldistancetraveled (l)
    (cond
        ((eq (len l) 2) (distancebetween (getx (car l)) (gety (car l)) 
                                        (getx (car (cdr l))) (gety (car (cdr l)))))
        (t 
            (+ 
                (totaldistancetraveled (cdr l))
                (distancebetween (getx (car l)) (gety (car l)) (getx (car (cdr l))) (gety (car (cdr l))))
            )
        )                
    )
)

; returns the distance between two points
;
(defun distancebetween (x1 y1 x2 y2)
    (sqrt 
        (+ 
            (square 
                (- 
                    x2 x1
                 )
            ) 
            (square 
                (- 
                    y2 y1
                 )
            )
         )
     )
)

; all permutation code from:
; http://cscnew.columbusstate.edu/eckart/classes/cpsc5135/topics/topic_05.shtml
;
(defun permute (x)
    (cond
        ((not x) nil)
        ((not (cdr x)) (cons x nil))
        (t (for_each x x))
    )
)

; uses each element of "member" as basis for a reduced permute problem
; on the other elements of "list". Returns a list of lists.
; 		
(defun for_each (member list)
    (cond
        ((not member) nil)
        (t
            (append
                (cons_each 
                    (car member)
                    (permute
                        (remove_first
                            (car member)
                            list
                    ))
                )
                (for_each (cdr member) list)
            )
        )
    )
)

; removes only the first occurrence of "item" from the "list".
;
(defun remove_first (item list)
    (cond
        ((not list) nil)
        ((eq item (car list)) (cdr list))
        (t (cons (car list) (remove_first item (cdr list))))
    )
)

; cons the "item" onto each sublist in the "list".
;
(defun cons_each (item lists)
    (cond
        ((not lists) nil)
        (t (cons
            (cons item (car lists))
            (cons_each item (cdr lists)))
        )
    )
)

; pulls name out of the list
;
(defun getname (l)
    (car l)
)

; pulls x coordinate out of the list
;
(defun getx (l)
    (car (car (cdr l)))
)

; pulls y cooridinate out of the list
;
(defun gety (l)
    (cdr (car (cdr l)))
)

; auxiliary function for squaring numbers 
; 
(defun square (n)
    (* n n)
)

; auxiliary length function
; 
(defun len (l)
    (cond
        ((not l) 0)
        (t (+ 1 (len (cdr l))))
    )
)
