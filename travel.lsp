(defun travel (l)
    (travelhelper (permute l) l)
)

(defun travelhelper (l min)
    (cond
        ((not l) min)
        ((< (totaldistancetraveled (car l)) (totaldistancetraveled min)) (travelhelper (cdr l) (car l)))
        (t (travelhelper (cdr l) min))
    )
)

; takes a list that is made up of lists that contain
; city names and their coordinates, prints out the
; list with only city names seperated by newline
;
(defun printcities (l)
    (cond
        ((not l) t)
        (t (or (format t "~a~%" (getname (car l))) (printcities (cdr l))))
    )
)

; takes a list of list containing cities and locations
; and calculates the total distance that is required
; to visit all of the cities
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

(defun permute (x)
    (cond
        ((not x) nil)
        ((not (cdr x)) (cons x nil))
        (t (for_each x x))
    )
)


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


(defun remove_first (item list)
    (cond
        ((not list) nil)
        ((eq item (car list)) (cdr list))
        (t (cons (car list) (remove_first item (cdr list))))
    )
)


(defun cons_each (item lists)
    (cond
        ((not lists) nil)
        (t (cons
            (cons item (car lists))
            (cons_each item (cdr lists)))
        )
    )
)

; given a list in the format of (name (x . y))
; retieves the name of the city
;
(defun getname (l)
    (car l)
)

; given a list in the format of (name (x . y))
; retieves the name of the city
;
(defun getx (l)
    (car (car (cdr l)))
)

; given a list in the format of (name (x . y))
; retieves the name of the city
;
(defun gety (l)
    (cdr (car (cdr l)))
)

; auxiliary function for squaring numbers 
; 
(defun square (n)
    (* n n)
)

; auxiliary function for determining the
; length of a list
;
(defun len (l)
    (cond
        ((not l) 0)
        (t (+ 1 (len (cdr l))))
    )
)

(printcities (travel (quote 
			(
				(Atlanta (50 . 50))
				(Orlando (100 . -225))
				(Knoxville (60 . 100))
				(Dothan (10 . -75))
			)
)))

(printcities (travel (quote 
			(
				(Knoxville (60 . 100))
				(Atlanta (50 . 50))
				(Orlando (100 . -225))
				(Dothan (10 . -75))
			)
)))