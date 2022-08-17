
;; x x x x
;; board is just a sequence of pieces
;; piece is type
;; array of
;; empty squares are just squares that are not occupied
;; so if nobody claims a square then it is empty
;;

;;
;;
;;   transition right -->
;;
;;  <-- transition left
;;                           
;; flat x x x x               x    
;;                            x
;;                            x
;;                            x
;;
;;
;; box  x x   thats it!
;;      x x
;;
;; elbow x       x x x       x x           x
;;       x       x             x       x x x 
;;       x x                   x        
;;
;;                           
;; bend  x         x x       x
;;       x x     x x         x x        x x
;;         x                   x      x x
;;
;;
;; bend      x                  x      x x
;;         x x    x x         x x        x x
;;         x        x x       x        
;;
;; junction  x x x
;;             x
;;  
;;  x         x        x
;;  x x     x x x    x x
;;  x                  x
;;
;;
(define nth
  (lambda (n xs)
    ;; (display "n = ")
    ;; (display n)
    ;; (newline)
    ;; (display "xs = ")
    ;; (display xs)
    ;; (newline)
    (cond ((null? xs) (error "no car cdr of null"))
	  ((= n 1) (car xs))
	  (#t (nth (- n 1) (cdr xs))))))



(define first (lambda (xs) (nth 1 xs)))
(define second (lambda (xs) (nth 2 xs)))
(define third (lambda (xs) (nth 3 xs)))
(define fourth (lambda (xs)(nth 4 xs)))
(define fifth (lambda (xs) (nth 5 xs)))

(define (assoc-value key obj)
  (second (assoc key obj)))

;;------------------ flats ---------------------------
;; flats 
;;
;; state   1                  2
;; 
;;      x x o x               o    
;;                            x
;;                            x
;;                            x
;;

(define (flat? p)
  (equal? (assoc-value 'type p) 'flat))


(define (make-flat x y)
  `((class piece) (type flat) (x ,x) (y ,y) (state 1)))

(define (realise-flat piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 2) ,(- y 0))
		`(,(- x 1) ,(- y 0))
		`(,(- x 0) ,(- y 0))
		`(,(+ x 1) ,(- y 0))))
     ((= state 2)
      (list 	`(,(- x 0) ,(- y 0))
		`(,(- x 0) ,(- y 1))
		`(,(+ x 0) ,(- y 2))
		`(,(+ x 0) ,(- y 3))))
     (else (error "realise-flat")))))
;; flats done



(define (any-squares-list-are-x-y xs x-y)
  (cond
   ((null? xs) #f)
   ((equal? (car xs) x-y) x-y)
   (else (any-squares-list-are-x-y (cdr xs) x-y))))


(define (conflicts-piece-flat? piece x-y)
  (let ((squares (realise-flat piece)))
    (any-squares-list-are-x-y squares x-y)))



(define (left-flat piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-flat piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-flat piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-flat piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "flats only states are 1 and 2"))))))

(define (rotate-left-flat piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "flats only states are 1 and 2"))))))



;; ---------------- box --------------------------------------
;; boxes only have one shape
;;
;; box  x o   thats it!
;;      x x
;;

(define (box? p)
  (equal? (assoc-value 'type p) 'box))

(define (make-box x y)
  `((class piece) (type box) (x ,x) (y ,y) (state 1)))

 
(define (realise-box piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(- x 0) ,(- y 0))
		`(,(- x 1) ,(- y 1))
		`(,(+ x 0) ,(- y 1))))
     (else (error "boxes only have one state")))))
;; box done


(define (conflicts-piece-box? piece x-y)
  (let ((squares (realise-box piece)))
    (any-squares-list-are-x-y squares x-y)))


(define (left-box piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-box piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-box piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-box piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 1))	   
	   (else (error "boxs only have 1 state "))))))


(define (rotate-left-box piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 1))	   
	   (else (error "boxs only have 1 state"))))))

;; ------------------- elbows -------------------------------------
;; elbows have 4 shapes
;;
;; state  1        2           3           4
;;
;;       x o     x o x       x o         o x
;;       x       x             x       x x x 
;;       x x                   x        
;;
(define (elbow? p)
  (equal? (assoc-value 'type p) 'elbow))

(define (make-elbow x y)
  `((class piece) (type elbow) (x ,x) (y ,y) (state 1)))


;; 
(define (realise-elbow piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(- x 1) ,(- y 1))
		`(,(- x 1) ,(- y 2))
		`(,(+ x 0) ,(- y 2))))
     ((= state 2)
      (list 	`(,(- x 1) ,(- y 1))
		`(,(- x 1) ,(- y 0))
		`(,(+ x 0) ,(- y 0))
		`(,(+ x 1) ,(- y 0))))
     ((= state 3)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(- x 0) ,(- y 0))
		`(,(- x 0) ,(- y 1))
		`(,(+ x 0) ,(- y 2))))
     ((= state 4)
      (list 	`(,(+ x 1) ,(- y 0))
		`(,(+ x 1) ,(- y 1))
		`(,(+ x 0) ,(- y 1))
		`(,(- x 1) ,(- y 1))))
     (else (error "realise-elbow")))))
;; elbows done

     

(define (conflicts-piece-elbow? piece x-y)
  (let ((squares (realise-elbow piece)))
    (any-squares-list-are-x-y squares x-y)))


(define (left-elbow piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-elbow piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-elbow piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-elbow piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 3))
	   ((= n 3) (list 'state 4))
	   ((= n 4) (list 'state 1))	   
	   (else (error "elbows only states are 1 and 2 and 3 and 4"))))))


(define (rotate-left-elbow piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 4))
	   ((= n 2) (list 'state 1))
	   ((= n 3) (list 'state 2))
	   ((= n 4) (list 'state 3))	   
	   (else (error "elbows only states are 1 and 2 and 3 and 4"))))))


;; ------------------- left bend ?? -------------------------------------
;;
;; state  1       2      
;;
;;       x o       o x   
;;       x x     x x     
;;         x             
;;
(define (left-bend? p)
  (equal? (assoc-value 'type p) 'left-bend))

(define (make-left-bend x y)
  `((class piece) (type left-bend) (x ,x) (y ,y) (state 1)))

(define (realise-left-bend piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(- x 1) ,(- y 1))
		`(,(- x 0) ,(- y 1))
		`(,(+ x 0) ,(- y 2))))
     ((= state 2)
      (list 	`(,(- x 1) ,(- y 1))
		`(,(- x 0) ,(- y 1))
		`(,(+ x 0) ,(- y 0))
		`(,(+ x 1) ,(- y 0))))
     (else (error "realise-left-bend")))))
;; left-bend done



(define (conflicts-piece-left-bend? piece x-y)
  (let ((squares (realise-left-bend piece)))
    (any-squares-list-are-x-y squares x-y)))



(define (left-left-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-left-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-left-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-left-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "left-bends only states are 1 and 2 "))))))


(define (rotate-left-left-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "left-bends only states are 1 and 2 "))))))



;; ------------------- right bend -------------------------------------
;;
;; state    1        2
;;
;;         o x    x o       
;;         x x      x x   
;;         x            
;;
;;
(define (right-bend? p)
  (equal? (assoc-value 'type p) 'right-bend))

(define (make-right-bend x y)
  `((class piece) (type right-bend) (x ,x) (y ,y) (state 1)))

(define (realise-right-bend piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 0) ,(- y 2))
		`(,(- x 0) ,(- y 1))
		`(,(+ x 1) ,(- y 1))
		`(,(+ x 1) ,(- y 0))))
     ((= state 2)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(- x 0) ,(- y 0))
		`(,(+ x 0) ,(- y 1))
		`(,(+ x 1) ,(- y 1))))
     (else (error "realise-right-bend")))))
;; right-bend done


(define (conflicts-piece-right-bend? piece x-y)
  (let ((squares (realise-right-bend piece)))
    (any-squares-list-are-x-y squares x-y)))



(define (left-right-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-right-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-right-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-right-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "right-bends only states are 1 and 2 "))))))


(define (rotate-left-right-bend piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 1))
	   (else (error "right-bends only states are 1 and 2 "))))))



;; ------------------- t junction------------------------------------
;; 
;; state    1          2        3            4
;;
;;           o         o        o          x o x 
;;         x x       x x x      x x          x 
;;           x                  x             
;;
;; ------------------------------------------------------------------

(define (junction? p)
  (equal? (assoc-value 'type p) 'junction))

(define (make-junction x y)
  `((class piece) (type junction) (x ,x) (y ,y) (state 1)))

(define (realise-junction piece)
  (let ((state (assoc-value 'state piece))
	(x     (assoc-value 'x piece))
	(y     (assoc-value 'y piece)))
    (cond
     ((= state 1)
      (list 	`(,(- x 0) ,(- y 0))
		`(,(- x 0) ,(- y 1))
		`(,(- x 0) ,(- y 2))
		`(,(- x 1) ,(- y 1))))
     ((= state 2)
      (list 	`(,(- x 0) ,(- y 0))
		`(,(- x 1) ,(- y 1))
		`(,(+ x 0) ,(- y 1))
		`(,(+ x 1) ,(- y 1))))
     ((= state 3)
      (list 	`(,(- x 0) ,(- y 0))
		`(,(- x 0) ,(- y 1))
		`(,(- x 0) ,(- y 2))
		`(,(+ x 1) ,(- y 1))))
     ((= state 4)
      (list 	`(,(- x 1) ,(- y 0))
		`(,(+ x 0) ,(- y 0))
		`(,(+ x 1) ,(- y 0))
		`(,(+ x 0) ,(- y 1))))
     (else (error "realise-junction")))))
;; junction done

(define (conflicts-piece-junction? piece x-y)
  (let ((squares (realise-junction piece)))
    (any-squares-list-are-x-y squares x-y)))


(define (left-junction piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (- (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (right-junction piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(list 'x (+ (assoc-value 'x piece) 1))
	(assoc 'y piece)
	(assoc 'state piece)))

(define (down-junction piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(list 'y (- (assoc-value 'y piece) 1))
	(assoc 'state piece)))

(define (rotate-right-junction piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 2))
	   ((= n 2) (list 'state 3))
	   ((= n 3) (list 'state 4))
	   ((= n 4) (list 'state 1))	   
	   (else (error "junctions only states are 1 and 2 and 3 and 4"))))))


(define (rotate-left-junction piece)
  (list (assoc 'class piece)
	(assoc 'type piece)
	(assoc 'x piece)
	(assoc 'y piece)
	(let ((n (assoc-value 'state piece)))
	  (cond
	   ((= n 1) (list 'state 4))
	   ((= n 2) (list 'state 1))
	   ((= n 3) (list 'state 2))
	   ((= n 4) (list 'state 3))	   
	   (else (error "junctions only states are 1 and 2 and 3 and 4"))))))


;; -------------------  -------------------------------------


;; (define p (make-flat 10 10))
;; p
;; nice properties of functional coding ...
;;
;; provable :
;;
;; (equal? p (left-flat (right-flat p)))
;; #t
;;
;; (equal? p (transition-left-flat (transition-right-flat p)))
;; #t
;;

;; ------------------- dispatch methods  -------------------------------------


(define (rotate-right piece)
  (cond
   ((equal? (assoc-value 'type piece) 'flat)
    (rotate-right-flat piece))
   ((equal? (assoc-value 'type piece) 'box)
    (rotate-right-box piece))
   ((equal? (assoc-value 'type piece) 'elbow)
    (rotate-right-elbow piece))
   ((equal? (assoc-value 'type piece) 'left-bend)
    (rotate-right-left-bend piece))
   ((equal? (assoc-value 'type piece) 'right-bend)
    (rotate-right-right-bend piece))
   ((equal? (assoc-value 'type piece) 'junction)
    (rotate-right-junction piece))   
   (else (error "rotate-right"))))


(define (rotate-left piece)
  (cond
   ((equal? (assoc-value 'type piece) 'flat)
    (rotate-left-flat piece))
   ((equal? (assoc-value 'type piece) 'box)
    (rotate-left-box piece))
   ((equal? (assoc-value 'type piece) 'elbow)
    (rotate-left-elbow piece))
      ((equal? (assoc-value 'type piece) 'right-bend)
    (rotate-left-right-bend piece))
   ((equal? (assoc-value 'type piece) 'left-bend)
    (rotate-left-left-bend piece))
   ((equal? (assoc-value 'type piece) 'junction)
    (rotate-left-junction piece))
   (else (error "rotate-left"))))


(define (down piece)
  (cond
   ((equal? (assoc-value 'type piece) 'flat)
    (down-flat piece))
   ((equal? (assoc-value 'type piece) 'box)
    (down-box piece))
   ((equal? (assoc-value 'type piece) 'elbow)
    (down-elbow piece))
   ((equal? (assoc-value 'type piece) 'right-bend)
    (down-right-bend piece))
   ((equal? (assoc-value 'type piece) 'left-bend)
    (down-left-bend piece))
   ((equal? (assoc-value 'type piece) 'junction)
    (down-junction piece))
   (else (error "down"))))


(define (left piece)
  (cond
   ((equal? (assoc-value 'type piece) 'flat)
    (left-flat piece))
   ((equal? (assoc-value 'type piece) 'box)
    (left-box piece))
   ((equal? (assoc-value 'type piece) 'elbow)
    (left-elbow piece))
   ((equal? (assoc-value 'type piece) 'right-bend)
    (left-right-bend piece))
   ((equal? (assoc-value 'type piece) 'left-bend)
    (left-left-bend piece))
   ((equal? (assoc-value 'type piece) 'junction)
    (left-junction piece))
   (else (error "left"))))


(define (right piece)
  (cond
   ((equal? (assoc-value 'type piece) 'flat)
    (right-flat piece))
   ((equal? (assoc-value 'type piece) 'box)
    (right-box piece))
   ((equal? (assoc-value 'type piece) 'elbow)
    (right-elbow piece))
   ((equal? (assoc-value 'type piece) 'right-bend)
    (right-right-bend piece))
   ((equal? (assoc-value 'type piece) 'left-bend)
    (right-left-bend piece))
   ((equal? (assoc-value 'type piece) 'junction)
    (right-junction piece))
   (else (error "right"))))


(define (make-piece type x y)
  (cond
   ((equal? type 'flat) (make-flat x y))
   ((equal? type 'box) (make-box x y))
   ((equal? type 'elbow) (make-elbow x y))
   ((equal? type 'right-bend) (make-right-bend x y))
   ((equal? type 'left-bend) (make-left-bend x y))
   ((equal? type 'junction) (make-junction x y))
   (else (error "make-piece"))))


(define (piece-conflicts? piece x-y)
  (cond
   ((flat? piece) (conflicts-piece-flat? piece x-y))
   ((box? piece) (conflicts-piece-box? piece x-y))
   ((elbow? piece) (conflicts-piece-elbow? piece x-y))
   ((right-bend? piece) (conflicts-piece-right-bend? piece x-y))
   ((left-bend? piece) (conflicts-piece-left-bend? piece x-y))
   ((junction? piece) (conflicts-piece-junction? piece x-y))
   (else (error "piece-conflicts?"))))


(define (realise-piece piece)
  (cond
   ((flat? piece) (realise-flat piece))
   ((box? piece) (realise-box piece))
   ((elbow? piece) (realise-elbow piece))
   ((right-bend? piece)  (realise-right-bend piece))
   ((left-bend? piece)  (realise-left-bend piece))
   ((junction? piece)  (realise-junction piece))
   (else (error "realise-piece"))))





;; ----------------------- game loop ---------------------
(define (one-of xs)
  (let ((len (length xs)))
    (nth (+ 1 (random len)) xs)))


(define (margin)
  (display "                "))


;; iterate over board looking for x y
(define (scan-board x-y board)
  (cond
   ((null? board) #f)
   ((equal? x-y (car board)) x-y)
   (else (scan-board x-y (cdr board)))))




;; if square is occupied then put a # otherwise a dot .
(define (show-board-squares x y board)
  (cond
   ((> x 11) (newline)(margin)(show-board-squares 0 (- y 1) board))
   ((< y 0) (newline))
   (else    
    (cond
     ((scan-board (list x y) board)
      (display " X "))
     (else
      (display " . ")))
    (show-board-squares (+ x 1) y board))))




(define (show-board board)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (margin)(display " T E T R I Z   v  1 . 0") (newline)
  (newline)
  (newline)
  (margin)
  (display "_ _ _ _ _ _ _ _ _ _ _ _")
  (newline)
  (newline)
  (margin)
  (show-board-squares 0 21 board)
  (newline)
  (margin)
  (display "_ _ _ _ _ _ _ _ _ _ _ _")
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
)





;; iterate over board - see if any piece on board conflicts with piece
(define (any-conflicts? piece board)
  (cond
   ((null? board) #f)
   ((piece-conflicts? piece (first board)) (first board))
   (else (any-conflicts? piece (cdr board)))))


(define (combine-piece-and-board piece board)
  (let ((squares (realise-piece piece)))
    (append squares board)))


;; accept - move piece down continuously until stops
(define (game-ask piece board)
  (newline)
  (show-board (combine-piece-and-board piece board))
  (newline)
  (display "Your move :> enter S A D N or M ")	      
  (newline)  
  (display "south!(s) : left(a) : right(d) : rot-right(n) : rot-left(m) : quit(q)> ")
  (let ((response (read)))
    (newline)
    (display "response =[")
    (display response)
    (display "]")
    (cond
     ((or (equal? response 's))
      (display "understood as s for south move")
      (if (any-conflicts? (down piece) board)
	  (begin
	    (game-ask piece board))
	  (game-ask (down piece) board)))
     
     ((or (equal? response 'a))
      (display "understood as a for LEFT")
      (if (any-conflicts? (left piece) board)
	  (begin
	    (newline)(display "cannot go LEFT any more - conflicts detected")
	    (game-ask piece board))
	  (game-ask (left piece) board)))     
      
     ((or (equal? response 'd))
      (display "understood as d for RIGHT")
      (if (any-conflicts? (right piece) board)
	  (begin
	    (newline)(display "cannot go RIGHT any more - conflicts detected")
	    (game-ask piece board))
	  (game-ask (right piece) board)))
     
     ((or (equal? response 'n))
      (display "understood as n for ROTATE rIGHT")
      (if (any-conflicts? (rotate-right piece) board)
	  (begin
	    (newline)(display "cannot rotate RIGHT  - conflicts detected")
	    (game-ask piece board))
	  (game-ask (rotate-right piece) board)))
           
     ((or (equal? response 'm))
      (display "understood as m for ROTATE LEFT")      
      (if (any-conflicts? (rotate-left piece) board)
	  (begin
	    (newline)(display "cannot rotate LEFT - conflicts detected")	    
	    (game-ask piece board))
	  (game-ask (rotate-left piece) board)))

     ((or (equal? response 'f))
      (display "understood as f for COMPLETED MOVE")      
      (if (any-conflicts? (down piece) board)
	  (begin
	    (newline)(display "we are done with this piece !")
	    (game-loop (combine-piece-and-board piece board)))
	  (game-ask (rotate-left piece) board)))
     

     ((or (equal? response 'q))
      (display "understood as q for QUIT")
      (error "quit from game - q - "))     
                 
     (else (newline)
	   (display "...not understood ...")
	   (display response)
	   (newline)
	   (game-ask piece board)))))









     
(define (game-loop board)
  (let ((piece (make-piece (one-of '(flat box elbow right-bend left-bend junction))
			   5 20)))
    (game-ask piece board)))






(define (game)
  (let ((board '((0 0)(1 0) (2 0)(3 0)(4 0) (5 0)(6 0)(7 0)(8 0)(9 0)(10 0)(11 0)
		 (0 1)(0 2) (0 3)(0 4)(0 5) (0 6)(0 7)(0 8)(0 9)(0 10)(0 11)(0 12)(0 13)(0 14)(0 15)(0 16)(0 17)(0 18)(0 19)(0 20)(0 21)
		 (11 1)(11 2) (11 3)(11 4)(11 5) (11 6)(11 7)(11 8)(11 9)(11 10)(11 11)(11 12)(11 13)(11 14)(11 15)(11 16)(11 17)(11 18)(11 19)(11 20)(11 21)
		 (0 21)(1 21) (2 21)(3 21)(4 21) (5 21)(6 21)(7 21)(8 21)(9 21)(10 21)(11 21))))
    (game-loop board)))


		 
		 
(game)












   


   
