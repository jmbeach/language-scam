(define (length items)
  ;@ base case. List is empty
  (cond
    ((eq? items nil) 0)
    (else (+ 1 (length (cdr items))))
  )
)

;@ tail recursive
(define (length items)
  (define (iter store src)
    (cond
      ((null? src) store)
      (else (iter (+ store 1) (cdr items)))
    )
  )
  (iter 0 items)
)

(define (find x items)
  (cond
    ((null? items) #f)
    ((eq? x (car items)) #t)
    (else (find x (cdr items)))
  )
)

(define (collect x items)
  (cond
    ((null? items) nil)
    ((eq? x (car items)))
  )
)

(define (map f items)
  (cond
    ((null? items) nil)
    (else (cons (f (car items)) (map f (con items))))
  )
)

(define (keep p? items)
  (cond
    ((null? items) nil)
    ((p? (car items)) (cons (car items) (keep p? (con items))))
    (else (keep p? (car items)))
  )
)
(println "assignment 2 loaded!")
