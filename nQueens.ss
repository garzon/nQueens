#lang scheme

(define reduce (lambda(f init list)
  (if
   (null? list)
   init
   (reduce f (f init (car list)) (cdr list))
  )
))

(define flatmap (lambda(f list)
 (reduce append 
  '()
  (map f list)
 )
))

(define queens (lambda(n)
  (define make-pos (lambda(x y) (list x y)))
  (define pos->x (lambda(pos) (car pos)))
  (define pos->y (lambda(pos) (car (cdr pos))))
  
  (define valid? (lambda(board)
    (define check? (lambda(pos1 pos2)
      (cond
        ((= (pos->x pos1) (pos->x pos2)) #f)
        ((= (pos->y pos1) (pos->y pos2)) #f)
        ((= (abs (- (pos->x pos2) (pos->x pos1))) (abs (- (pos->y pos2) (pos->y pos1)))) #f)
        (else #t)
      )
    ))
    (reduce
     (lambda(init flag) (and init flag))
     #t
     (map (lambda(others-pos) (check? (car board) others-pos)) (cdr board))
    )
  ))
  
  (define queen-col (lambda (x)
    (cond
     ((< x 0) '(()))
     (else 
      (filter valid?
        (flatmap 
          (lambda(board) 
            (map (lambda(row) (cons (make-pos row x) board)) (range 0 n))
          )
          (queen-col (- x 1))
        )
      )
     )
   )
  ))
  
  (queen-col (- n 1))
))

(queens 6)