(define (not x)
  (if x #f #t))

(define zero?
  (lambda (x) (= 0 x)))

(define (even? x)
  (= (mod x 2) 0))

(define (odd? x)
  (not (even? x)))

(define (null? lst)
  (eq? lst '()))

(define (curry func arg1)
    (lambda (arg)
        (apply func arg1 arg)))

(define (compose f g)
  (lambda (arg)
    (f (g arg))))

(define (foldr func lst base)
  (if (null? lst)
    base
    (func (car lst) (foldr func (cdr lst) base))))

(define (map func lst)
  (if (null? lst)
    '()
    (cons (func (car lst)) (map func (cdr lst)))))

(define (filter pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

(define (flip func)
  (lambda (x y)
    (func y x)))

(define (reverse lst)
  (foldr (flip cons) '() lst))

(define (length lst)
  (if (null? lst)
    0
    (+ 1 (length (cdr lst)))))

(define (id x) x)

(define (append lst x)
  (foldr cons lst x))

(define (max lst)
  (foldr (lambda (a b) (if (> a b) a b))
         (cdr lst)
         (car lst)))

(define (sum lst)
    (foldr + lst 0))

(define (product lst)
  (foldr * lst 1))

(define (and lst)
  (foldr && lst #t))

(define (or lst)
  (foldr || lst #f))

(define (make-range start end)
  (if (> start end)
    '()
    (cons start (make-range (+ start 1) end))))

(define (factorial n)
  (product (make-range 2 n)))

"Library loaded correctly"
