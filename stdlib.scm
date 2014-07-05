(define (not x) (if x #f #t))

(define (null? obj) (if (eqv? obj '()) #t #f))

(define (list . objs) objs)

(define (id obj) obj)

(define (flip func)
  (lambda (x y) (func x y)))

(define (curry func x)
  (lambda (y) (apply func (cons x (list y)))))

(define (compose f g)
  (lambda (y) (f (apply g y))))

(define zero?     (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num)  (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr func acc xs)
  (if (null? xs)
      acc
      (func (car xs) (foldr func acc (cdr xs)))))

(define (foldl func acc xs)
  (if (null? xs)
      acc
      (foldl func (func acc (car xs)) (cdr xs))))

(define fold foldl)

(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum . xs)     (fold + 0 xs))
(define (product . xs) (fold * 1 xs))

(define (and . xs) (fold && #t xs))
(define (or . xs)  (fold || #f xs))

(define (max first . rest)
  (fold (lambda (old new)
          (if (> old new) old new)) first rest))

(define (min first . rest)
  (fold (lambda (old new)
          (if (< old new) old new)) first rest))

(define (length xs)
  (fold (lambda (acc x) (+ acc 1)) 0 xs))

(define (reverse xs)
  (fold (flip cons) '() xs))

(define (mem-helper pred op)
  (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(define (memq obj xs)
  (fold (mem-helper (curry eq? obj) id) #f xs))
(define (memv obj xs)
  (fold (mem-helper (curry eqv? obj) id) #f xs))
(define (member obj xs)
  (fold (mem-helper (curry equal? obj) id) #f xs))

(define (assq obj xs)
  (fold (mem-helper (curry eq? obj) car) #f xs))
(define (assv obj xs)
  (fold (mem-helper (curry eqv? obj) car) #f xs))
(define (assoc obj xs)
  (fold (mem-helper (curry equal? obj) car) #f xs))

(define (map func xs)
  (foldr (lambda (x y) (cons (func x) y)) '() xs))

(define (filter pred xs)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() xs))
