#lang racket
; sub1 primitive
;
(define sub1
  (lambda (n)
    (- n 1)))

; add1 primitive
;
(define add1
  (lambda (n)
    (+ n 1)))

; atom? primitive
;
(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))  

; member? helper function
;
(define member?
  (lambda (a l)
    (letrec
      ((yes? (lambda (l)
               (cond
                 ((null? l) #f)
                 ((eq? (car l) a) #t)
                 (else (yes? (cdr l)))))))
      (yes? l))))


; Code examples start here
;
(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define sweet-toothL
  (lambda (food)
;    (set! last food)
    (cons food 
          (cons (quote cake)
                (quote ())))))

(define ingredients '())

; The sweet-toothR function builds a list of foods
;
(define sweet-toothR
  (lambda (food)
    (set! ingredients
      (cons food ingredients))
    (cons food (cons 'cake '()))))

; The deep function wraps pizza in n parenthesis
;
;(define deep
;  (lambda (m)
;    (cond
;      ((zero? m) 'pizza)
;      (else
;        (cons (deep (sub1 m)) '())))))

(define Ns1 '())
(define deepR1
  (lambda (n)
    (set! Ns1 (cons n Ns1))
    (deep n)))

(define Ns '())
(define Rs '())
(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))

; The find function finds pizza in Rs
;
;(define find
;  (lambda (n Ns Rs)
;    (letrec
;      ((A (lambda (ns rs)
;            (cond
;              ((= (car ns) n) (car rs))
;              (else
;                (A (cdr ns) (cdr rs)))))))
;      (A Ns Rs))))

(define deepM-tmp
  (lambda (n)
    (if (member? n Ns)
      (find n Ns Rs)
      (deepR n))))

;(set! Ns (cdr Ns))
;(set! Rs (cdr Rs))

;(define deepM
;  (lambda (n)
;    (if (member? n Ns)
;      (find n Ns Rs)
;      (let ((result (deep n)))
;        (set! Rs (cons result Rs))
;        (set! Ns (cons n Ns))
;        result))))

(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deepM (sub1 m)) '())))))

;(define deepM
;  (let ((Rs '())
;        (Ns '()))
;    (lambda (n)
;      (if (member? n Ns)
;        (find n Ns Rs)
;        (let ((result (deep n)))
;          (set! Rs (cons result Rs))
;          (set! Ns (cons n Ns))
;          result)))))

; Better answer for find on empty lists
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= (car ns) n) (car rs))
              (else
                (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

; And a better deepM
;
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

;(define lengthz
;  (lambda (l)
;    (cond
;      ((null? l) 0)
;      (else (add1 (lengthz (cdr l)))))))

;(define lengthz
;  (lambda (l) 0))

;(set! lengthz
;  (lambda (l)
;    (cond
;      ((null? l) 0)
;      (else (add1 (lengthz (cdr l)))))))

;(define lengthz
;  (let ((h (lambda (l) 0)))
;    (set! h
;      (lambda (l)
;        (cond
;          ((null? l) 0)
;          (else (add1 (h (cdr l)))))))
;    h))

(define h1              ; h1 is actually an anonymous name
  (lambda (l) 0))

;(define lengthz
;  (let ()
;    (set! h1
;      (lambda (l)
;        (cond
;          ((null? l) 0)
;          (else (add1 (h1 (cdr l)))))))
;    h1))

(define h2              ; h2 is actually an anonymous name
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (h2 (cdr l)))))))

;(define lengthz
;  (let () h2))

;(define lengthz
;  (let ((h (lambda (l) 0)))
;    (set! h
;      (lambda (l)
;        (cond
;          ((null? l) 0)
;          (else (add1 (h (cdr l)))))))
;    h))

(define L
  (lambda (lengthz)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (lengthz (cdr l))))))))

;(define lengthz
;  (let ((h (lambda (l) 0)))
;    (set! h
;      (L (lambda (arg) (h arg))))
;    h))

(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))

(define lengthz (Y-bang L))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s)) (depth* (cdr s)))
        (else
          (max (add1 (depth* (car s))) (depth* (cdr s))))))))

(define depth* (Y-bang D))

;(define biz
;  (let ((x 0))
;    (lambda (f)
;      (set! x (add1 x))
;      (lambda (a)
;        (if (= a x)
;          0
;          (f a))))))

(define x1 0)               ; anonymous var
(define biz
  (lambda (f)
    (set! x1 (add1 x1))
    (lambda (a)
      (if (= a x1)
        0
        (f a)))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

