#lang racket
(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))  

(define add1
  (lambda (n)
    (+ n 1)))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define leftmost-fixed
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (cond
          ((atom? (leftmost-fixed (car l)))
           (leftmost-fixed (car l)))
          (else (leftmost-fixed (cdr l))))))))

(define leftmost-let
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (let ((a (leftmost-let (car l))))
          (cond
            ((atom? a) a)
            (else (leftmost-let (cdr l)))))))))

(define rember1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (cdr l))
         (else
           (cons (car l) (rember1* a (cdr l))))))
      (else
        (cond
          ((equal? (rember1* a (car l)) (car l)) ; if the list with 'a' removed doesn't change
           (cons (car l) (rember1* a (cdr l))))  ; then recurse
          (else
            (cons (rember1* a (car l)) (cdr l))))))))

(define rember1*-letrec
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (cond
                  ((equal? (R (car l)) (car l)) ; if the list with 'a' removed doesn't change
                   (cons (car l) (R (cdr l))))  ; then recurse
                  (else
                    (cons (R (car l)) (cdr l))))))))) ; otherwise remove 'a'
      (R l))))

(define rember1*-letrec-let
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (cond
                    ((equal? (car l) av)         ; if the list with 'a' removed didn't change
                     (cons (car l) (R (cdr l)))) ; then recurse
                    (else
                      (cons av (cdr l))))))))))  ; otherwise remove 'a'
      (R l))))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (cond
          ((> (depth* (cdr l))
              (add1 (depth* (car l))))
           (depth* (cdr l)))
          (else
            (add1 (depth* (car l)))))))))

(define depth*-let
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*-let (cdr l)))
      (else 
        (let ((a (add1 (depth*-let (car l))))
              (d (depth*-let (cdr l))))
          (cond
            ((> d a) d)
            (else a)))))))

(define depth*-let-2
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
        (let ((d (depth*-let-2 (cdr l))))
          (cond
            ((atom? (car l)) d)
            (else
              (let ((a (add1 (depth*-let-2 (car l)))))
                (cond
                  ((> d a) d)
                  (else a))))))))))

(define depth*-if
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-if (cdr l)))
      (else
        (let ((a (add1 (depth* (car l))))
              (d (depth* (cdr l))))
          (if (> d a) d a))))))

(define depth*-max
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-max (cdr l)))
      (else
        (max
          (add1 (depth*-max (car l)))
          (depth*-max (cdr l)))))))

(define leftmost-letcc
  (lambda (l)
    (call-with-current-continuation
      (lambda (skip)
        (lm l skip)))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else
        (let () ; can also use 'begin'
          (lm (car l) out)
          (lm (cdr l) out))))))

(define leftmost-1314
  (letrec
    ((lm (lambda (l out)
           (cond
             ((null? l) '())
             ((atom? (car l)) (out (car l)))
             (else
               (begin
                 (lm (car l) out)
                 (lm (cdr l) out)))))))
    (lambda (l)
      (call-with-current-continuation
        (lambda (skip)
          (lm l skip))))))

(define leftmost-13142
  (lambda (l)
      (letrec
        ((lm (lambda (l out)
               (cond
                 ((null? l) '())
                 ((atom? (car l)) (out (car l)))
                 (else
                   (begin
                     (lm (car l) out)
                     (lm (cdr l) out)))))))
        (call-with-current-continuation
          (lambda (skip)
            (lm l skip))))))

(define leftmost-131422
  (lambda (l)
    (call-with-current-continuation
      (lambda (skip)
        (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else
                     (begin
                       (lm (car l))
                       (lm (cdr l))))))))
          (lm l))))))

(define rember1*-letcc
  (lambda (a l)
    (letrec
      ((rm (lambda (a l oh)
             (cond
               ((null? l) (oh 'no))
               ((atom? (car l))
                (if (eq? (car l) a)
                  (cdr l)
                  (cons (car l) (rm a (cdr l) oh))))
               (else
                 (let ((new-car
                         (call-with-current-continuation
                           (lambda (oh)
                             (rm a (car l) oh)))))
                   (if (atom? new-car)
                     (cons (car l) (rm a (cdr l) oh))
                     (cons new-car (cdr l)))))))))
      (let ((new-l
              (call-with-current-continuation
                (lambda (oh)
                  (rm a l oh)))))
        (if (atom? new-l)
          l
          new-l)))))

