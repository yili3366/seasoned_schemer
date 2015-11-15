#lang racket
(define add1
  (lambda (x) (+ x 1)))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x ) ((f f) x)))))))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else
            (add1 (length (cdr l)))))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? a (car lat)) (mr (cdr lat)))
              (else
               (cons (car lat) (mr (cdr lat))))))))
     lat)))

(define multirember-letrec
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else
                  (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

(define multirember-letrec-2
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
             (cond
               ((null? lat) '())
               ((eq? a (car lat)) (mr (cdr lat)))
               (else
                 (cons (car lat) (mr (cdr lat))))))))
       (mr lat))))

(define id
  (lambda (b)
    b))

(define multirember-letrec-3
  (letrec
    ((mr (lambda (a lat)
           (cond
             ((null? lat) '())
             ((eq? (car lat) a) (mr a (cdr lat)))
             (else
               (cons (car lat) (mr a (cdr lat))))))))
    mr))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(define member-letrec?
  (lambda (a l)
    ((letrec
       ((yes? (lambda (l)
                (cond
                  ((null? l) #f)
                  ((eq? (car l) a) #t)
                  (else (yes? (cdr l)))))))
     yes?)
    l)))

(define member-letrec-2?
  (lambda (a l)
    (letrec
      ((yes? (lambda (l)
               (cond
                 ((null? l) #f)
                 ((eq? (car l) a) #t)
                 (else (yes? (cdr l)))))))
      (yes? l))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) 
       (union (cdr set1) set2))
      (else
        (cons (car set1) (union (cdr set1) set2))))))

(define union-letrec
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((member? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set))))))))
      (U set1))))

(define union-letrec-protected
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set)))))))
       (M?
         (lambda (a lat)
           (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else
               (M? a (cdr lat)))))))
      (U set1))))

(define union-letrec-protected-12
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set)))))))
       (M?
         (lambda (a lat)
           (letrec
             ((N? (lambda (lat)
                    (cond
                      ((null? lat) #f)
                      ((eq? (car lat) a) #t)
                      (else
                        (N? (cdr lat)))))))
             (N? lat)))))
      (U set1))))

(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else
                (W (car lat) (cdr lat)))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(define two-in-a-row-2?
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? a (car lat)) #t)
            (else
              (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) '())
              (else
                (cons (+ sss (car tup))
                      (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))
