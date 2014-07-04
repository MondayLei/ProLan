
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (> low high) 
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (st)(string-append st suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([ith (remainder n (length xs))])
              (car (list-tail xs ith)))]))

;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
(define funny-number-stream 
  (letrec ([f (lambda (x)
               (if (= 0 (remainder x 5))
                   (cons (- 0 x) (lambda() (f (+ x 1))))
                   (cons x (lambda() (f (+ x 1))))))])
  (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (even? x)
                    (cons "dan.jpg" (lambda () (f (+ x 1))))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))))])
  (lambda () (f 0))))

;; 7
(define (stream-add-zero s)
  (letrec ([f (lambda(x)
                (lambda() (cons (cons 0 (car (x))) (f (cdr (x))))))])
  (lambda () (f s))))

;; 8
(define (cycle-lists xs ys)
  (letrec([f (lambda(n)
               (lambda() (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (f (+ n 1)))))])
  (lambda () (f 0))))

;; 9
(define (vector-assoc v vec)
  (letrec  ([vlen (vector-length vec)]
            [f (lambda(index)
                 (cond[(= index vlen) #f]
                      [(pair? (vector-ref vec index))
                       (if (equal? (car (vector-ref vec index)) v)
                           (vector-ref vec index)
                           (f (+ index 1)))]
                      [#t (f (+ index 1))]))])
  (lambda () (f 0))))

;; 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [index 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin (vector-set! memo index new-ans)
                                   (set! index (remainder (+ index 1) n))
                                   new-ans)
                            #f)))))])
    f))

;; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-lees e1 do e2)
     (let ([t1 e1])
       (letrec ([loop (lambda (t2)
                        (if (>= t2 t1)
                            #t
                            (loop e2)))])
         (loop e2)))]))