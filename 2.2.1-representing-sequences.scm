(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (1- n))))

(let ((squares (list 1 4 9 16 25)))
  (list-ref squares 3)) ;; => 16


(define (length-v1 items)
  (if (null? items)
      0
      (+ 1 (length-v1 (cdr items)))))

(let ((odds (list 1 3 5 7)))
  (length-v1 odds)) ;; => 4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))

  (length-iter items 0))

(let ((odds (list 1 3 5 7)))
  (length odds)) ;; => 4


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(let ((squares (list 1 4 9 16 25))
      (odds (list 1 3 5 7)))
  (append squares odds)) ;; => (1 4 9 16 25 1 3 5 7)


(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34)) ;; => (34)


(define (reverse items)
  (define (reverse-iter a result)
    (if (null? a)
        result
        (reverse-iter (cdr a) (cons (car a) result))))

  (reverse-iter items (list)))

(reverse (list 1 4 9 16 25)) ;; => (25 16 9 4 1)


(define (first-denomination denominations)
  (car denominations))

(define (except-first-denomination denominations)
  (cdr denominations))

(define (no-more? denominations)
  (null? denominations))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(let ((us-coins (list 50 25 10 5 1))
      (uk-coins (list 100 50 20 10 5 2 1 0.5)))
  (cc 100 us-coins)) ;; => 292


(define (map proc items)
  (if (null? items)
      (list)
      (cons (proc (car items))
            (map proc (cdr items)))))

(map
 (lambda (x) (* x x))
 (list 1 2 3 4)) ;; => (1 4 9 16)


(define (for-each proc items)
  (unless (null? items)
    (proc (car items))
    (for-each proc (cdr items))))

(for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))
