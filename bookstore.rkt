#lang sicp
;DATA STRUCTURES
(define inventory (cons (cons 'title (list 'author 'genre 'price 'noInStock 'noSold)) '()))
(define author-count (cons (cons 'author 'count) '()))
(define genre-count (cons (cons 'genre 'count) '()))

;ADD-BOOK <- nanditha
(define (add-book title author genre price noInStock)
  (define (get-last-node inventory)
    (if (null? (cdr inventory))
        inventory
        (get-last-node (cdr inventory))))
  (define (make-book-node title author genre price noInStock)
    (list (cons title (list (cons 'author author)
                            (cons 'genre genre)
                            (cons 'price price)
                            (cons 'noInStock noInStock)
                            (cons 'noSold 0)))))
  (define (add-author author author-count)
    (if (not (eq? author (caar author-count)))
        (if (null? (cdr author-count))
            (set-cdr! author-count (list (cons author 0)))
            (add-author author (cdr author-count)))))
  (define (add-genre genre genre-count)
    (if (not (eq? genre (caar genre-count)))
        (if (null? (cdr genre-count))
            (set-cdr! genre-count (list (cons genre 0)))
            (add-genre genre (cdr genre-count)))))
  (let ((last-node (get-last-node inventory)))
    (set-cdr! last-node (make-book-node title author genre price noInStock)))
  (add-author author author-count)
  (add-genre genre genre-count)
 )


;FIND-BOOK <-rishi
(define (find-book title inventory)
  (if (null? inventory) 'no-such-book
  (if (equal? title (caar inventory))
      (cdar inventory)
      (find-book title (cdr inventory)))))


;GETTING INDIVIDUAL FIELD FROM BOOK NODE <- srihitha
(define (get-field-from-book field book)
  (if (equal? field (caar book))
      (car book)
      (get-field-from-book field (cdr book))))


;GET-STOCK-COUNT OF A BOOK <-jaswanth
(define (get-stock-count title inventory)
  (define book (find-book title inventory))
  (if (eq? book 'no-such-book) book
      (cdr (get-field-from-book 'noInStock book))))

 
;GET-SOLD-COUNT OF A BOOK <-jaswanth
(define (get-sold-count title inventory)
  (define book (find-book title inventory))
  (if (eq? book 'no-such-book) book
      (cdr (get-field-from-book 'noSold book))))

;INCREASE STOCK COUNT OF A BOOK <-srihitha
(define (increase-stock title count inventory)
  (let ((book (find-book title inventory)))
    (if (eq? book 'no-such-book) book
        (let ((stock-field (get-field-from-book 'noInStock book)))
          (let ((current-stock (cdr stock-field)))
            (let ((new-stock (+ current-stock count)))
              (set-cdr! stock-field new-stock)))))))


;SELL BOOK 
(define (sell-book title count inventory)
  (define (get-count-field bookfield count-list)
    (if (equal? bookfield (caar count-list))
        (car count-list)
        (get-count-field bookfield (cdr count-list))))
  (define (increase-count bookfield count count-list)
    (let ((countfield (get-count-field bookfield count-list)))
      (let ((currentcount (cdr countfield)))
        (let ((newcount (+ currentcount count)))
          (set-cdr! countfield newcount)))))
  (let ((book (find-book title inventory)))
    (if (eq? book 'no-such-book) book
        (let ((stockfield (get-field-from-book 'noInStock book))
              (soldfield (get-field-from-book 'noSold book))
              (genrefield (get-field-from-book 'genre book))
              (authorfield (get-field-from-book 'author book)))
          (let ((stockcount (cdr stockfield))
                (soldcount (cdr soldfield))
                (bookgenre (cdr genrefield))
                (bookauthor (cdr authorfield)))
            (if (< stockcount count) 'not-enough-stock
                (let ((newstock (- stockcount count))
                      (newsold (+ soldcount count)))
                  (set-cdr! stockfield newstock)
                  (set-cdr! soldfield newsold)
                  (increase-count bookgenre count genre-count)
                  (increase-count bookauthor count author-count))))))))


;NOT IN STOCK BOOKS <- rishi
(define (not-in-stock inventory)
  (define not-in-stock-count 0)
  (define (get-count inventory)
    (cond [(null? inventory) not-in-stock-count]
        [else (let ((currentbook (cdar inventory)))
          (let ((current-noinstock-field (get-field-from-book 'noInStock currentbook)))
                (if (equal? 0 (cdr current-noinstock-field))
                    (set! not-in-stock-count (+ not-in-stock-count 1))
                    ))
         (get-count (cdr inventory)))]))
  (get-count (cdr inventory)))


;NOT SOLD BOOKS <- rishi 
(define (not-sold inventory)
  (define not-sold-count 0)
  (define (get-count inventory)
    (cond [(null? inventory) not-sold-count]
        [else (let ((currentbook (cdar inventory)))
          (let ((current-nosold-field (get-field-from-book 'noSold currentbook)))
                (if (equal? 0 (cdr current-nosold-field))
                    (set! not-sold-count (+ not-sold-count 1)))))
         (get-count (cdr inventory))]))
  (get-count (cdr inventory)))


;STATISTICS FOR GENRE-COUNT <- jaswanth
(define (best-sold-genre genre-count)
  (define highest-count 0)
  (define (get-highest-count genre-count)
    (if (null? genre-count)
        highest-count
        (let ((current-count (cdar genre-count)))
          ;(display current-count)
          (if (> current-count highest-count)
              (set! highest-count current-count))
          (get-highest-count (cdr genre-count)))))
  (get-highest-count (cdr genre-count))
  (display 'HIGHEST-SELLING-GENRE-COUNT-)
  (display highest-count)
  (newline)
  (display 'BEST-SELLING-GENRES)
  (define (get-best-sold-genre genre-count)
    (if (not (null? genre-count))
        (let ((current-count (cdar genre-count)))
          (cond [(equal? highest-count current-count)
              (display (caar genre-count))
              (newline)])
          (get-best-sold-genre (cdr genre-count)))))
  (get-best-sold-genre (cdr genre-count)))

(define (worst-sold-genre genre-count)
  (define lowest-count 0)
  (define (get-lowest-count genre-count)
    (if (null? genre-count)
        lowest-count
        (let ((current-count (cdar genre-count)))
          (if (< current-count lowest-count)
              (set! lowest-count current-count))
          (get-lowest-count (cdr genre-count)))))
  (get-lowest-count (cdr genre-count))
  (display 'LOWEST-SELLING-GENRE-COUNT-)
  (display lowest-count)
  (newline)
  (display 'WORST-SELLING-GENRES)
  (newline)
  (define (get-worst-sold-genre genre-count)
    (if (not (null? genre-count))
        (let ((current-count (cdar genre-count)))
          (cond [(equal? lowest-count current-count)
              (display (caar genre-count))
              (newline)])
          (get-worst-sold-genre (cdr genre-count)))))
  (get-worst-sold-genre (cdr genre-count)))


;STATISTICS FOR AUTHOR-COUNT <- nanditha 
(define (best-sold-author author-count)
  (define highest-count 0)
  (define (get-highest-count author-count)
    (if (null? author-count)
        highest-count
        (let ((current-count (cdar author-count)))
          (if (> current-count highest-count)
              (set! highest-count current-count))
          (get-highest-count (cdr author-count)))))
  (get-highest-count (cdr author-count))
  (display 'HIGHEST-SELLING-AUTHOR-COUNT-)
  (display highest-count)
  (newline)
  (display 'BEST-SELLING-AUTHORS)
  (newline)
  (define (get-best-sold-author author-count)
    (if (not (null? author-count))
        (let ((current-count (cdar author-count)))
          (cond [(equal? highest-count current-count)
              (display (caar author-count))
              (newline)])
          (get-best-sold-author (cdr author-count)))))
  (get-best-sold-author (cdr author-count)))

(define (worst-sold-author author-count)
  (define lowest-count 0)
  (define (get-lowest-count author-count)
    (if (null? author-count)
        lowest-count
        (let ((current-count (cdar author-count)))
          (if (< current-count lowest-count)
              (set! lowest-count current-count))
          (get-lowest-count (cdr author-count)))))
  (get-lowest-count (cdr author-count))
  (display 'LOWEST-SELLING-AUTHOR-COUNT-)
  (display lowest-count)
  (newline)
  (display 'WORST-SELLING-AUTHORS)
  (newline)
  (define (get-worst-sold-author author-count)
    (if (not (null? author-count))
        (let ((current-count (cdar author-count)))
          (cond [(equal? lowest-count current-count)
              (display (caar author-count))
              (newline)])
          (get-worst-sold-author (cdr author-count)))))
  (get-worst-sold-author (cdr author-count)))


                
;INPUTTING SOME BOOKS INTO INVENTORY
(add-book 'abc 'def 'Romance 130 20)
(add-book 'pqr 'tdg 'Romance 100 80)
(add-book 'amazing 'srihitha 'Comedy 200 30)
(add-book 'Ghost-Busters 'kick 'Horror 150 0)
(sell-book 'abc 20 inventory)
(sell-book 'amazing 20 inventory)