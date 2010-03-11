 
;twenty-one.scm 
;project 1, cs61 data structure and interpretation of computer programs
;laney spring 2010
;by roselyn roark


(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
	
(define (move-card in out which)
	      (if (= which 0)
		  (se (first in) (shuffle (se (bf in) out) (- size 1)))
		  (move-card (bf in) (se (first in) out) (- which 1)) ))
	    (if (= size 0)
		deck
	    	(move-card deck '() (random size)) ))
	  (shuffle (make-ordered-deck) 52) )
	
	;best-total

	(define (is-an-ace card)
		(if (equal? (first  card) 'a) 1 0)) 

	(define (count-ace sent)
		(accumulate + (every is-an-ace sent)))

	(define (non-ace-value card)
		(cond ((equal? (first card) 'a) 0)
			  ((member? (first card) '(k j q)) 10)
			  (else (first card))))

	(define (non-ace-total sent)
			(accumulate + (every non-ace-value sent)))

	(define (which-hand option-one option-two)
		(if (<= option-two 21) option-two option-one))

	(define (best-total hand)
		    (if (equal? (count-ace hand) 0) (non-ace-total hand)
			(which-hand (+ (non-ace-total hand) (count-ace hand))
						(+ (non-ace-total hand) (count-ace hand) 10))))

	;stop-at-17

	(define (stop-at-17 customer-hand-so-far dealer-up-card)
		(if (< (best-total customer-hand-so-far) 17) #t #f))

	;play-n

	(define (play-n strategy n)
		(if (> n 0) 
			(+ (twenty-one strategy) (play-n strategy (- n 1)))
			0))

	;dealer-sensitive

	(define (dealer-sensitive customer-hand-so-far dealer-up-card)
		(cond ((and (< (best-total customer-hand-so-far) 12) (<= (face-card-value dealer-up-card) 6)) #t)
		      ((and (< (best-total customer-hand-so-far) 17) (>= (face-card-value dealer-up-card) 7)) #t)
		   	  (else #f)))

				(define (face-card-value card)
					(cond ((equal? (first card) 'a) 11)
						  ((member? (first card) '(k j q)) 10)
						  (else (first card))))

	;stop-at

	(define (stop-at n)
		(lambda (customer-hand-so-far dealer-up-card) 
		(if (< (best-total customer-hand-so-far) n) #t #f)))

	;valentine

	(define (valentine customer-hand-so-far dealer-up-card)
		(if (> (count-hearts customer-hand-so-far) 0)
	 	((stop-at 19) customer-hand-so-far dealer-up-card)
		((stop-at 17) customer-hand-so-far dealer-up-card)))

	(define (is-a-heart card)
		(if (equal? (last card) 'h) 1 0))

	(define (count-hearts sent)
		(accumulate + (every is-a-heart sent)))

	;suit-strategy

	(define (suit-strategy suit strategy-without-suit strategy-with-suit)
		(lambda (customer-hand-so-far dealer-up-card)
	(if (> (count-suits suit customer-hand-so-far) 0) 
		(strategy-without-suit customer-hand-so-far dealer-up-card) 
		(strategy-with-suit customer-hand-so-far dealer-up-card))))

				(define (count-suits suit sent)
					(accumulate + (every (is-the-suit suit) sent)))

					(define (is-the-suit suit-check)
						(lambda (customer-hand-so-far) 
							(if (equal? (last customer-hand-so-far) suit-check) 1 0)))

	;majority

	(define (majority strategy-1 strategy-2 strategy-3)
		(lambda (customer-hand-so-far dealer-up-card)
			(cond ((and (strategy-1 customer-hand-so-far dealer-up-card) (strategy-2 customer-hand-so-far dealer-up-card)) #t)
				  ((and (strategy-2 customer-hand-so-far dealer-up-card) (strategy-3 customer-hand-so-far dealer-up-card)) #t)
				  ((and (strategy-3 customer-hand-so-far dealer-up-card) (strategy-1 customer-hand-so-far dealer-up-card)) #t)
		        (else #f))))