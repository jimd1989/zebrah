(import (chicken fixnum)
        (chicken memory representation)
        (chicken process-context)
        (chicken random)
        srfi-1
        srfi-4
        srfi-69
        typed-records)

; /* struct typedefs */
(define-type domain (struct domain))
(define-type relative (struct relative))
(define-type scalar (struct scalar))
(define-type constraint (struct constraint))
(define-type constraints (struct constraints))
(define-type puzzle (struct puzzle))

; /* types */
(define-type range (list-of fixnum))
(define-type domain-position (or fixnum false))
(define-type domain-all-positions (vector-of domain-position))
(define-type domain-matrix (vector-of domain))
(define-type puzzle-list (list-of symbol))
(define-type puzzle-2d-list (list-of puzzle-list))
(define-type puzzle-vector (vector-of scalar))
(define-type puzzle-matrix (vector-of puzzle-vector))
(define-type constraint-list (or null (list-of constraint)))

; /* structs */
(define-record domain
	       (score : fixnum)
	       (arc : domain-position)
	       (positions : domain-all-positions))

(define-record relative
	       (count : fixnum)
	       (members : (or null (list-of fixnum))))

(define-record scalar
	       (name : symbol)
	       (left-of : (vector-of relative))
	       (next-to : (vector-of relative))
	       (domains : domain-matrix))

(define-record constraint
	       (f : (procedure (*) symbol))
	       (plaintext : (list-of symbol)))

(define-record constraints
	       (direct : fixnum)
	       (relative : fixnum)
	       (left-of : fixnum)
	       (total : fixnum)
	       (generated : hash-table))

(define-record puzzle
	       (score : fixnum)
	       (goal : fixnum)
	       (y : fixnum)
	       (x : fixnum)
	       (y-range : range)
	       (x-range : range)
	       (matrix : puzzle-matrix)
	       (reset : puzzle-matrix)
	       (constraints : constraints)
	       (modified? : boolean))

; /* global vars */
(: *VERBOSE?* boolean)
(define *VERBOSE?* #f)

(: *INTERACTIVE?* boolean)
(define *INTERACTIVE?* #f)

(: *ZEBRA-FILE* string)
(define *ZEBRA-FILE* "")

; /* list, vector, and matrix manipulation */
(: range-foldr (procedure any fixnum --> any))
(define (range-foldr f acc n)
  (letrec ((loop (lambda (nn)
		   (if (fx= nn n)
		     acc
		     (f nn (loop (fx+ nn 1)))))))
    (loop 0)))

(: range-for-each (procedure fixnum -> boolean))
(define (range-for-each f n)
  (letrec ((loop (lambda (nn)
		   (if (fx= nn n)
		     #t
		     (begin (f nn) (loop (fx+ nn 1)))))))
    (loop 0)))

; /* puzzle init */
(: blank-relative ( --> relative))
(define (blank-relative)
  (make-relative 0 '()))

(: blank-relative-vector (fixnum --> (vector-of relative)))
(define (blank-relative-vector y)
  (list->vector
    (range-foldr (lambda (_ acc) (cons (blank-relative) acc)) '() y)))

(: blank-scalar ( --> scalar))
(define (blank-scalar)
  (make-scalar 'null (vector (blank-relative)) (vector (blank-relative))
	       (vector (make-domain 0 0 (vector 0)))))

(: blank-puzzle-vector (fixnum --> puzzle-vector))
(define (blank-puzzle-vector x)
  (list->vector
    (range-foldr (lambda (_ acc) (cons (blank-scalar) acc)) '() x)))

(: blank-puzzle-matrix (fixnum fixnum --> puzzle-matrix))
(define (blank-puzzle-matrix y x)
  (list->vector
    (range-foldr (lambda (_ acc) (cons (blank-puzzle-vector x) acc)) '() y)))

(: constraints-init (fixnum fixnum --> constraints))
(define (constraints-init y x)
  (let* ((direct (direct-assertion-total y x))
	 (relative (relative-assertion-total y x))
	 (total (fx+ direct (fx+ relative relative))))
    (make-constraints direct relative (fx+ direct relative) total
		      (make-hash-table))))

(: blank-puzzle (puzzle-2d-list --> puzzle))
(define (blank-puzzle xs)
  (let* ((y (length xs))
	 (x (length (car xs)))
	 (M (blank-puzzle-matrix y x)))
    (make-puzzle 0 (fx* y (fx* y x)) y x (iota y) (iota x) M M
		 (constraints-init y x) #f)))

(: puzzle-ref (puzzle fixnum fixnum --> scalar))
(define (puzzle-ref p y x)
  (vector-ref (vector-ref (puzzle-matrix p) y) x))

(: domain-init (puzzle --> domain))
(define (domain-init p)
  (make-domain (puzzle-x p) #f (list->vector (puzzle-x-range p))))

(: domain-matrix-init (puzzle --> domain-matrix))
(define (domain-matrix-init p)
  (list->vector (map (lambda (y) (domain-init p)) (puzzle-y-range p))))

(: scalar-init! (puzzle scalar symbol -> noreturn))
(define (scalar-init! p s name)
  (scalar-name-set! s name)
  (scalar-left-of-set! s (blank-relative-vector (puzzle-y p)))
  (scalar-next-to-set! s (blank-relative-vector (puzzle-y p)))
  (scalar-domains-set! s (domain-matrix-init p)))

(: puzzle-vector-init! (puzzle puzzle-list fixnum -> noreturn))
(define (puzzle-vector-init! p ps y)
  (for-each
    (lambda (x n) (scalar-init! p (puzzle-ref p y x) n))
    (puzzle-x-range p)
    ps))

(: puzzle-matrix-init! (puzzle puzzle-2d-list -> noreturn))
(define (puzzle-matrix-init! p pss)
  (for-each
    (lambda (y ps) (puzzle-vector-init! p ps y))
    (puzzle-y-range p)
    pss))

(: puzzle-init! (puzzle-2d-list -> puzzle))
(define (puzzle-init! ps)
  (let ((p (blank-puzzle ps)))
    (puzzle-matrix-init! p ps)
    (puzzle-reset-set! p (object-copy (puzzle-matrix p)))
    p))

; /* direct solving */
(: puzzle-score-increment! (puzzle -> noreturn))
(define (puzzle-score-increment! p)
  (puzzle-score-set! p (fx+ (puzzle-score p) 1)))

(: domain-positions-ref (domain fixnum --> domain-position))
(define (domain-positions-ref a x)
  (vector-ref (domain-positions a) x))

(: domain-positions-set! (domain fixnum domain-position -> noreturn))
(define (domain-positions-set! a x n)
  (vector-set! (domain-positions a) x n))

(: domain-ref (puzzle fixnum fixnum fixnum --> domain))
(define (domain-ref p y1 x1 y2)
  (vector-ref (scalar-domains (puzzle-ref p y1 x1)) y2))

(: domain-matrix-ref (domain-matrix fixnum fixnum --> domain-position))
(define (domain-matrix-ref A y x)
  (domain-positions-ref (vector-ref A y) x))

(: domain-score-decrement! (domain -> noreturn))
(define (domain-score-decrement! a)
  (domain-score-set! a (fx- (domain-score a) 1)))

(: domain-position? (domain-position --> boolean))
(define (domain-position? x)
  (if x #t #f))

(: domain-poe-solved? (domain --> boolean))
(define (domain-poe-solved? a)
  (fx= (domain-score a) 1))

(: domain-arc-position (puzzle domain --> fixnum))
(define (domain-arc-position p a)
  (call/cc (lambda (break)
	     (range-for-each (lambda (x)
			       (let ((ax (domain-positions-ref a x)))
				 (if (domain-position? ax)
				   (break ax))))
			     (puzzle-x p)))))

(: domain-poe-solve! (puzzle domain -> noreturn))
(define (domain-poe-solve! p a)
  (domain-arc-set! a (domain-arc-position p a))
  (puzzle-score-increment! p))

(: domain-position-remove! (puzzle domain fixnum -> boolean))
(define (domain-position-remove! p a x)
  (if (domain-position? (domain-positions-ref a x))
    (begin (domain-positions-set! a x #f)
	   (domain-score-decrement! a)
	   (puzzle-modified?-set! p #t)
	   #t)
    #f))

(: domain-merge! (puzzle fixnum fixnum fixnum fixnum domain domain fixnum
			 -> noreturn))
(define (domain-merge! p y1 x1 y2 x2 a1 a2 y)
  (range-for-each (lambda (x)
		    (let ((ap1 (vector-ref (domain-positions a1) x))
			  (ap2 (vector-ref (domain-positions a2) x)))
		      (cond ((and (not ap1) ap2) (negate! p y2 x2 y x))
			    ((and ap1 (not ap2)) (negate! p y1 x1 y x)))))
		  (puzzle-x p)))

(: domain-matrix-merge! (puzzle fixnum fixnum fixnum fixnum -> boolean))
(define (domain-matrix-merge! p y1 x1 y2 x2)
  (range-for-each (lambda (y)
		    (let ((a1 (domain-ref p y1 x1 y))
			  (a2 (domain-ref p y2 x2 y)))
		      (domain-merge! p y1 x1 y2 x2 a1 a2 y)))
		  (puzzle-y p)))

(: neighbors-negate! (puzzle fixnum fixnum fixnum fixnum -> noreturn))
(define (neighbors-negate! p y1 x1 y2 x2)
  (range-for-each
    (lambda (x) (if (not (fx= x x1)) (negate! p y1 x y2 x2)))
    (puzzle-x p)))

(: domain-matrix-propogate! (puzzle domain-matrix fixnum fixnum fixnum fixnum
				    -> boolean))
(define (domain-matrix-propogate! p A y1 x1 y2 x2)
  (range-for-each (lambda (y)
		    (let ((x (domain-arc (vector-ref A y))))
		      (if (and x (not (fx= y y1)))
			(unary-negate! p y x y2 x2 #f))))
		  (puzzle-y p)))

(: unary-negate! (puzzle fixnum fixnum fixnum fixnum #!optional boolean
			 -> boolean))
(define (unary-negate! p y1 x1 y2 x2 #!optional (propogate? #t))
  (let* ((s (puzzle-ref p y1 x1))
	 (a (domain-ref p y1 x1 y2))
	 (r (domain-position-remove! p a x2)))
    (if r
      (cond ((domain-poe-solved? a)
	     (domain-poe-solve! p a)
	     (assert! p y1 x1 y2 (domain-arc a))
	     (domain-matrix-merge! p y1 x1 y2 (domain-arc a)))
	    (propogate?
	      (domain-matrix-propogate! p (scalar-domains s) y1 x1 y2 x2))
	    (else #t))
      #f)))

(: negate! (puzzle fixnum fixnum fixnum fixnum #!optional boolean -> symbol))
(define (negate! p y1 x1 y2 x2 #!optional (propogate? #t))
  (let ((r1 (unary-negate! p y1 x1 y2 x2 propogate?))
	(r2 (unary-negate! p y2 x2 y1 x1 propogate?)))
    (if (or r1 r2)
      'pertinent
      'redundant)))

(: assert! (puzzle fixnum fixnum fixnum fixnum -> symbol))
(define (assert! p y1 x1 y2 x2)
  (if (and (domain-arc (domain-ref p y1 x1 y2))
	   (domain-arc (domain-ref p y2 x2 y1)))
    'redundant
    (begin (neighbors-negate! p y1 x1 y2 x2)
	   'pertinent)))

; /* relative solving */
(: relative-uncounted? (relative fixnum --> boolean))
(define (relative-uncounted? r x)
  (if (member x (relative-members r)) #f #t))

(: relative-all-counted? (relative --> boolean))
(define (relative-all-counted? r)
  (if (fx= (relative-count r) 2) #t #f))

(: relative-increment! (puzzle relative fixnum -> noreturn))
(define (relative-increment! p r x)
  (if (relative-uncounted? r x)
    (begin (relative-members-set! r (cons x (relative-members r)))
	   (relative-count-set! r (fx+ (relative-count r) 1))
	   (puzzle-modified?-set! p #t))))

(: relative-rule-out! (puzzle scalar fixnum fixnum fixnum symbol -> noreturn))
(define (relative-rule-out! p s y x1 x2 type)
  (let* ((v (if (eq? type 'left-of) (scalar-left-of s) (scalar-next-to s)))
	 (r (vector-ref v y)))
    (if (not (relative-all-counted? r))
      (begin (relative-increment! p r x2)
	     (if (relative-all-counted? r)
	       (begin (negate! p 0 0 y x1)
		      (negate! p 0 (fx- (puzzle-x p) 1) y x1)))))))

(: theoretical-vector-ref (puzzle domain-all-positions fixnum
				  --> domain-position))
(define (theoretical-vector-ref p v x)
  (cond ((fx< x 0) #f)
	((fx= x (puzzle-x p)) #f)
	(else (vector-ref v x))))

(: theoretical-negate! (puzzle fixnum fixnum fixnum fixnum -> noreturn))
(define (theoretical-negate! p y1 x1 y2 x2)
  (if (not (or (fx>= x1 (puzzle-x p)) (fx< x1 0)))
    (negate! p y1 x1 y2 x2)))

(: left-of-rule-out!
  (puzzle fixnum fixnum fixnum fixnum domain-all-positions domain-all-positions
	   -> noreturn))
(define (left-of-rule-out! p y1 x1 y2 x2 v1 v2)
  (range-for-each (lambda (x)
		    (let ((ap1 (theoretical-vector-ref p v1 (fx- x 1)))
			  (ap2 (theoretical-vector-ref p v2 x)))
		      (cond ((and (not ap1) (not ap2)) #f)
			    ((not ap1) (negate! p 0 x y2 x2))
			    ((not ap2) (negate! p 0 (fx- x 1) y1 x1)))))
		  (fx+ (puzzle-x p) 1)))

(: left-of! (puzzle fixnum fixnum fixnum fixnum -> symbol))
(define (left-of! p y1 x1 y2 x2)
  (if (or (fx= y1 0) (fx= y2 0))
    'unacceptable
    (let ((s1 (puzzle-ref p y1 x1))
	  (s2 (puzzle-ref p y2 x2))
	  (a1 (domain-ref p y1 x1 0))
	  (a2 (domain-ref p y2 x2 0)))
      (relative-rule-out! p s1 y1 x1 x2 'left-of)
      (relative-rule-out! p s2 y2 x2 x1 'left-of)
      (cond ((and (domain-arc a1) (domain-arc a2))
	     'redundant)
	    ((domain-arc a1)
	     (assert! p 0 (fx+ (domain-arc a1) 1) y2 x2) 'pertinent)
	    ((domain-arc a2)
	     (assert! p 0 (fx- (domain-arc a2) 1) y1 x1) 'pertinent)
	    (else
	      (left-of-rule-out! p y1 x1 y2 x2
				 (object-copy (domain-positions a1))
				 (object-copy (domain-positions a2)))
	      (cond ((and (domain-arc a1) (domain-arc a2))
		     'pertinent)
		    ((domain-arc a1)
		     (assert! p 0 (fx+ (domain-arc a1) 1) y2 x2) 'pertinent)
		    ((domain-arc a2)
		     (assert! p 0 (fx- (domain-arc a2) 1) y1 x1) 'pertinent)
		    (else
		      'ambiguous)))))))

(: next-to-rule-out! (puzzle fixnum fixnum domain-all-positions -> noreturn))
(define (next-to-rule-out! p y1 x1 v)
  (range-for-each (lambda (x)
		    (let ((ap1 (theoretical-vector-ref p v (fx- x 1)))
			  (ap2 (theoretical-vector-ref p v (fx+ x 1))))
		      (if (not (or ap1 ap2))
			(negate! p 0 x y1 x1))))
		  (puzzle-x p)))

(: next-to-half-solved-rule-out!
   (puzzle fixnum fixnum fixnum domain-all-positions -> noreturn))
(define (next-to-half-solved-rule-out! p y1 x1 x2 v)
  (let ((n1 (fx- x2 1))
	(n2 (fx+ x2 1)))
    (range-for-each
      (lambda (x) (if (not (or (fx= x n1) (fx= x n2))) (negate! p 0 x y1 x1)))
      (puzzle-x p))))

(: next-to! (puzzle fixnum fixnum fixnum fixnum -> symbol))
(define (next-to! p y1 x1 y2 x2)
  (if (or (fx= y1 0) (fx= y2 0))
    'unacceptable
    (let ((s1 (puzzle-ref p y1 x1))
	  (s2 (puzzle-ref p y2 x2))
	  (a1 (domain-ref p y1 x1 0))
	  (a2 (domain-ref p y2 x2 0)))
      (relative-rule-out! p s1 y1 x1 x2 'left-of)
      (relative-rule-out! p s2 y2 x2 x1 'left-of)
      (cond ((and (domain-arc a1) (domain-arc a2)) 'redundant)
	    ((domain-arc a1)
	     (next-to-half-solved-rule-out! p y2 x2 (domain-arc a1)
					   (object-copy (domain-positions a2)))
	     (if (domain-arc a2)
	       'pertinent
	       'ambiguous))
	    ((domain-arc a2)
	     (next-to-half-solved-rule-out! p y1 x1 (domain-arc a2)
					   (object-copy (domain-positions a1)))
	     (if (domain-arc a1)
	       'pertinent
	       'ambiguous))
	    (else (next-to-rule-out! p y1 x1
				     (object-copy(domain-positions a2)))
		  (next-to-rule-out! p y2 x2
				     (object-copy (domain-positions a1)))
		  (if (and (domain-arc a1) (domain-arc a2))
		    'pertinent
		    'ambiguous))))))

; /* constraint calculation */
(: triangular-number (fixnum --> fixnum))
(define (triangular-number n)
  (fx/ (fx* n (fx+ n 1)) 2))

(: direct-assertion-total (fixnum fixnum --> fixnum))
(define (direct-assertion-total y x)
  (fx* (triangular-number (fx- y 1)) x))

(: relative-assertion-total (fixnum fixnum --> fixnum))
(define (relative-assertion-total y x)
  (fx* (fx* y y) (fx- x 1)))

(: direct-y1 (fixnum fixnum --> fixnum))
(define (direct-y1 x r)
  (fx+ 1 (fx/ (fx- (inexact->exact
		     (floor (sqrt (fx+ 1 (fx* 8 (fx/ r x)))))) 1) 2)))

(: direct-y2 (fixnum fixnum fixnum --> fixnum))
(define (direct-y2 y x r)
  (fx/ (fx- r (fx* (triangular-number (fx- y 1)) x)) x))

(: relative-y1 (fixnum fixnum fixnum --> fixnum))
(define (relative-y1 y x r)
  (fx/ (fx/ r y) (fx- x 1)))

(: relative-y2 (fixnum fixnum fixnum --> fixnum))
(define (relative-y2 y x r)
  (modulo (fx/ r (fx- x 1)) y))

(: make-direct-constraint (puzzle fixnum --> constraint))
(define (make-direct-constraint p r)
  (let* ((y1 (direct-y1 (puzzle-x p) r))
	 (y2 (direct-y2 y1 (puzzle-x p) r))
	 (x (modulo r (puzzle-x p)))
	 (s1 (puzzle-ref p y1 x))
	 (s2 (puzzle-ref p y2 x))
	 (order-flipped? (fx= (pseudo-random-integer 2) 0)))
    (if order-flipped?
      (make-constraint (lambda (p) (assert! p y2 x y1 x))
		       (list (scalar-name s2) '= (scalar-name s1)))
      (make-constraint (lambda (p) (assert! p y1 x y2 x))
		       (list (scalar-name s1) '= (scalar-name s2))))))

(: make-relative-constraint (puzzle fixnum #!optional boolean --> constraint))
(define (make-relative-constraint p r #!optional (left-of? #t))
  (let* ((y1 (relative-y1 (puzzle-y p) (puzzle-x p) r))
	 (y2 (relative-y2 (puzzle-y p) (puzzle-x p) r))
	 (x1 (modulo r (fx- (puzzle-x p) 1)))
	 (x2 (fx+ x1 1))
	 (s1 (puzzle-ref p y1 x1))
	 (s2 (puzzle-ref p y2 x2))
	 (order-flipped? (fx= (pseudo-random-integer 2) 0)))
    (if left-of?
      (if order-flipped?
	(make-constraint (lambda (p) (left-of! p y1 x1 y2 x2))
			 (list (scalar-name s2) 'is 'right 'of
			       (scalar-name s1)))
	(make-constraint (lambda (p) (left-of! p y1 x1 y2 x2))
			 (list (scalar-name s1) 'is 'left 'of
			       (scalar-name s2))))
      (if order-flipped?
	(make-constraint (lambda (p) (next-to! p y2 x2 y1 x1))
			 (list (scalar-name s2) 'is 'next 'to
			       (scalar-name s1)))
	(make-constraint (lambda (p) (next-to! p y1 x1 y2 x2))
			 (list (scalar-name s1) 'is 'next 'to
			       (scalar-name s2)))))))

(: constraint-generate! (puzzle fixnum -> constraint))
(define (constraint-generate! p r)
  (let ((C (puzzle-constraints p)))
    (cond ((hash-table-exists? (constraints-generated C) r)
	   (constraint-generate! p (modulo (fx+ r 1) (constraints-total C))))
	  ((< r (constraints-direct C))
	   (hash-table-set! (constraints-generated C) r r)
	   (make-direct-constraint p r))
	  ((< r (constraints-left-of C))
	   (hash-table-set! (constraints-generated C) r r)
	   (make-relative-constraint p (fx- r (constraints-direct C))))
	  (else
	    (hash-table-set! (constraints-generated C) r r)
	    (make-relative-constraint p (fx- r (constraints-left-of C)) #f)))))

; /* puzzle generation */
(: puzzle-solved? (puzzle --> boolean))
(define (puzzle-solved? p)
  (fx= (puzzle-score p) (puzzle-goal p)))

(: one-round-solve! (puzzle constraint-list
			    -> (list constraint-list constraint-list)))
(define (one-round-solve! p cs)
  (foldr (lambda (c acc)
	   (let ((r ((constraint-f c) p)))
	     (cond ((eq? 'ambiguous r) (list (car acc) (cons c (cadr acc))))
		   ((eq? 'unacceptable r) acc)
		   (else (list (cons c (car acc)) (cadr acc))))))
	 (list '() '())
	 cs))

(: solve-until-redundant!
   (puzzle constraint-list constraint-list
	   -> (list constraint-list constraint-list)))
(define (solve-until-redundant! p sc dc)
  (puzzle-modified?-set! p #f)
  (let* ((scdc (one-round-solve! p dc))
	 (nsc (append (car scdc) sc))
	 (ndc (cadr scdc)))
    (if (puzzle-modified? p)
      (solve-until-redundant! p nsc ndc)
      (list nsc ndc))))

(: initial-solve! (puzzle constraint-list constraint-list -> constraint-list))
(define (initial-solve! p sc dc)
  (let* ((c (constraint-generate!
              p
              (pseudo-random-integer (constraints-total
                                       (puzzle-constraints p)))))
	 (scdc (solve-until-redundant! p sc (cons c dc))))
    (if (puzzle-solved? p)
      (append (car scdc)(cadr scdc))
      (apply initial-solve! p scdc))))

(: solution-refine! (puzzle constraint-list constraint-list
			    -> constraint-list))
(define (solution-refine! p read-cs unread-cs)
  (if (null? unread-cs)
    read-cs
    (begin (puzzle-matrix-set! p (object-copy (puzzle-reset p)))
	   (puzzle-score-set! p 0)
	   (if *VERBOSE?* (begin (display "Checking ")
				 (display (constraint-plaintext
					    (car unread-cs)))))
	   (solve-until-redundant! p '() (append read-cs (cdr unread-cs)))
	   (if (puzzle-solved? p)
	     (begin (if *VERBOSE?* (display " ... redundant\n"))
		    (solution-refine! p read-cs (cdr unread-cs)))
	     (begin (if *VERBOSE?* (display " ... ok\n"))
		    (solution-refine! p (cons (car unread-cs) read-cs)
				      (cdr unread-cs)))))))

(: puzzle-generate! (puzzle -> noreturn))
(define (puzzle-generate! p)
  (if *VERBOSE?* (display "Generating initial puzzle ...\n"))
  (let ((cs (initial-solve! p '() '())))
    (if *VERBOSE?* (begin (display (length cs))
			  (display " constraints generated.\n")))
    (begin (if *VERBOSE?*
	     (display "Checking for redundant information ...\n"))
	   (let ((rcs (solution-refine! p '() cs)))
	     (newline)
	     (for-each
	       (lambda (c) (display (constraint-plaintext c))(newline))
		       rcs)))))

; /* user input */
(: display-help (--> noreturn))
(define (display-help)
  (display "USAGE: zebrah -option <file>\n")
  (display "\t-v: verbose output\n")
  (display "\t-i: interactive mode (no file argument required)\n")
  (display "\t-h: display this help message\n"))

(: process-command-line-arguments ((or null (list-of string)) -> noreturn))
(define (process-command-line-arguments xs)
  (cond ((null? xs) #t)
	((string=? (car xs) "-v") (set! *VERBOSE?* #t)
				  (process-command-line-arguments (cdr xs)))
	((string=? (car xs) "-i") (set! *INTERACTIVE?* #t)
				  (process-command-line-arguments (cdr xs)))
	((string=? (car xs) "-h") (display-help))
	(else (set! *ZEBRA-FILE* (car xs)))))

(: read-row (fixnum --> puzzle-list))
(define (read-row x)
  (if (fx= 0 x)
    '()
    (begin (display "Enter item: ")
	   (cons (read) (read-row (fx- x 1))))))

(: read-matrix (fixnum fixnum --> puzzle-2d-list))
(define (read-matrix y x)
  (if (fx= 0 y)
    '()
    (begin (display "New row\n")
	   (cons (read-row x) (read-matrix (fx- y 1) x)))))

(: puzzle-2d-list-init ( --> puzzle-2d-list))
(define (puzzle-2d-list-init)
  (read-matrix (begin (display "Enter number of rows: ") (read))
	       (begin (display "Enter number of items in row: ") (read))))

(: number->plaintext (fixnum --> symbol))
(define (number->plaintext x)
  (let* ((n (modulo x 10))
	 (suffix (cond ((eq? n 1) "st")
		       ((eq? n 2) "nd")
		       ((eq? n 3) "rd")
		       (else "th"))))
    (string->symbol (string-append (number->string x) suffix))))

(: first-row-init (fixnum --> puzzle-list))
(define (first-row-init x)
  (map (lambda (n) (number->plaintext n)) (iota x 1)))

; /* main */
(if (null? (command-line-arguments))
  (display-help)
  (begin
    (process-command-line-arguments (command-line-arguments))
    (cond (*INTERACTIVE?*
            (let ((M (puzzle-2d-list-init)))
              (puzzle-generate!
                (puzzle-init!
                  (cons (first-row-init (length (car M))) M)))))
          (else (let ((M (read (open-input-file *ZEBRA-FILE*))))
                  (puzzle-generate!
                    (puzzle-init! (cons
                                    (first-row-init
                                      (length (car M))) M))))))))

; /* debug */
(: text-truncate (symbol --> string))
(define (text-truncate t)
  (let* ((ts (string->list (symbol->string t)))
	 (len (length ts)))
    (if (fx> len 5)
      (list->string (append (take ts 5) (list #\tab)))
      (list->string (append ts (list #\tab))))))

(: scalar-print (puzzle scalar -> noreturn))
(define (scalar-print p s)
  (range-for-each (lambda (x)
		    (if (domain-position?
			  (domain-matrix-ref (scalar-domains s) 0 x))
		      (display (text-truncate (scalar-name s)))
		      (display "_____\t")))
		  (puzzle-x p))
  (newline))

(: vector-print (puzzle puzzle-vector -> noreturn))
(define (vector-print p v)
  (range-for-each (lambda (x) (scalar-print p (vector-ref v x))) (puzzle-x p))
  (newline))

(: matrix-print (puzzle -> noreturn))
(define (matrix-print p)
  (newline)
  (for-each
    (lambda (y) (vector-print p (vector-ref (puzzle-matrix p) y)))
    (cdr (puzzle-y-range p))))

(: domain-print (puzzle domain fixnum -> noreturn))
(define (domain-print p a y)
  (range-for-each (lambda (x)
		    (if (domain-position? (domain-positions-ref a x))
		      (display
			(text-truncate (scalar-name (puzzle-ref p y x))))
		      (display "_____\t")))
		  (puzzle-x p))
  (newline))

(: domain-matrix-print (puzzle fixnum fixnum -> noreturn))
(define (domain-matrix-print p y x)
  (newline)
  (display (scalar-name (puzzle-ref p y x)))(newline)(newline)
  (let ((A (scalar-domains (puzzle-ref p y x))))
    (range-for-each
      (lambda (y) (domain-print p (vector-ref A y) y))
      (puzzle-y p)))
  (newline))
