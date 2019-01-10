;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 30-39) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **************************
;;   Project Euler
;; **************************
;;

;; ==== Question 30 ========================


;; (fifth-power-digits num) produces the sum of the fifth powers of the digits
;;   of a natural number.
;; fifth-power-digits: Nat -> Nat
(define (fifth-power-digits num)
  (cond [(< num 10) (expt num 5)]
        [else (+ (expt (modulo num 10) 5)
                 (fifth-power-digits (quotient num 10)))]))


;; sum-fifth-digit-powers produces the sum of all natural numbers equal to the
;;   sum of the fifth powers of their digits.

;(define sum-fifth-digit-powers
;  (local [(define ceiling 1000000)
;          (define (sum-all-nums current)
;            (cond [(<= ceiling current) 0]
;                  [(= current (fifth-power-digits current))
;                   (+ current (sum-all-nums (add1 current)))]
;                  [else (sum-all-nums (add1 current))]))]
;    (sum-all-nums 2)))
;
;sum-fifth-digit-powers


;; ==== Question 31 ========================


;; A FreqList is a (list Nat Nat Nat Nat Nat Nat Nat Nat) where the 8 Nats
;;   represent values for 1 2 5 10 20 50 100 200 respectively.
;; A FreqDict is a (listof (list Nat FreqList)) where the Nat corresponds to the
;;   FreqList.


;; (f term subtract dict) retrieves a value from the dictionary for the
;;   (term - subtract) term.
;; f: Nat Nat FreqDict
;; requires: subtract is (anyof 1 2 5 10 20 50 100 200)

(define (f term subtract dict)
  (cond [(> 0 (- term subtract)) 0]
        [(= 0 (- term subtract)) 1]
        [else 
         (local [(define freq-list (second (assoc (- term subtract) dict)))
                 (define (sum stop-index num-list)
                   (cond [(= 0 stop-index) 0]
                         [else (+ (first num-list)
                                  (sum (sub1 stop-index) (rest num-list)))]))]
           (cond [(= 1 subtract) (sum 1 freq-list)]
                 [(= 2 subtract) (sum 2 freq-list)]
                 [(= 5 subtract) (sum 3 freq-list)]
                 [(= 10 subtract) (sum 4 freq-list)]
                 [(= 20 subtract) (sum 5 freq-list)]
                 [(= 50 subtract) (sum 6 freq-list)]
                 [(= 100 subtract) (sum 7 freq-list)]
                 [(= 200 subtract) (sum 8 freq-list)]))]))


;; (combinations target) produces the number of combinations possible to make
;;   the target sum with the numbers 1 2 5 10 20 50 100 and 200.
;; combinations: Nat -> Nat

(define (combinations target)
  (local [(define (next dict)
            (local [(define next-num (add1 (first (first dict))))]
              (cond [(< target next-num)
                     (foldr + 0 (second (first dict)))]
                    [else
                     (next (cons (list next-num
                                       (list (f next-num 1 dict)
                                             (f next-num 2 dict)
                                             (f next-num 5 dict)
                                             (f next-num 10 dict)
                                             (f next-num 20 dict)
                                             (f next-num 50 dict)
                                             (f next-num 100 dict)
                                             (f next-num 200 dict)))
                                 dict))])))]
    (next '((1 (1 0 0 0 0 0 0 0))))))

;(combinations 200)


;; ==== Question 32 ========================


;; (pandigital? a b c) checks if the three numbers are 1 to 9 pandigital.
;; pandigital: Nat Nat Nat -> Bool

(define (pandigital? a b c)
  (equal? (quicksort (explode (foldr string-append ""
                                     (map number->string (list a b c))))
                     string<?)
          (list "1" "2" "3" "4" "5" "6" "7" "8" "9")))


;; (remove-dupes num-list) removes all duplicates of a list of numbers.
;; remove-dupes: (listof Nat) -> (listof Nat)

(define (remove-dupes num-list)
  (foldr (lambda (x y) (cond [(empty? y) (list x)]
                             [(equal? x (first y)) y]
                             [else (cons x y)]))
         empty
         (quicksort num-list <)))


;; pand-products produces the sum of all numbers with two factors a and b such
;;   that a, b, and a*b include exactly the numbers 1 through 9.

;(define pand-products
;  (local [(define (pand-nxm a b n m)
;            (local [(define c (* a b))]
;              (cond [(<= (expt 10 m) b)
;                     (pand-nxm (add1 a) (expt 10 (sub1 m)) n m)]
;                    [(<= (expt 10 n) a) empty]
;                    [(<= (expt 10 (- 9 (+ n m))) c)
;                     (pand-nxm (add1 a) (expt 10 (sub1 m)) n m)]
;                    [(pandigital? a b c)
;                     (cons c (pand-nxm a (add1 b) n m))]
;                    [else (pand-nxm a (add1 b) n m)])))]
;    (foldr + 0 (remove-dupes (append (pand-nxm 1 1000 1 4)
;                                     (pand-nxm 10 100 2 3))))))
;
;pand-products


;; ==== Question 33 ========================


;; (curious? a b) determines if the fraction a/b can be incorrectly simplified
;;   by cancelling out exactly one digit in both a and b. For example, 49/98 is
;;   curious because it is equal to 4/8.
;; curious: Nat Nat -> Bool

(define (curious? a b)
  (local [(define num (explode (number->string a)))
          (define den (explode (number->string b)))
          (define simpl-num (remove (first den) (remove (second den) num)))
          (define simpl-den (remove (first num) (remove (second num) den)))]
    (cond [(or (empty? simpl-den)
               (= 2 (length simpl-den))
               (string=? "0" (first simpl-den))) false]
          [else (= (/ a b) (/ (string->number (first simpl-num))
                              (string->number (first simpl-den))))])))


;; find-curious finds all fractions a/b where a and b are both two-digit
;;   natural numbers and a/b is less than 1.

;(define find-curious
;  (local [(define (test-curiosity a b)
;            (cond [(> a 99) empty]
;                  [(> b 99) (test-curiosity (add1 a) (+ 2 a))]
;                  [(curious? a b)
;                   (cons (list a b) (test-curiosity a (add1 b)))]
;                  [else (test-curiosity a (add1 b))]))
;          (define (remove-trivial pair-list)
;            (cond [(empty? pair-list) empty]
;                  [(and (zero? (modulo (first (first pair-list)) 10))
;                        (zero? (modulo (second (first pair-list)) 10)))
;                   (remove-trivial (rest pair-list))]
;                  [else (cons (first pair-list)
;                              (remove-trivial (rest pair-list)))]))]
;    (remove-trivial (test-curiosity 10 11))))


;; (simplify-frac frac-list) takes a list of fractions a/b stored as (list a b)
;;   and returns it in simplest form (list c d).
;; simplify-frac: (listof (list Nat Nat)) -> (list Nat Nat)

(define (simplify-frac frac-list)
  (local [(define (simplify a b factor)
            (cond [(and (> factor (sqrt a)) (not (zero? (modulo b a))))
                   (list a b)]
                  [(zero? (modulo b a)) (/ b a)]
                  [(and (= 2 factor) (even? a) (even? b))
                   (simplify (/ a 2) (/ b 2) 2)]
                  [(and (zero? (modulo a factor)) (zero? (modulo b factor)))
                   (simplify (/ a factor) (/ b factor) factor)]
                  [(= 2 factor) (simplify a b 3)]
                  [else (simplify a b (+ 2 factor))]))]
    (simplify (foldr * 1 (map first frac-list))
              (foldr * 1 (map second frac-list))
              2)))


;(simplify-frac find-curious)


;; ==== Question 34 ========================


;; (factorial nat) produces the factorial of a nat.
;; factorial: Nat -> Nat

(define (factorial nat)
  (foldr * 1 (build-list nat add1)))


;; (factorial-digits nat) produces the sum of the factorials of the digits of a
;;   nat.
;; factorial-digits: Nat -> Nat

(define (factorial-digits nat)
  (foldr + 0
         (map factorial (map string->number (explode (number->string nat))))))


;; (dig-fact-sum ceiling) produces the sum of all natural numbers which are
;;   equal to the sum of the factorials of their digits.
;; dig-fact-sum: Nat -> Nat

(define (dig-fact-sum ceiling)
  (local [(define (try-next current)
            (cond [(> current ceiling) 0]
                  [(= current (factorial-digits current))
                   (+ current (try-next (add1 current)))]
                  [else (try-next (add1 current))]))]
    (try-next 10)))

;(dig-fact-sum (* 7 (factorial 9)))


;; ==== Question 35 ========================


;; (sieve num-list) produces the primes in the list of natural numbers using the
;;   Sieve of Erastothenes.
;; sieve-sum: (listof Nat) -> Nat
;; requires: num-list is a list of increasing natural numbers.

(define (sieve num-list)
  (cond [(empty? num-list) empty]
        [(= 1 (first num-list))
         (sieve (rest num-list))]
        [else
         (local [(define new (first num-list))]
           (cons new (sieve
                      (filter (lambda (x) (not (= 0 (remainder x new))))
                              (rest num-list)))))]))


;; primes-to-1k is a list of all primes below 1000, good for checking primality
;;   up to 1 million.

;(define primes-to-1k (sieve (build-list 1000 add1)))


;; (prime-sub-1m? nat) determines if the nat is a prime.
;; prime?: Nat -> Bool
;; requires: nat < 1000000

;(define (prime-sub-1m? nat)
;  (local [(define (primality nat prime-list)
;            (cond [(empty? prime-list) true]
;                  [(> (first prime-list) (sqrt nat)) true]
;                  [(= (first prime-list) nat) true]
;                  [(zero? (remainder nat (first prime-list))) false]
;                  [else (primality nat (rest prime-list))]))]
;    (primality nat primes-to-1k)))


;; (produce-circle-list nat) produces a increasing list of all numbers generated
;;   by "rotating" the natural number. For example, 137 becomes 371 and 713.
;; produce-circle-list: Nat -> (listof Nat)

(define (produce-circle-list nat)
  (local [;; Translation Functions
          (define (num->num-list nat)
            (explode (number->string nat)))
          (define (num-list->num num-list)
            (string->number (foldr string-append "" num-list)))
          
          (define num-len (length (num->num-list nat)))
          (define (rotate num-list)
            (append (rest num-list) (list (first num-list))))
          (define (all-rotations num-list counter)
            (cond [(= 0 counter) empty]
                  [else
                   (cons num-list (all-rotations (rotate num-list)
                                                 (sub1 counter)))]))]
    (remove-dupes
     (quicksort (map num-list->num
                     (all-rotations (num->num-list nat) num-len)) <))))


;; (circular-primes ceiling) produces the number of natural numbers below the
;;   ceiling for which all rotations of the number are prime.
;; circular-primes: Nat -> Nat

;(define (circular-primes ceiling)
;  (local [(define (check-prime current)
;            (local [(define circle (produce-circle-list current))]
;              (cond [(>= current ceiling) empty]
;                    [(not (= current (first circle)))
;                     (check-prime (add1 current))]
;                    [(foldl (lambda (x y) (and y (prime-sub-1m? x)))
;                            true circle)
;                     (cons circle (check-prime (add1 current)))]
;                    [else (check-prime (add1 current))])))]
;    (length (foldr append empty (check-prime 2)))))


;(circular-primes 1000000)


;; ==== Question 36 ========================


;; (base-change nat from to) changes the base of a number.
;; base-change: Nat Nat Nat -> Nat

(define (base-change nat from to)
  (local [(define (base nat pow)
            (cond [(zero? nat) 0]
                  [(zero? (modulo nat to))
                   (base (quotient nat to) (add1 pow))]
                  [else (+ (* (modulo nat to) (expt from pow))
                           (base (quotient nat to) (add1 pow)))]))]
    (base nat 0)))


;; (double-palindrome? nat) checks if the nat given in base 10 is a palindrome
;;   in base 10 and base 2.
;; double-palindrome?: Nat -> Bool

(define (double-palindrome? nat)
  (local [(define b10 (explode (number->string nat)))
          (define b2 (explode (number->string (base-change nat 10 2))))
          (define (palindrome? digit-list)
            (equal? digit-list (reverse digit-list)))]
    (and (palindrome? b10) (palindrome? b2))))


;; (sum-double-palindrome cap) produces the sum of all numbers up to the ceiling
;;   which are palindromes in both base 10 and base 2.
;; sum-double-palindrome: Nat -> Nat

(define (sum-double-palindrome cap)
  (local [(define ceiling (sub1 cap))]
    (foldr (lambda (x y) (cond [(double-palindrome? x) (+ x y)] [else y]))
           0 (build-list ceiling add1))))


;(sum-double-palindrome 1000000)


;; ==== Question 37 ========================


;(define primes-to-1k (sieve (build-list 1000 add1)))


;; (trunc-right nat) produces the list of numbers formed by truncating the nat
;;   from the right.
;; trunc-right: Nat -> (listof Nat)

(define (trunc-right nat)
  (cond [(< nat 10) (list nat)]
        [else (cons nat (trunc-right (quotient nat 10)))]))


;; (trunc-left nat) produces the list of numbers formed by truncating the nat
;;   from the left.
;; trunc-left: Nat -> (listof Nat)

(define (trunc-left nat)
  (local [(define digits (length (explode (number->string nat))))]
    (cond [(= 1 digits) (list nat)]
          [else (cons nat (trunc-left (modulo nat (expt 10 (sub1 digits)))))])))

(trunc-right 3797)
(trunc-left 3797)