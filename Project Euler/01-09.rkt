;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 01-09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **************************
;;   Project Euler
;; **************************
;;

;; ==== Question 1 ========================


;; (mult? divisor-list dividend) returns true if the dividend is
;;   divisible by any divisor in the list.
;; mult?: (listof Int) Int -> Bool

(define (mult? divisor-list dividend)
  (cond [(empty? divisor-list) false]
        [(< (sqrt dividend) (first divisor-list))
         (mult? (rest divisor-list) dividend)]
        [(= 0 (remainder dividend (first divisor-list))) true]
        [else (mult? (rest divisor-list) dividend)]))


;; (sum-of-multiples mult-list ceiling) produces the sum of all natural numbers
;;   up to the ceiling which are divisible by at least one of the numbers in the
;;   mult-list.
;; sum-of-multiples: (listof Int) Nat -> Nat

(define (sum-of-multiples mult-list ceiling)
  (foldr + 0 (build-list ceiling (lambda (x) (cond [(mult? mult-list x) x]
                                                   [else 0])))))

;(sum-of-multiples '(3 5) 1000)


;; ==== Question 2 ========================


(define fib-1 1)
(define fib-2 2)

;; (even-fib cur-fib prev-fib fib-sum ceiling) calculates the sum of all the
;;   even fibonacci numbers not exceeding the ceiling.
;; even-fib: Nat Nat Nat Nat -> Nat

(define (even-fib cur-fib prev-fib fib-sum ceiling)
  (cond [(> cur-fib ceiling) fib-sum]
        [(even? cur-fib)
         (even-fib (+ cur-fib prev-fib) cur-fib (+ cur-fib fib-sum) ceiling)]
        [else
         (even-fib (+ cur-fib prev-fib) cur-fib fib-sum ceiling)]))

;(even-fib fib-2 fib-1 0 4000000)


;; ==== Question 3 ========================


;; (next-prime prime-list) produces the first prime larger than all primes in
;;   the prime list.
;; next-prime: (listof Nat) -> Nat
;; requires: prime-list is a list of primes in descending order.

(define (next-prime prime-list)
  (local [;; (prime-search prime-candidate) recursively checks if the prime
          ;;   candidate is divisible by any prime in the list and produces
          ;;   the first number that isn't.
          ;; prime-search: Nat -> Nat
          
          (define (prime-search prime-candidate)
            (cond [(mult? prime-list prime-candidate)
                   (prime-search (+ 2 prime-candidate))]
                  [else prime-candidate]))]
    
    (cond [(empty? prime-list) 2]
          [(= 1 (length prime-list)) 3]
          [else (prime-search (+ 2 (first prime-list)))])))


;; (divide-all dividend divisor) divides the dividend by the divisor until it is
;;   unable to divide any further.
;; divide-all: Nat Nat -> Nat

(define (divide-all dividend divisor)
  (cond [(= 0 (remainder dividend divisor))
         (divide-all (/ dividend divisor) divisor)]
        [else dividend]))


;; (largest-factor large-num prime-list) finds the largest prime factor of a
;;   number.
;; largest-factor: Nat (listof Nat) -> Nat
;; requires: prime-list is a list of primes in descending order.
;;           large-num > 1

(define (largest-factor large-num prime-list)
  (cond [(= 1 large-num) (first prime-list)]
        [else (local [(define prime (next-prime prime-list))]
                (largest-factor (divide-all large-num prime)
                                (cons prime prime-list)))]))

;(largest-factor 600851475143 empty)


;; ==== Question 4 ========================


(define 3-digit-max 999)


;; (palindrome? nat) checks if the number is a palindrome.
;; palindrome? Nat -> Bool

(define (palindrome? nat)
  (local [(define nat-list (string->list (number->string nat)))]
    (equal? nat-list (reverse nat-list))))


;; (largest-palindrome nat-a nat-b record) produces the largest palindrome which
;;   is a product of two natural numbers less than a maximum.
;; largest-palindrome: Nat Nat Nat -> Nat

(define (largest-palindrome nat-a nat-b record)
  (cond [(< nat-a (/ record 3-digit-max)) record]
        [(< nat-b (/ record nat-a))
         (largest-palindrome (sub1 nat-a) 3-digit-max record)]
        [(palindrome? (* nat-a nat-b))
         (largest-palindrome (sub1 nat-a) 3-digit-max
                             (max record (* nat-a nat-b)))]
        [else
         (largest-palindrome nat-a (sub1 nat-b) record)]))

;(largest-palindrome 3-digit-max 3-digit-max 0)


;; ==== Question 5 ========================


;; (factor nat factor-list) factors a natural number recursively and produces a
;;   list of prime factors in ascending order.
;; factor: Nat (listof Nat) -> (listof Nat)

(define (factor nat factor-list)
  (cond [(= 1 nat) factor-list]
        [else (local [(define next-factor (largest-factor nat empty))]
                (factor (/ nat next-factor) (cons next-factor factor-list)))]))


;; (num-list-union list1 list2) finds the union of two lists of numbers, sorted
;;   in non-decreasing order, maintaining their sorting order.
;; num-list-union: (listof Num) (listof Num) -> (listof Num)

(define (num-list-union list1 list2)
  (cond [(empty? list1) list2]
        [(empty? list2) list1]
        [(= (first list1) (first list2))
         (cons (first list1) (num-list-union (rest list1) (rest list2)))]
        [(< (first list1) (first list2))
         (cons (first list1) (num-list-union (rest list1) list2))]
        [else
         (cons (first list2) (num-list-union list1 (rest list2)))]))
  

;; (smallest-multiple ceiling) returns the smallest natural number which is
;;   divisible by all of the natural numbers from 1 to the ceiling.
;; smallest-multiple: Nat -> Nat

(define (smallest-multiple ceiling)
  (foldr * 1
         (foldr (lambda (x y) (num-list-union x y))
                empty
                (build-list ceiling (lambda (x) (factor (add1 x) empty))))))

;(smallest-multiple 20)


;; ==== Question 6 ========================


;; (sum-of-squares num-list) produces the sum of the squares of a list.
;; sum-of-squares: (listof Num) -> Num

(define (sum-of-squares num-list)
  (foldr (lambda (x y) (+ (sqr x) y)) 0 num-list))


;; (square-of-sum num-list) produces the square of the sum of a list.
;; square-of-sum (listof Num) -> Num

(define (square-of-sum num-list)
  (sqr (foldr + 0 num-list)))


;; (sum-square-difference ceiling) produces the difference between the sum of
;;   the squares and the square of the sum of all the natural numbers up to
;;   and including ceiling.
;; sum-square-difference: Nat -> Nat

(define (sum-square-difference ceiling)
  (local [(define int-list (build-list ceiling add1))]
    (abs (- (sum-of-squares int-list) (square-of-sum int-list)))))

;(sum-square-difference 100)


;; ==== Question 7 ========================


;; (nth-prime n prime-list) produces the nth prime.
;; nth-prime: Nat (listof Nat) -> Nat

(define (nth-prime n prime-list)
  (cond [(= n (length prime-list)) (first prime-list)]
        [else (nth-prime n (cons (next-prime prime-list) prime-list))]))

;(nth-prime 10001 empty)


;; ==== Question 8 ========================


(define series
  (map string->number
       (explode (string-append 
                 "73167176531330624919225119674426574742355349194934"
                 "96983520312774506326239578318016984801869478851843"
                 "85861560789112949495459501737958331952853208805511"
                 "12540698747158523863050715693290963295227443043557"
                 "66896648950445244523161731856403098711121722383113"
                 "62229893423380308135336276614282806444486645238749"
                 "30358907296290491560440772390713810515859307960866"
                 "70172427121883998797908792274921901699720888093776"
                 "65727333001053367881220235421809751254540594752243"
                 "52584907711670556013604839586446706324415722155397"
                 "53697817977846174064955149290862569321978468622482"
                 "83972241375657056057490261407972968652414535100474"
                 "82166370484403199890008895243450658541227588666881"
                 "16427171479924442928230863465674813919123162824586"
                 "17866458359124566529476545682848912883142607690042"
                 "24219022671055626321111109370544217506941658960408"
                 "07198403850962455444362981230987879927244284909188"
                 "84580156166097919133875499200524063689912560717606"
                 "05886116467109405077541002256983155200055935729725"
                 "71636269561882670428252483600823257530420752963450"))))


;; (largest-product-in-series num-list num-len acc) finds the list with length
;;   num-len within num-list that has the greatest product, and produces the
;;   product.
;; largest-product-in-series: (listof Nat) Nat Nat -> Nat
;; requires: all elements of num-list <= 9 (e.g. single digit Nat)

(define (largest-product-in-series num-list num-len acc)
  (local [;; (product num-list num-len) produces the product of the first
          ;;   num-len items of num-list.
          ;; product: (listof Nat) Nat -> Nat
          
          (define (product num-list num-len)
            (cond [(= 0 num-len) 1]
                  [else (* (first num-list)
                           (product (rest num-list) (sub1 num-len)))]))]
    
    (cond [(> num-len (length num-list)) acc]
          [else
           (largest-product-in-series (rest num-list)
                                      num-len
                                      (max acc (product num-list num-len)))])))

;(largest-product-in-series series 13 0)


;; ==== Question 9 ========================


;; (pythag-trip sum) finds the first pythagorean triple (a, b, c) such that
;;    a + b + c = sum, and then produces the product abc.
;; pythag-trip: Nat -> Nat

(define (pythag-trip sum)
  (local [;; (try-next a b) recurses until a valid triple is found, and then
          ;;   produces the product abc.
          ;; try-next: Nat Nat -> Nat
          
          (define (try-next a b)
            (local [(define c (sqrt (+ (sqr a) (sqr b))))]
              (cond [(and (integer? c) (= 0 (remainder sum (+ a b c))))
                     (* (expt (/ sum (+ a b c)) 3) a b c)]
                    [(< sum (+ a b c)) (try-next (add1 a) 1)]
                    [(even? a) (try-next a (add1 b))]
                    [(even? b) (try-next a (+ 2 b))]
                    [else (try-next a (add1 b))])))]
    
    (try-next 1 1)))

;(pythag-trip 1000)