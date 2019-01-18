;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 40-49) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; **************************
;;   Project Euler
;; **************************
;;

;; ==== Question 40 ========================


(define (nth-digit n)
  (local [(define (find-digit n current)
            (local [(define num-len (string-length (number->string current)))]
              (cond [(< num-len n)
                     (find-digit (- n num-len) (add1 current))]
                    [else (modulo (floor (/ current
                                            (expt 10 (- num-len n)))) 10)])))]
    (find-digit n 1)))

;(* (nth-digit 1) (nth-digit 10) (nth-digit 100) (nth-digit 1000)
;   (nth-digit 10000) (nth-digit 100000) (nth-digit 1000000))


;; ==== Question 41 ========================


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


;; primes-to-1b is a list of all primes up to 31427, good for checking primes
;;   up to 1 billion.

(define primes-to-1b (sieve (build-list 31427 add1)))

;; (prime-sub-1b? nat) determines if the nat is a prime.
;; prime?: Nat -> Bool
;; requires: nat < 1000000000

(define (prime-sub-1b? nat)
  (local [(define (primality nat prime-list)
            (cond [(empty? prime-list) true]
                  [(= 1 nat) false]
                  [(> (first prime-list) (sqrt nat)) true]
                  [(= (first prime-list) nat) true]
                  [(zero? (remainder nat (first prime-list))) false]
                  [else (primality nat (rest prime-list))]))]
    (primality nat primes-to-1b)))


(define (pandigital? nat)
  (local [(define len (string-length (number->string nat)))]
    (equal? (quicksort (explode (number->string nat)) <)
            (build-list len add1))))