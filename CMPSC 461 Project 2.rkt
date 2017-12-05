;CMPSC 461
;Project 2
;Brianna Drummond (bld5292@psu.edu)


;--------------------------------------------------------------------------------------------------------------------------------------------------------

;1. Function to get maximum number in a list.

;(a). Helper function.
(define (maxInt_helper k x) ;Helper function for maxInt, takes a number and a list of numbers.
  (cond ((null? x) k) ;If the list is empty, returns k as the max number.
        ((> (car x) k) (maxInt_helper (car x) (cdr x))) ;If first element of x is > k, call helper function with k = car x and x = cdr x
        (else (maxInt_helper k (cdr x))))) ;If k > first element of x, call helper with k and the rest of list x

;(b). Main maxInt function.
(define (maxInt l) ;Function maxInt takes a list of integers and finds the maximum integer
  (cond ((null? l) '(Error: Empty list)) ;Returns an error if the list is empty
        (else (maxInt_helper (car l) (cdr l))))) ;If list is not empty, calls helper function with k = car l and x = cdr l

;------------------------------------------------------------------------------------------------------------------------------------------------------

;2. Zip function returns a list of pairs of items from two input lists.

(define (zip l1 l2) ;Function zip takes two lists as input
  (cond ((not (equal? (length l1) (length l2))) '(Error: List lengths not equal)) ;If lists are not the same length, returns an error
        (else (map list l1 l2)))) ;If input is valid, map goes through each element and makes sub-lists with the pairs

;------------------------------------------------------------------------------------------------------------------------------------------------------

;3. Function to compute value of single-variable polynomial.

(define (compute l x) ;Function compute takes a list representing the polynomial and a number x
  (define (compute_helper l x current_result) ;Helper function takes a list, a number x, and the current result of the polynomial calculation
    (cond ((null? l) current_result) ;If list is empty, returns the result of the polynomial calculation
          (else (compute_helper (cdr l) x (+ (* x current_result) (car l)))))) ;Else it calls the helper with cdr of the list, x, and new current result
  (if (null? l) '(Error: Empty list) ;If user input list is empty, returns an error
      (compute_helper (reverse l) x 0))) ;Else it calls the helper function with the reversed list, x, and 0

;------------------------------------------------------------------------------------------------------------------------------------------------------

;4. Function that takes a number between 0 and 99 and returns the number as it's corresponding english word/s.

(define (int-words-helper n);Helper function for int-to-words function, takes an integer n
  (cond ((equal? n 0) '()) ;If n is 0, returns an empty list
        ((equal? n 1) '(one))    ;For digits 1-9, returns
        ((equal? n 2) '(two))    ;a list of the english word that
        ((equal? n 3) '(three))  ;represents the integer
        ((equal? n 4) '(four))
        ((equal? n 5) '(five))
        ((equal? n 6) '(six))
        ((equal? n 7) '(seven))
        ((equal? n 8) '(eight))
        ((equal? n 9) '(nine))))

(define (int-to-words n) ;Funtion int-to-words takes an integer n and returns the english words representing that number
  (cond ((or (< n 0) (> n 99)) '(Error: input is not between 0-99)) ;If the nuber is not between 0-99, returns an error
        ((equal? n 0) '(zero)) ;If the number is 0, return (zero)
        ((equal? (quotient n 10) 0) (int-words-helper n)) ;If n is between 1-9, returns value from call to helper function with n
        ((equal? n 10) '(ten))      ;If n is between 10-19, returns
        ((equal? n 11) '(eleven))   ; the specific english name for
        ((equal? n 12) '(twelve))   ; each number
        ((equal? n 13) '(thirteen))
        ((equal? n 14) '(fourteen))
        ((equal? n 15) '(fifteen))
        ((equal? n 16) '(sixteen))
        ((equal? n 17) '(seventeen))
        ((equal? n 18) '(eighteen))
        ((equal? n 19) '(nineteen)) 
        ((equal? (quotient n 10) 2) (append '(twenty) (int-words-helper (modulo n 10)))) ;If n is > 19, returns 
        ((equal? (quotient n 10) 3) (append '(thirty) (int-words-helper (modulo n 10)))) ; a list of the first word (twenty, fifty, etc.)
        ((equal? (quotient n 10) 4) (append '(forty) (int-words-helper (modulo n 10))))  ; appended with list returned by calling
        ((equal? (quotient n 10) 5) (append '(fifty) (int-words-helper (modulo n 10))))  ; helper function with the remainder
        ((equal? (quotient n 10) 6) (append '(sixty) (int-words-helper (modulo n 10))))  ; of n divided by 10
        ((equal? (quotient n 10) 7) (append '(seventy) (int-words-helper (modulo n 10))))
        ((equal? (quotient n 10) 8) (append '(eighty) (int-words-helper (modulo n 10))))
        ((equal? (quotient n 10) 9) (append '(ninety) (int-words-helper (modulo n 10))))))

;------------------------------------------------------------------------------------------------------------------------------------------------------

;5. Funtion to calculate word counts.


;(a). Function that takes a list of words and creates a word-count list with count of 1 for each word.
(define (initialWCList l) ;Parameter l is the list of words
  (define (make-ones-list n) ;This function creates a list of the same length but filled with ones, to be used in map with l
    (if (= n 0) (list 1) ;If n = 0, return (1)
        (cons 1 (make-ones-list (- n 1))))) ;If n != 0, cons 1 with a recursive call to function with parameter n-1
  (if (null? l) '(Error: Empty list) ;If list is empty, return error
      (map list l (make-ones-list (- (length l) 1))))) ;If list not empty, make a list of lists pairing each word with 1


;(b). Function that takes a word-count pair and a word-count list and returns a new word-count list with updated word counts.
(define (mergeWC p l) ;Parameter p is the word-count pair, parameter l is the word-count list
  (define (replace-member listIn find replaceWith) ;This function finds member "find" from "listIn" and replaces it with "replaceWith"
    (cond ((equal? listIn find) replaceWith) ;If the input list and the item to find are the same, return replaceWith
          ((not (pair? listIn)) listIn)      ;If input list is not a pair, return input list
          (else (cons (replace-member (car listIn) find replaceWith)     ;Recursively calls replace-member to replace the
                      (replace-member (cdr listIn) find replaceWith))))) ; desired members with the new member
  (cond ((or (null? p) (null? l)) '(Error: Empty list)) ;If either list is empty, returns an error
        ((not (assoc (car p) l)) (append l (list p))) ;If the word is not in the list, appends pair to list
        (else (replace-member l                 ;If word is in list, calls replace-member function to replace list member
                              (assoc (car p) l) ; with new member with updated count
                              (list (car p) (+ (cadr p) (cadr (assoc (car p) l))))))))


;(c). Function that takes a word-count list and produces a new word-count list with the updated counts for any repeated words.
(define (mergeByWord l)
  (define (reduce f l v) ;Defining reduce function as a helper: takes a function f, a list l, and a value v
    (if (null? l) v ;If list is empty, returns value v
        (f (car l) (reduce f (cdr l) v)))) ;If list is not empty, calls function f with first member of l and a recursive call to reduce
  (if (or (null? l) (member '() l)) '(Error: Empty list) ;If list is empty or contains an empty list, returns error
      (cdr (reduce mergeWC l (list '(0)))))) ;If list is not empty, calls reduce with parameter mergeWC on the list


;(d). Function that takes in a list of words and outputs the word-count list.
(define (wordcount l) ;Parameter l is the list of words
  (if (null? l) l ;If the list is empty, returns the empty list
      (mergeByWord (initialWCList l)))) ;If list is not empty, calls mergeByWord on the list returned by calling initialWCList on l
