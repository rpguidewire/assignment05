;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "namelist.rkt")

;(first name-list)
;(first (rest name-list))

;;Question 1

;;Data Definition 
;;A Namelist is one of 
;;Empty
;;(cons Nameinfo Namelist)

;;Template 
;(define (my-nameinfolist-fn anameinfo-list)
;  (cond 
;    [(empty? anameinfo-list) ...]
;    [else ...(my-nameinfo-fn (first anameinfo-list))...
;          ...(my-nameinfolist-fn (rest anameinfo-list))...]))
;(define (my-nameinfo-fn anameinfo)
;  (...(nameinfo-name anameinfo)....
;   ...(nameinfo-decade anameinfo)...
;   ...(nameinfo-rank anameinfo)....
;   ...(nameinfo-gender anameinfo)...))

;;Examples/Testing Data 
(define triallst1  (list (make-nameinfo "Holly" 2000 192 'Female)
                         (make-nameinfo "Holly" 2010 290 'Female)
                         (make-nameinfo "Holly" 1990 948 'Female)
                         (make-nameinfo "Holly" 2020 50 'Female)
                         (make-nameinfo "Zack" 2000 102 'Male)
                         (make-nameinfo "Ali" 2010 10 'Male)
                         (make-nameinfo "Robert" 2000 5 'Male)
                         (make-nameinfo "Aaron" 2010 8 'Male)
                         (make-nameinfo "Aaron" 2010 50 'Female)
                         (make-nameinfo "Aaron" 2000 292 'Female)
                         (make-nameinfo "Aaron" 1990 892 'Female)))

      
;;Question 2
;; find-rank: Namelist String Symbol Nat --> Int

;;The porpose of this function is to take in a Namelist, namelst; a name, name; the gender, 
;;gender and the decade, decade and return the rank of the name for the given decade and gender. 

;;Examples 
(check-expect (find-rank triallst1 "Katie" 'Female 2010) false)
(check-expect (find-rank triallst1 "Aaron" 'Female 2000) 292)
(check-expect (find-rank triallst1 "Aaron" 'Female 2010) 50)

;;Funtion Definition 
(define (find-rank namelst name gender decade)
  (cond 
    [(empty? namelst) false]
    [(and (equal? name (nameinfo-name (first namelst)))
          (equal? decade (nameinfo-decade (first namelst)))
          (equal? gender (nameinfo-gender (first namelst)))) (nameinfo-rank (first namelst))]
    [else (find-rank (rest namelst) name gender decade)]))

;;Tests
(check-expect (find-rank triallst1 "Holly" 'Female 1990) 948)
(check-expect (find-rank triallst1 "Zack" 'Male 2000) 102)
(check-expect (find-rank triallst1 "Aaron" 'Male 2010) 8)
(check-expect (find-rank triallst1 "Aaron" 'Male 2000) false)
(check-expect (find-rank triallst1 "Holly" 'Female 2010) 290)
(check-expect (find-rank triallst1 "Aaron" 'Female 2020) false)
(check-expect (find-rank triallst1 "Ali" 'Female 2010) false)
(check-expect (find-rank triallst1 "Holly" 'Female 2020) 50)

;;Question 3
;;collect-name: Namelist String Symbol --> Namelist

;;The purpose of this function is to take in a Namelist, namelst; a name, name, and a gender, gender and return 
;;while maintaining the order a namelist of all the nameinfo of matching name and gender. 

;;Examples 
(check-expect (collect-name empty "Suzie" 'Female) empty)
(check-expect (collect-name (list (make-nameinfo "Marcus" 2010 190 'Male)
                                  (make-nameinfo "Marcus" 2010 920 'Female)
                                  (make-nameinfo "Mark" 2010 290 'Male)) "Marcus" 'Male) 
              (list (make-nameinfo "Marcus" 2010 190 'Male)))

;;Function Definition 
(define (collect-name namelst name gender)
  (cond 
    [(empty? namelst) empty]
    [(and (equal? name (nameinfo-name (first namelst)))
          (equal? gender (nameinfo-gender (first namelst))))
     (cons (first namelst) (collect-name (rest namelst) name gender))]
     [else (collect-name (rest namelst) name gender)]))

;;Tests 
(check-expect (collect-name triallst1 "Holly" 'Female) (list
                                                        (make-nameinfo "Holly" 2000 192 'Female)
                                                        (make-nameinfo "Holly" 2010 290 'Female)
                                                        (make-nameinfo "Holly" 1990 948 'Female)
                                                        (make-nameinfo "Holly" 2020 50 'Female)))
(check-expect (collect-name triallst1 "Ali" 'Male) (list (make-nameinfo "Ali" 2010 10 'Male)))
(check-expect (collect-name triallst1 "Zack" 'Female) empty)
(check-expect (collect-name triallst1 "Megan" 'Female) empty)
(check-expect (collect-name triallst1 "Aaron" 'Female) (list (make-nameinfo "Aaron" 2010 50 'Female)
                                                            (make-nameinfo "Aaron" 2000 292 'Female)
                                                            (make-nameinfo "Aaron" 1990 892 'Female)))


;;Question 4
;;first-n: Namelist Nat --> (union Namelist String)

;;The purpose of this function is to take in a Namelist,lst and a Nat, nat, and return the first 'nat' items 
;;in that list. 

;;Function Definition 
(define (first-n lst n)
  (cond
    [(member? 'NotEnoughItems (count-first-n lst n)) 'NotEnoughItems]
    [else (count-first-n lst n)]))


(check-expect (first-n (list 2 9 4 1 3 0) 2) (list 2 9))
(check-expect (first-n empty 2 ) 'NotEnoughItems)
(check-expect (first-n (list 2 3 4 5 ) 0) empty)
(check-expect (first-n (list 2 3 01 93 8 2 3 4 19) 10) 'NotEnoughItems)


;;Count-first-n: Namelist Nat --> (union Listof Nat Symbol)
;;Function Definition 
(define (count-first-n lst n)
  (cond 
    [(and (empty? lst)
          (not (= n 0))) (cons 'NotEnoughItems empty)]
    [(= n 0) empty]
    [else (cons (first lst) (count-first-n (rest lst) (sub1 n)))]))

;;Tests 
(check-expect (count-first-n (list 2 9 4 1 3 0) 2) (list 2 9))
(check-expect (count-first-n empty 2 ) (list 'NotEnoughItems))
(check-expect (count-first-n (list 2 3 4 5 ) 0) empty)
(check-expect (count-first-n (list 2 3 01 93 8 2 3 4 19) 10) (list 2 3 01 93 8 2 3 4 19 'NotEnoughItems))

;;Question 5

;;name-sort: (Listof Nameinfo) --> (Listof Nameinfo)

(define (name-sort lst)
  (cond
    [(empty? lst) empty]
    [else (name-insert (first lst) (name-sort (rest lst)))]))

;;;;name-insert: Nameinfo (Listof Nameinfo) --> (Listof Nameinfo)

(define (name-insert k slst)
  (cond 
    [(empty? slst) (cons k empty)]
    [(string<? (nameinfo-name k) (nameinfo-name (first slst))) (cons k slst)]
    [else (cons (first slst) (name-insert k (rest slst)))]))

(check-expect (name-sort triallst1)(list
                                    (make-nameinfo "Aaron" 1990 892 'Female)
                                    (make-nameinfo "Aaron" 2000 292 'Female)
                                    (make-nameinfo "Aaron" 2010 50 'Female)
                                    (make-nameinfo "Aaron" 2010 8 'Male)
                                    (make-nameinfo "Ali" 2010 10 'Male)
                                    (make-nameinfo "Holly" 2020 50 'Female)
                                    (make-nameinfo "Holly" 1990 948 'Female)
                                    (make-nameinfo "Holly" 2010 290 'Female)
                                    (make-nameinfo "Holly" 2000 192 'Female)
                                    (make-nameinfo "Robert" 2000 5 'Male)
                                    (make-nameinfo "Zack" 2000 102 'Male)))


;;Question 6

;;bar-graph: (Listof Nat)--> String
;;The purpose of this function is to take a list of nats, lst and draw number of stars equivalent to the 
;; nat, return as a string. 

;;Examples 
(check-expect (bar-graph (list 2 3 4 5 6)) "**\n***\n****\n*****\n******\n")
(check-expect (bar-graph (list 2 3 16)) "**\n***\n****************\n")
(check-expect (bar-graph (list 0 0 0)) "\n\n\n")

;;Function Definition 
(define (bar-graph lst)
  (list->string (printstar lst)))

;;Tests 
(check-expect (bar-graph (list 2 3 6)) "**\n***\n******\n")
(check-expect (bar-graph (list 3 4 3 4)) "***\n****\n***\n****\n")
(check-expect (bar-graph empty) "")

;;printstar: (Listof Nat) --> (Listof Char)
;;The purpose of this function is to take a list of nats, lst and draw number of stars equivalent to the 
;; nat, return as a listof charecters. 

;;Examples 
(check-expect (printstar (list 2 3 4 5)) (list #\* #\* #\newline #\* #\* #\* #\newline
                                               #\* #\* #\* #\* #\newline
                                               #\* #\* #\* #\* #\* #\newline))
(check-expect (printstar (list)) (list))

;;Function Definition 
(define (printstar lst)
  (cond
    [(empty? lst) empty]
    [(= 0 (first lst)) (cons #\newline (printstar (rest lst)))]
    [else (cons #\* (printstar (cons (sub1 (first lst)) (rest lst))))]))

;;Tests 
(check-expect (printstar (list 2 3 6)) (list #\* #\* #\newline #\* #\* #\* #\newline 
                                             #\* #\* #\* #\* #\* #\* #\newline))
(check-expect (printstar (list 3 4 3 4)) (list #\* #\* #\* #\newline #\* #\* #\* #\* #\newline #\* #\* #\* #\newline 
                                               #\* #\* #\* #\* #\newline))

(check-expect (printstar (list 0 0 0)) (list #\newline #\newline #\newline ))

;;Question 7
(define (diaplay-results lst)
  (display (bar-graph lst)))









