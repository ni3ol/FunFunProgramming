;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      CS 3 Scheme programming assignment               ;;;;
;;;;                 April 2016                            ;;;;
;;;;        Nicol Vojacek solutions VJCNIC001              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;; This depends on "cs3-black-jack.scm"
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 1.  Code for "best-hand"

;; The best hand function chooses what the highest hand size is depending on 
;; if Aces are in play. Aces can represent 1 or 11, and best-hand returns the 
;; highest hand size possible that is smaller than 21

;; Test Cases 
;; > (best-hand '((A d) (8 s)))
;; 19
;; > (best-hand '((A d) (8 s) (5 h)))
;; 14
;; > (best-hand '((A d) (A s) (8 s)))
;; 20
;;> (best-hand '((A d) (A s) (9 h)))
;;21



(define (aces-exist hand)
     (if (null? hand) 0 
         (if (equal? (car (car hand)) 'A) 1
	       (aces-exist (cdr hand)))))

(define (best-hand hand)
   (if (equal? (aces-exist hand) 1) 
       (if (<= (+ (min-val hand) 10) 21) 
            (+ (min-val hand) 10) 
               (min-val hand))  
                   (min-val hand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 2.  "stop-at"
;; This function determines a strategy when given an argument n where a 
;; card is only taken if the best hand is much less than n

;; Test cases 

; > (black-jack(stop-at 2))


; player-hand: Qs6d
; Player outcome: Qs6d
; dealer-hand: Qh7d8s
; 1

; > (black-jack(stop-at 14))


; player-hand: 6s9c
; Player outcome: 6s9c
; dealer-hand: 6dJs3d

; -1

; > (black-jack(stop-at 10))


; player-hand: 8d9h
; Player outcome: 8d9h
; dealer-hand: Kd4s3h

; 0

; > (black-jack(stop-at 20))


; player-hand: 3c4h
; Player outcome: 10dJc3c4h
; -1

 (define (stop-at n)
   (lambda (my-hand dealt-card) 
     (< (best-hand my-hand) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Question 3.  "repeat-game"
;; Repeat game is a function that returns the number of games won minus lost
;; for a given strategy for the number of rounds repeated. 

;; Test Cases

; > (repeat-game (stop-at 20)5) 


; player-hand: Qs6d
; Player outcome: 7dQs6d


; player-hand: 5s2s
; Player outcome: 3s6s6h5s2s


; player-hand: 6s9c
; Player outcome: Js6s9c


; player-hand: 8d9h
; Player outcome: 4s8d9h
; dealer-hand: JhKd3h



; player-hand: 3c4h
; Player outcome: 10dJc3c4h
; -3

; > (repeat-game (stop-at 10)10)


; player-hand: Qs6d
; Player outcome: Qs6d
; dealer-hand: Qh7d8s



; player-hand: 5s2s
; Player outcome: 6h5s2s
; dealer-hand: 3s6sJd



; player-hand: 6s9c
; Player outcome: 6s9c
; dealer-hand: 6dJs3d



; player-hand: 8d9h
; Player outcome: 8d9h
; dealer-hand: Kd4s3h



; player-hand: 3c4h
; Player outcome: Jc3c4h
; dealer-hand: 10dKs



; player-hand: KcAh
; Player outcome: KcAh
; dealer-hand: 10c8dAc4s



; player-hand: 10s5d
; Player outcome: 10s5d
; dealer-hand: 6dKs2s



; player-hand: 6dAd
; Player outcome: 6dAd
; dealer-hand: 10s6s9d



; player-hand: 7s3c
; Player outcome: 7s3c
; dealer-hand: 6h6d5d



; player-hand: 6h7c
; Player outcome: 6h7c
; dealer-hand: Js3sKc2d

; -1

; > (repeat-game (stop-at 17)5)


; player-hand: Qs6d
; Player outcome: 7dQs6d


; player-hand: 5s2s
; Player outcome: 6s6h5s2s
; dealer-hand: 9d3sJd



; player-hand: 6s9c
; Player outcome: Js6s9c


; player-hand: 8d9h
; Player outcome: 8d9h
; dealer-hand: Kd4s3h



; player-hand: 3c4h
; Player outcome: Jc3c4h
; dealer-hand: 10dKs

; -2 

 (define repeat-help 
  (lambda (strategy n score)
   (if (= n 0) score
         (repeat-help strategy (- n 1) (+ score (black-jack strategy))))))
   
 
 (define (repeat-game strategy n)
   (repeat-help strategy n 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Question 4.    clever
;; Clever is a strategy that considers both the dealer's and players hand and
;;follows the following condtions in executing: 
; if the player has 11 or less you always take a hit.  
; If it is 17 or higher you always stand. 
; If the player has 12 you stand iff the dealer’s up card is 4, 5
;  or 6, otherwise hit. 
; If the player has ≥13 and ≤16 then
;  take a hit iff the dealer ha
; s an ace or any card of value 7 or 
; higher showing, otherwise stand

;;Test Cases 

; > (clever '((2 c) (9 h)) '(6 c))
; #t

; > (clever '((7 c) (9 h)) '(6 c))
; #f


; > (clever '((4 c) (4 h)) '(8 c))
; #t

; > (clever '((5 c) (6 h)) '(9 c))
; #t

; > (clever '((8 c) (A h)) '(9 c))
; #f

; > (clever '((3 c) (9 h)) '(6 c))
; #f

; > (clever '((7 c) (5 h)) '(6 c))
; #f

; > (clever '((7 c) (5 h)) '(4 c))
; #f

; > (clever '((7 c) (5 h)) '(5 c))
; #f

; > (clever '((7 c) (5 h)) '(A c))
; #t

; > (clever '((9 c) (5 h)) '(A c))
; #t


(define (clever my-hand up-card) 
  (cond ((<= (best-hand my-hand) 11) #t)
        ((>= (best-hand my-hand) 17) #f)
        ((and (= (best-hand my-hand) 12) (not (or (= (min-val (list up-card)) 4) (= (min-val (list up-card)) 5) (= (min-val (list up-card)) 6)))) #t)        
        ((and (and (>= (best-hand my-hand) 13) (<= (best-hand my-hand)) 16) (or (equal? (aces-exist (list up-card)) 1)  (>= (best-hand(list up-card)) 7))) #t)
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Question 5.                Majority
;;Majority takes three different strategies and and produces a strategy as a result
;;The majority strategy only executes if two of the three strategies chosen would 
;; have hit for the given input

;;Test Cases 

; > ((majority stop-at-17 (stop-at 20) clever) '((5 d) (4 s)) '(2 h))
; #t

; > ((majority stop-at-17 (stop-at 20) clever) '((A d) (9 s)) '(8 h))
; #f


(define (majority strategy-a strategy-b strategy-c)
  (lambda (my-hand dealt-card)
     
          (if (>= (+ (+ (get-stategy strategy-a my-hand dealt-card) (get-stategy strategy-b my-hand dealt-card)) (get-stategy strategy-c my-hand dealt-card)) 2) #t #f)
          )
        )

        (define (get-stategy strategy my-hand dealt-card)
          (if (equal? (strategy my-hand dealt-card) #t) 1 0)       
        )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Question 6.              Get Stats
;;Get Stats repeats a stategy a number of times for a number of game rounds 
;;And returns the number of games won minus lost for each data point. 

;;Test Cases

; > (get-stats stop-at-17 100 10)
; (-30 -2 -18 7 10 -5 -10 1 -1 4)

; (get-stats stop-at-17 10 10)
; (-5 -5 5 0 -2 0 1 0 -3 1)

; (get-stats (stop-at 14) 100 10)
; (-15 13 -18 -8 -3 4 3 -28 -12 -3)


(define (stat-list strategy repeat-count data-points)
  (cond ((= data-points 0)'())
        (else (append (list (repeat-game strategy repeat-count)) (get-stats strategy repeat-count (- data-points 1))))))

(define (get-stats strategy repeat-count data-points)
   (stat-list strategy repeat-count data-points))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Question 8.   interactive 
;;This function is an interactive version of the blackjack game that allows
;;the user to input the decision to hit or stand. 

;;Test Cases

; > (black-jack hit?)


; player-hand: Kd4h
; Player's Hand Total: 14
; Dealer's Hand: 9s
; Do you want to hit again? y or n
; n
; Player outcome: Kd4h
; dealer-hand: 9h3c4d9s

; 1

; > (black-jack hit?)


; player-hand: 6h7s
; Player's Hand Total: 13
; Dealer's Hand: 4c
; Do you want to hit again? y or n
; y
; Player's Hand Total: 14
; Dealer's Hand: 4c
; Do you want to hit again? y or n
; y
; Player's Hand Total: 18
; Dealer's Hand: 4c
; Do you want to hit again? y or n
; n
; Player outcome: 4hAs6h7s
; dealer-hand: QhQd4c

; 1

; > (black-jack hit?)


; player-hand: 5d5s
; Player's Hand Total: 10
; Dealer's Hand: 10c
; Do you want to hit again? y or n
; y
; Player's Hand Total: 16
; Dealer's Hand: 10c
; Do you want to hit again? y or n
; y
; Player's Hand Total: 18
; Dealer's Hand: 10c
; Do you want to hit again? y or n
; n
; Player outcome: 2h6h5d5s
; dealer-hand: 8d10c

; 0

; > (black-jack hit?)


; player-hand: 2hQs
; Player's Hand Total: 12
; Dealer's Hand: 8s
; Do you want to hit again? y or n
; y
; Player's Hand Total: 14
; Dealer's Hand: 8s
; Do you want to hit again? y or n
; y
; Player outcome: 10h2d2hQs
; -1

; function to get the input returns #t if the user types y otherwise #f
(define (hit-me?)
  (eq? (read) 'y))

(define (hit? my-hand up-card) 
  (display "Player's Hand Total: ") 
  (display (best-hand my-hand))
  (newline)
  (display "Dealer's Hand: ") 
  (display up-card)
  (newline)
  (display "Do you want to hit again? y or n")
  (newline)
  (hit-me?))
  