#lang typed/racket

(require megaparsack megaparsack/text)
(require/typed megaparsack/text [integer/p (parser/c char? integer?) ])

;;(require typed)

(: input (Listof String))
(define input (string-split (file->string "inputs/2024/day1.txt") "\n"))

(parse-string integer/p "123")



;;(: location-pair (-> String (U (Pairof Integer Integer) False) ))
;;(define (location-pair str) 
;;  (let* (
;;    [l (string-split str "   ")]
;;    [x : (U Complex False) (string->number (first l))])
;;    (cons 1 0)
;;  )
;;)

;;(: unzip (-> (Listof (Pairof Number Number)) (Pairof (Listof Number) (Listof Number)) ))
;;(define (unzip al) (cons (map car al) (map cdr al)))


(: part1 (-> (List Integer) (List Integer) Integer))
(define (part1 as bs) (apply + (map abs (map - as bs)))) 

(: part2 (-> (List Integer) (List Integer) Integer))
(define (part2 as bs) 0) 



;;(let* (
;;  [locations (map location-id input)]
;;  [transpose (unzip locations)]
;;  [diff (map - (sort (first transpose) <) (sort (second transpose) <))])
;;  (apply + (map abs diff)))


