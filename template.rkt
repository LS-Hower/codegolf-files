#lang racket

;; TODO: 支持 program 使用输出过程而不止（可能多值的）求值结果
;; TODO: 支持 Code 部分中编写过程（及变量向过程的绑定），支持使用函数的参数和返回值做 IO

;; This template is suitable for: https://tio.run/#racket & https://codegolf.stackexchange.com/

;; 'Code' part itself is also a full program.
;; A trick is used to encapsulate it into a procedure, allowing it to be tested multiple times.

(define program (quote

; -------- Code part start --------
(let*([a(read)][b(read)])(values(apply + a)(apply * b)))
; -------- Code part end --------

))

#|
Test cases in question description:
[1, 2, 3], [4, 5, 6] -> 6, 120
[1, 6], [-5, -8, 10] -> 7, -400
[], [] -> 0, 1
|#
(define input-data
  '(((1 2 3) (4 5 6))
    ((1 6) (-5 -8 -10))
    (() ())))
;; Multi-value evaluation output is written as a list.
(define output-data-expected
  '((6 120)
    (7 -400)
    (0 1)))

(define (space-joined-text to-string ls)
  (string-join (map to-string ls)))

(define (test input-data output-data-expected f)
  (let* ([output-data-real (map (compose list f) input-data)]
         [summary (map equal? output-data-real  output-data-expected)]
         [summary-strings (map (compose second (curryr assv '((#t "OK") (#f "!!")))) summary)])

    (for ([i (in-naturals 1)]
          [in (in-list input-data)]
          [out-expected (in-list output-data-expected)]
          [out-real (in-list output-data-real)]
          [okay summary-strings])
      (printf "[~a] Test case #~a~n" okay i)
      (printf "  input:           ~a~n" (space-joined-text ~a in))
      (printf "  output:          ~a~n" (space-joined-text ~a out-real))
      (printf "  expected output: ~a~n" (space-joined-text ~a out-expected))
      (printf "~n"))

    (printf "Summary:~n")
    (printf "  all: ~a~n" summary-strings)
    (printf "  OK: ~a~n" (map add1 (indexes-of summary-strings "OK")))
    (printf "  !!: ~a~n" (map add1 (indexes-of summary-strings "!!")))))

(define (program->procedure program [module-names '()])
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (for ([name module-names])
      (namespace-require name)))
  (λ (datum)
    (with-input-from-string
     (space-joined-text ~s datum)
     (thunk (eval program ns)))))

(test input-data
      output-data-expected
      (program->procedure program))
