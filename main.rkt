#lang racket
(require "utility.rkt")
(require "runner.rkt")
(require "parcer.rkt")
(require "variable_env.rkt")

(define env '((a 1) (b 2) (c 5)))

(define sample-code '(call (function () (ask (bool != a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

;(let* ((alex 21) (bushi 22) (blade 35) (amy 7) (substract (lambda(a b)(- a b))) (amy 5)) (substract alex amy))
;(run-bool-parsed-code 'a env)
(elementAt '(!= (var-exp a) (var-exp b)) 1)