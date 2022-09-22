;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname neo-language) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;variable scope/environment
;() | ((varname var_value)....)
(define env '((a 1) (b 2) (c 5)))
;resolve a
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname))
      )
    )
  )

;(resolve env 'a)

(define neo-parser
  (lambda (neo-code)
    (cond
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ;(math op num1 num2) > (math-exp op (neo-exp) (neo-exp))
      ((equal? (car neo-code) 'math)
       (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code))))
      ;(function (x y z,...) x)
      ((equal? (car neo-code) 'function)
       (list 'func-exp
             (list 'params (caadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code)))))
      ((equal? (car neo-code) 'call)
       (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code))))
      (else #false)
      )
    )
  )


;(display (neo-parser sample-code))
;(display (de-neo-parser (neo-parser sample-code)))

;(app-exp (func-exp (params x) (body-exp (var-exp x))) (var-exp a))
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ;(math-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      (else (run-neo-parsed-code
             (cadr parsed-code) ;function expression
             (cons (list (cadr (cadr (cadr parsed-code))) ;(x 1) (1 is the value of a)
                         (run-neo-parsed-code (caddr parsed-code) env))
                   env) ;environment scope update
             )
            )
      )
    )
  )

;tool to deal with math
(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false)
      )
    )
  )

;(define env '((a 1) (b 2) (c 5)))
(define sample-code '(call (function(x) (math + 1 1)) 4))
(display (neo-parser sample-code))
(run-neo-parsed-code (neo-parser sample-code) env)
;(define sample-code '(function(x) (math + x x)))
;(display (neo-parser sample-code))
