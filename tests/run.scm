(import (r7rs)
        (scheme)
        (scm-bot)
        (test))

(define init-env (setup-environment))

(test '42
      (eval-exp '42 the-empty-environment 0))

(test '42.1
      (eval-exp '42.1 the-empty-environment 0))

(test '#t
      (eval-exp '#t the-empty-environment 0))

(test '"string"
      (eval-exp '"string" the-empty-environment 0))

(test '100
      (eval-exp
       '(begin (define (p a)
                 (if (> a 0) (+ 1 (p (- a 1))) 0))
               (p 100))
       init-env
       0))

(define ev (make-eval (setup-environment)))

(test "ok"
      (eval-safe ev '(define (foo) 42)))

(test "#<compound-procedure>"
      (eval-safe ev 'foo))

(test "42"
      (eval-safe ev '(foo)))

(test "#t"
      (eval-safe ev '(and)))

(test "#f"
      (eval-safe ev '(or)))

(test "#f"
      (eval-safe ev '(and 1 #f 2)))

(test "#t"
      (eval-safe ev '(or #f #f 2 #f)))

(test '100
      (string->datum "100"))

(test ''()
      (string->datum "'()"))

(test "100"
      (string->datum "\"100\""))

(test '(define (foo) (foo))
      (string->datum "(define (foo) (foo))"))

(test-exit)
