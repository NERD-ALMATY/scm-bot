;;; Code from SICP
;;; https://mitpress.mit.edu/sites/default/files/sicp/code/index.html

(import (chicken condition))
(import (only (chicken random)
              pseudo-random-integer))

(define apply-in-underlying-scheme apply)

(define threshold-eval 500000)

(define (eval-exp exp env eval-count)
  (when (> eval-count threshold-eval)
    (abort 'too-long-eval))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-exp-assignment exp env (+ 1 eval-count)))
        ((definition? exp) (eval-exp-definition exp env (+ 1 eval-count)))
        ((if? exp) (eval-exp-if exp env (+ 1 eval-count)))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-exp-sequence (begin-actions exp) env (+ 1 eval-count)))
        ((cond? exp) (eval-exp (cond->if exp) env (+ 1 eval-count)))
        ((and? exp) (eval-and (cdr exp) env (+ 1 eval-count)))
        ((or? exp) (eval-or (cdr exp) env (+ 1 eval-count)))
        ((application? exp)
         (apply-exp (eval-exp (operator exp) env (+ 1 eval-count))
                    (list-of-values (operands exp) env eval-count)
                    (+ 1 eval-count)))
        (else
         (abort 'unknown-exp-type))))

(define (apply-exp procedure arguments eval-count)
  (cond ((primitive-procedure? procedure)
         (apply-exp-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-exp-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))
          eval-count))
        (else
         (abort 'unknown-proc-type))))

(define (list-of-values exps env eval-count)
  (if (no-operands? exps)
      '()
      (cons (eval-exp (first-operand exps) env (+ 1 eval-count))
            (list-of-values (rest-operands exps) env (+ 1 eval-count)))))

(define (eval-exp-if exp env eval-count)
  (if (true? (eval-exp (if-predicate exp) env (+ 1 eval-count)))
      (eval-exp (if-consequent exp) env (+ 1 eval-count))
      (eval-exp (if-alternative exp) env (+ 1 eval-count))))

(define (eval-exp-sequence exps env eval-count)
  (cond ((last-exp? exps) (eval-exp (first-exp exps) env (+ 1 eval-count)))
        (else (eval-exp (first-exp exps) env (+ 1 eval-count))
              (eval-exp-sequence (rest-exps exps) env (+ 1 eval-count)))))

(define (eval-exp-assignment exp env eval-count)
  (set-variable-value! (assignment-variable exp)
                       (eval-exp (assignment-value exp) env (+ 1 eval-count))
                       env)
  'ok)

(define (eval-exp-definition exp env eval-count)
  (define-variable! (definition-variable exp)
    (eval-exp (definition-value exp) env (+ 1 eval-count))
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      '#f))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '#f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                #;(error "ELSE clause isn't last -- COND->IF"
                       clauses)
                (abort 'else-is-not-last))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-and exp env eval-count)
  (cond ((no-operands? exp)
         #t)
        ((eval-exp (first-operand exp) env (+ 1 eval-count))
         (eval-and (rest-operands exp) env (+ 1 eval-count)))
        (else #f)))

(define (eval-or exp env eval-count)
  (cond ((no-operands? exp)
         #f)
        ((eval-exp (first-operand exp) env (+ 1 eval-count))
         #t)
        (else
         (eval-or (rest-operands exp) env eval-count))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        #;(error 'lookup-variable-value "Unbound variable" var)
        (abort 'unbound-variable)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        #;(error 'set-variable-value!
               "Unbound variable -- SET!" var)
        (abort 'unbound-variable)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    ;; (define-variable! '#t #t initial-env)
    ;; (define-variable! '#f #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'list list)
        (list 'list-ref list-ref)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'remainder remainder)
        (list 'even? even?)
        (list 'odd? odd?)
        (list 'not not)
        (list 'map map)
        (list 'random pseudo-random-integer)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-exp-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
