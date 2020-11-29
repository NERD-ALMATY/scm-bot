(import (r7rs))
(define-library (scm-bot)
  (export eval-exp
          the-empty-environment
          setup-environment
          make-eval
          eval-safe
          eval-erase-env!
          eval-get-env
          send-log
          string->datum
          start-polling
          )
  (import (scheme)
          (chicken base)
          (chicken condition)
          (chicken irregex)
          (only (chicken port)
                call-with-input-string)
          (only (srfi-19)
                current-date
                date->string)
          (only (srfi-48)
                format)
          (srfi-69)
          (prefix (telebot) telebot:)
          (medea)
          (srfi-18))
  (begin
    (include-ci "scm-mceval.scm")

    ;; Logging
    (define (dispatch-level level-sym)
      (case level-sym
        ('info "INFO")
        ('warn "WARNING")
        ('err "ERROR")
        (else "WARNING")))

    (define send-log
      (case-lambda
        ((msg)
         (send-log 'info msg))
        ((level msg)
         (print (date->string (current-date))
                " - "
                (dispatch-level level)
                " - "
                (datum->string msg)))))

    ;; Parsing sexpressions
    (define (string->datum s)
      (call-with-input-string s
        (lambda (port)
          (read port))))

    (define (datum->string d)
      ; FIXME: loops in environment object
      (format "~a" d))

    ;; Evaluator
    (define (make-eval env)
      (define (dispatch msg)
        (case msg
          ('eval
           (lambda (exp)
             (handle-exceptions exn
                 (cond
                  ((eq? exn 'too-long-eval)
                   "EVAL: Too long evaluation")
                  ((eq? exn 'unknown-exp-type)
                   "EVAL: Unknown expression type")
                  ((eq? exn 'unknown-proc-type)
                   "EVAL: Unknown proc type")
                  ((eq? exn 'unbound-variable)
                   "EVAL: Unbound variable")
                  ((eq? exn 'else-is-not-last)
                   "EVAL: ELSE clause isn't last")
                  (else
                   (send-log 'err exn)
                   "EVAL: Some error occured"))
               (let ((res (eval-exp exp env 0)))
                 (if (compound-procedure? res)
                     "#<compound-procedure>"
                     res)))))
          ('erase-env!
           (set! env (setup-environment))
           "ENV erased")
          ('get-env
           env)))
      dispatch)

    (define (eval-safe eval-object exp)
      (format "~a"
              ((eval-object 'eval) exp)))

    (define (eval-erase-env! eval-object)
      (eval-object 'erase-env!))

    (define (eval-get-env eval-object)
      (format "~a" (caar (eval-object 'get-env))))

    ;; Bot
    (define help-command
      (string-append
       "Usage:\n"
       " /eval - evaluate an expression\n"
       " /help - show this\n"
       " /get_env - get an environment\n"
       " /erase_env - erase an environment"))

    (define (command? cmd text)
      (let ((lst (irregex-extract
                  `(: bos ,cmd)
                  text)))
        (not (null? lst))))

    (define (start-polling token)
      (define chat-envs (make-hash-table test: =)) ; int -> env
      (define (dispatch-command msg chat-id)
        (let ((text (telebot:resolve-query '(message text) msg))
              (from-user (telebot:resolve-query
                          '(message from first_name) msg))
              (group-title (telebot:resolve-query
                            '(message chat title) msg))
              (eval-object (hash-table-ref/default chat-envs chat-id #f)))
          (unless eval-object
            (set! eval-object (make-eval (setup-environment)))
            (hash-table-set! chat-envs chat-id eval-object))
          (send-log 'info `("Message from" ,from-user
                            ,@(if (null? group-title)
                                  (list ":")
                                  (list "in" group-title ":"))
                            ,text))
          (cond ((command? "/eval" text)
                 (telebot:sendMessage token
                                      chat_id: chat-id
                                      text:
                                      (eval-safe
                                       eval-object
                                       (condition-case
                                           (string->datum
                                            (car
                                             (irregex-extract
                                              '(: whitespace (+ ascii) eos)
                                              text)))
                                         ((exn type)
                                          "EVAL: type error")
                                         ((exn syntax)
                                          "EVAL: syntax error")
                                         ((exn)
                                          "EVAL: some error")))))
                ((command? "/help" text)
                 (telebot:sendMessage token
                                      chat_id: chat-id
                                      text: help-command))
                ((command? "/get_env" text)
                 (telebot:sendMessage token
                                      chat_id: chat-id
                                      text: (eval-get-env eval-object)))
                ((command? "/erase_env" text)
                 (telebot:sendMessage token
                                      chat_id: chat-id
                                      text: (begin
                                              (eval-erase-env! eval-object)
                                              (hash-table-set!
                                               chat-envs chat-id eval-object)
                                              "EVAL: ENV erased")))
                (else
                 (send-log 'warn (list
                                  "dispatch-command: Unknown command type"
                                  text))))))
      (define (msg-handler msg)
        (cond ((telebot:is-text? msg)
               (dispatch-command msg
                                 (telebot:resolve-query
                                  '(message chat id) msg)))
              (else
               (send-log 'warn (list
                                "msg-handler: Unknown message type"
                                msg)))))
      (let loop ()
        (condition-case
            (telebot:poll-updates token
                                  msg-handler)
          ((exn http client-error)
           (send-log 'err "HTTP client-error (poll-updates)")
           (sleep 60)
           (loop)))))
    ))
