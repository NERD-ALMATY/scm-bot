(import (r7rs))
(import scheme
        (chicken base)
        (chicken process-context)
        (scm-bot))

(define token
  (let ((v (get-environment-variable "TELEGA_BOT")))
    (if v
        v
        (error 'token "Set env variable TELEGA_BOT"))))

(start-polling token)
