(define-library (srfi 111)
  (import (meevax box))
  (export box box? (rename box-ref unbox) (rename box-set! set-box!)))
