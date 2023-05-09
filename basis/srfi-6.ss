(define-library (srfi 6)
  (import (meevax port))
  (export get-output-string
          (rename open-string open-input-string)
          (rename open-string open-output-string)))
