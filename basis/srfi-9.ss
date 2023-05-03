#|
   Copyright (C) Richard Kelsey (1999). All Rights Reserved.

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE.
|#

(define-library (srfi 9)
  (import (scheme r5rs))

  (export define-record-type)

  (begin (define record-marker (list 'record-marker))

         (define (record? x)
           (and (vector? x)
                (< 0 (vector-length x))
                (eq? (vector-ref x 0)
                     record-marker)))

         (define (make-record size)
           (let ((new (make-vector (+ size 1))))
             (vector-set! new 0 record-marker)
             new))

         (define (record-ref record index)
           (vector-ref record (+ index 1)))

         (define (record-set! record index value)
           (vector-set! record (+ index 1) value))

         (define :record-type (make-record 3))

         (record-set! :record-type 0 :record-type)

         (record-set! :record-type 1 ':record-type)

         (record-set! :record-type 2 '(name field-tags))

         (define (record-type record)
           (record-ref record 0))

         (define (make-record-type name field-tags)
           (let ((new (make-record 3)))
             (record-set! new 0 :record-type)
             (record-set! new 1 name)
             (record-set! new 2 field-tags)
             new))

         (define (record-type-name record-type)
           (record-ref record-type 1))

         (define (record-type-field-tags record-type)
           (record-ref record-type 2))

         (define (field-index type tag)
           (let loop ((i 1) (tags (record-type-field-tags type)))
             (cond ((null? tags)
                    (error "record type has no such field" type tag))
                   ((eq? tag (car tags))
                    i)
                   (else (loop (+ i 1) (cdr tags))))))

         (define (record-constructor type tags)
           (let ((size (length (record-type-field-tags type)))
                 (arg-count (length tags))
                 (indexes (map (lambda (tag)
                                 (field-index type tag))
                               tags)))
             (lambda args
               (if (= (length args)
                      arg-count)
                   (let ((new (make-record (+ size 1))))
                     (record-set! new 0 type)
                     (for-each (lambda (arg i)
                                 (record-set! new i arg))
                               args
                               indexes)
                     new)
                   (error "wrong number of arguments to constructor" type args)))))

         (define (record-predicate type)
           (lambda (thing)
             (and (record? thing)
                  (eq? (record-type thing)
                       type))))

         (define (record-accessor type tag)
           (let ((index (field-index type tag)))
             (lambda (thing)
               (if (and (record? thing)
                        (eq? (record-type thing)
                             type))
                   (record-ref thing index)
                   (error "accessor applied to bad value" type tag thing)))))

         (define (record-modifier type tag)
           (let ((index (field-index type tag)))
             (lambda (thing value)
               (if (and (record? thing)
                        (eq? (record-type thing)
                             type))
                   (record-set! thing index value)
                   (error "modifier applied to bad value" type tag thing)))))

         (define-syntax define-record-field
           (syntax-rules ()
             ((define-record-field type field-tag accessor)
              (define accessor (record-accessor type 'field-tag)))
             ((define-record-field type field-tag accessor modifier)
              (begin (define accessor (record-accessor type 'field-tag))
                     (define modifier (record-modifier type 'field-tag))))))

         (define-syntax define-record-type
           (syntax-rules ()
             ((define-record-type type
                                  (constructor constructor-tag ...)
                                  predicate
                                  (field-tag accessor . more) ...)
              (begin (define type
                       (make-record-type 'type '(field-tag ...)))
                     (define constructor
                       (record-constructor type '(constructor-tag ...)))
                     (define predicate
                       (record-predicate type))
                     (define-record-field type field-tag accessor . more)
                     ...))))))
