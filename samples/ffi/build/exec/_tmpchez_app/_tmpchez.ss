#!/nix/store/30km4dqjdq5clcqsk1ldqkgsld4njfb5-chez-scheme-9.5.2/bin/scheme --script

(import (chezscheme))
(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [(i3nt ti3nt a6nt ta6nt) (load-shared-object "msvcrt.dll")]
  [else (load-shared-object "libc.so")])



(let ()
(define blodwen-read-args (lambda (desc)
  (case (vector-ref desc 0)
    ((0) '())
    ((1) (cons (vector-ref desc 2)
               (blodwen-read-args (vector-ref desc 3)))))))
(define b+ (lambda (x y bits) (remainder (+ x y) (expt 2 bits))))
(define b- (lambda (x y bits) (remainder (- x y) (expt 2 bits))))
(define b* (lambda (x y bits) (remainder (* x y) (expt 2 bits))))
(define b/ (lambda (x y bits) (remainder (floor (/ x y)) (expt 2 bits))))

(define blodwen-shl (lambda (x y) (ash x y)))
(define blodwen-shr (lambda (x y) (ash x (- y))))
(define blodwen-and (lambda (x y) (logand x y)))
(define blodwen-or (lambda (x y) (logor x y)))
(define blodwen-xor (lambda (x y) (logxor x y)))

(define cast-num
  (lambda (x)
    (if (number? x) x 0)))
(define destroy-prefix
  (lambda (x)
    (if (eqv? (string-ref x 0) #\#) "" x)))
(define cast-string-int
  (lambda (x)
    (floor (cast-num (string->number (destroy-prefix x))))))
(define cast-string-double
  (lambda (x)
    (cast-num (string->number (destroy-prefix x)))))
(define string-cons (lambda (x y) (string-append (string x) y)))
(define get-tag (lambda (x) (vector-ref x 0)))
(define string-reverse (lambda (x)
  (list->string (reverse (string->list x)))))
(define (string-substr off len s)
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
          (substring s b end)))

(define either-left
  (lambda (x)
    (vector 0 #f #f x)))

(define either-right
  (lambda (x)
    (vector 1 #f #f x)))

(define blodwen-error-quit
  (lambda (msg)
    (display msg)
    (newline)
    (exit 1)))

;; Buffers

(define (blodwen-new-buffer size)
  (make-bytevector size 0))

(define (blodwen-buffer-size buf)
  (bytevector-length buf))

(define (blodwen-buffer-setbyte buf loc val)
  (bytevector-u8-set! buf loc val))

(define (blodwen-buffer-getbyte buf loc)
  (bytevector-u8-ref buf loc))

(define (blodwen-buffer-setint buf loc val)
  (bytevector-s32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint buf loc)
  (bytevector-s32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setdouble buf loc val)
  (bytevector-ieee-double-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getdouble buf loc)
  (bytevector-ieee-double-ref buf loc (native-endianness)))

(define (blodwen-stringbytelen str)
  (bytevector-length (string->utf8 str)))

(define (blodwen-buffer-setstring buf loc val)
  (let* [(strvec (string->utf8 val))
         (len (bytevector-length strvec))]
    (bytevector-copy! strvec 0 buf loc len)))

(define (blodwen-buffer-getstring buf loc len)
  (let [(newvec (make-bytevector len))]
    (bytevector-copy! buf loc newvec 0 len)
    (utf8->string newvec)))

(define (blodwen-buffer-copydata buf start len dest loc)
  (bytevector-copy! buf start dest loc len))

(define (blodwen-readbuffer-bytes h buf loc max)
  (guard (x (#t -1))
    (get-bytevector-n! h buf loc max)))

(define (blodwen-readbuffer h)
  (guard (x (#t (bytevector)))
    (get-bytevector-all h)))

(define (blodwen-writebuffer h buf loc max)
  (guard (x (#t -1))
     (put-bytevector h buf loc max)
     max))

;; Files: Much of the following adapted from idris-chez, thanks to Niklas
;; Larsson

;; All the file operations are implemented as primitives which return
;; Either Int x, where the Int is an error code:
(define (blodwen-error-code x)
    (cond
        ((i/o-read-error? x) 1)
        ((i/o-write-error? x) 2)
        ((i/o-file-does-not-exist-error? x) 3)
        ((i/o-file-protection-error? x) 4)
        (else 255)))

;; If the file operation raises an error, catch it and return an appropriate
;; error code
(define (blodwen-file-op op)
  (guard
    (x ((i/o-error? x) (either-left (blodwen-error-code x))))
    (either-right (op))))

(define (blodwen-get-n n p)
    (if (port? p) (get-string-n p n) ""))

(define (blodwen-putstring p s)
    (if (port? p) (put-string p s) void)
    0)

(define (blodwen-open file mode bin)
    (define tc (if (= bin 1) #f (make-transcoder (utf-8-codec))))
    (define bm (buffer-mode line))
    (case mode
        (("r") (open-file-input-port file (file-options) bm tc))
        (("w") (open-file-output-port file (file-options no-fail) bm tc))
        (("wx") (open-file-output-port file (file-options) bm tc))
        (("a") (open-file-output-port file (file-options no-fail no-truncate) bm tc))
        (("r+") (open-file-input/output-port file (file-options no-create) bm tc))
        (("w+") (open-file-input/output-port file (file-options no-fail) bm tc))
        (("w+x") (open-file-input/output-port file (file-options) bm tc))
        (("a+") (open-file-input/output-port file (file-options no-fail no-truncate) bm tc))
        (else (raise (make-i/o-error)))))

(define (blodwen-close-port p)
    (when (port? p) (close-port p)))

(define (blodwen-get-line p)
    (if (and (port? p) (not (port-eof? p)))
        (let ((str (get-line p)))
            (string-append str "\n"))
        ""))

(define (blodwen-get-char p)
    (if (and (port? p) (not (port-eof? p)))
        (get-char p)
        #\nul))

(define (blodwen-file-size p)
    (port-length p))

(define (blodwen-eof p)
    (if (port-eof? p)
        1
        0))

;; Directories

(define (blodwen-current-directory)
  (current-directory))

(define (blodwen-change-directory dir)
  (if (file-directory? dir)
      (begin (current-directory dir) 1)
      0))

(define (blodwen-create-directory dir)
  (blodwen-file-op (lambda () (mkdir dir) 0)))

; Scheme only gives a primitive for reading all the files in a directory,
; so this is faking the C interface!
(define (blodwen-open-directory dir)
  (blodwen-file-op (lambda () (box (directory-list dir)))))

(define (blodwen-close-directory dir) '()) ; no-op, it's not really open

(define (blodwen-next-dir-entry dir)
  (let [(dlist (unbox dir))]
    (if (null? dlist)
      (either-left 255)
      (begin (set-box! dir (cdr dlist))
             (either-right (car dlist))))))

;; Threads

(define blodwen-thread-data (make-thread-parameter #f))

(define (blodwen-thread p)
    (fork-thread (lambda () (p (vector 0)))))

(define (blodwen-get-thread-data)
  (blodwen-thread-data))

(define (blodwen-set-thread-data a)
  (blodwen-thread-data a))

(define (blodwen-mutex) (make-mutex))
(define (blodwen-lock m) (mutex-acquire m))
(define (blodwen-unlock m) (mutex-release m))
(define (blodwen-thisthread) (get-thread-id))

(define (blodwen-condition) (make-condition))
(define (blodwen-condition-wait c m) (condition-wait c m))
(define (blodwen-condition-wait-timeout c m t) (condition-wait c m t))
(define (blodwen-condition-signal c) (condition-signal c))
(define (blodwen-condition-broadcast c) (condition-broadcast c))

(define (blodwen-sleep s) (sleep (make-time 'time-duration 0 s)))
(define (blodwen-usleep s)
  (let ((sec (div s 1000000))
        (micro (mod s 1000000)))
       (sleep (make-time 'time-duration (* 1000 micro) sec))))

(define (blodwen-time) (time-second (current-time)))

(define (blodwen-args)
  (define (blodwen-build-args args)
    (if (null? args)
        (vector 0 '())
        (vector 1 '() (car args) (blodwen-build-args (cdr args)))))
    (blodwen-build-args (command-line)))
(define Main-prim_applyFnIO (lambda (farg-0 farg-1 farg-2 farg-3) (vector 0 #f ((foreign-procedure #f "applyFn" (string int void*) string) farg-0 farg-1 (let ([c-code (foreign-callable #f (lambda (cb0 cb1) (vector-ref (((farg-2 cb0) cb1) #f)2)) (string int) string)]) (lock-object c-code) (foreign-callable-entry-point c-code))) #f)))
(define prim__add_Int (lambda (v-0 v-1) (b+ v-0 v-1 63)))
(define prim__add_Integer (lambda (v-0 v-1) (+ v-0 v-1)))
(define prim__sub_Integer (lambda (v-0 v-1) (- v-0 v-1)))
(define prim__mul_Int (lambda (v-0 v-1) (b* v-0 v-1 63)))
(define prim__mul_Integer (lambda (v-0 v-1) (* v-0 v-1)))
(define prim__lt_Integer (lambda (v-0 v-1) (or (and (< v-0 v-1) 1) 0)))
(define prim__lte_Integer (lambda (v-0 v-1) (or (and (<= v-0 v-1) 1) 0)))
(define prim__eq_Int (lambda (v-0 v-1) (or (and (= v-0 v-1) 1) 0)))
(define prim__eq_Integer (lambda (v-0 v-1) (or (and (= v-0 v-1) 1) 0)))
(define prim__eq_Char (lambda (v-0 v-1) (or (and (char=? v-0 v-1) 1) 0)))
(define prim__gte_Integer (lambda (v-0 v-1) (or (and (>= v-0 v-1) 1) 0)))
(define prim__gt_Integer (lambda (v-0 v-1) (or (and (> v-0 v-1) 1) 0)))
(define prim__strHead (lambda (v-0) (string-ref v-0 0)))
(define prim__strAppend (lambda (v-0 v-1) (string-append v-0 v-1)))
(define prim__cast_IntString (lambda (v-0) (number->string v-0)))
(define prim__cast_IntegerInt (lambda (v-0) v-0))
(define Main-case--1169-240 (lambda (v-0 v-1 v-2 v-3) (let ((sc0 v-3)) (case (get-tag sc0) ((0) v-1) ((1) (Strings-Prelude-C-43C-43 v-1 "s"))))))
(define Main-pluralise (lambda (v-0 v-1) (Prelude-C-62C-62C-61_Monad__IO 4294 4294 (PrimIO-putStrLn "Pluralising") (lambda (v-2) (Prelude-pure_Applicative__IO 4294 (Strings-Prelude-C-43C-43 (Prelude-show_Show__Int v-1) (Strings-Prelude-C-43C-43 " " (Main-case--1169-240 v-1 v-0 v-2 (Prelude-C-61C-61_Eq__Int v-1 ((Prelude-fromInteger_Num__Int) 1))))))))))
(define Main-main (lambda () (Prelude-C-62C-62C-61_Monad__IO 4294 4294 (Main-applyFnIO "Biscuit" ((Prelude-fromInteger_Num__Int) 10) (lambda (v-0) (lambda (v-1) (Main-pluralise v-0 v-1)))) (lambda (v-0) (Prelude-C-62C-62C-61_Monad__IO 4294 4294 (PrimIO-putStrLn v-0) (lambda (v-1) (Prelude-C-62C-62C-61_Monad__IO 4294 4294 (Main-applyFnIO "Tree" ((Prelude-fromInteger_Num__Int) 1) (lambda (v-2) (lambda (v-3) (Main-pluralise v-2 v-3)))) (lambda (v-2) (PrimIO-putStrLn v-2)))))))))
(define Main-applyFnIO (lambda (v-0 v-1 v-2) (vector 0 4294 (lambda (v-3) (Main-prim_applyFnIO v-0 v-1 (lambda (v-4) (lambda (v-5) (let ((sc0 ((v-2 v-4) v-5))) (case (get-tag sc0) ((0) (let ((v-6 (vector-ref sc0 1))) (let ((v-7 (vector-ref sc0 2))) v-7))))))) v-3)))))
(define Prelude-case--5377-6084 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-case--5378-6070 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-case--3251-3703 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-case--3252-3689 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-case--3176-3576 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) 0) ((1) (+ 1 (- v-0 ((Prelude-fromInteger_Num__Integer) 1))))))))
(define Prelude-case--1217-1436 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) (vector 1 )) ((1) (vector 2 ))))))
(define Prelude-case--1152-1419 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) (vector 0 )) ((1) (Prelude-case--1217-1436 v-0 v-1 (Prelude-C-61C-61_Eq__Integer v-1 v-0)))))))
(define Prelude-case--1157-1402 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-case--1158-1388 (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) v-1) ((1) v-0)))))
(define Prelude-show_Show__Int (lambda (v-0) (((Prelude-showPrec_Show__Int) (vector 0 )) v-0)))
(define Prelude-showPrec_Show__Int (lambda () (lambda (v-0) (lambda (v-1) (Prelude-primNumShow 4294 (lambda (v-2) (number->string v-2)) v-0 v-1)))))
(define Prelude-pure_Applicative__IO (lambda (v-0 v-1) (PrimIO-io_pure 4294 v-1)))
(define Prelude-min_Ord__Prec (lambda (v-0 v-1) (Prelude-case--5378-6070 v-1 v-0 (Prelude-C-60_Ord__Prec v-0 v-1))))
(define Prelude-min_Ord__Nat (lambda (v-0 v-1) (Prelude-case--3252-3689 v-1 v-0 (Prelude-C-60_Ord__Nat v-0 v-1))))
(define Prelude-min_Ord__Integer (lambda (v-0 v-1) (Prelude-case--1158-1388 v-1 v-0 (Prelude-C-60_Ord__Integer v-0 v-1))))
(define Prelude-max_Ord__Prec (lambda (v-0 v-1) (Prelude-case--5377-6084 v-1 v-0 (Prelude-C-62_Ord__Prec v-0 v-1))))
(define Prelude-max_Ord__Nat (lambda (v-0 v-1) (Prelude-case--3251-3703 v-1 v-0 (Prelude-C-62_Ord__Nat v-0 v-1))))
(define Prelude-max_Ord__Integer (lambda (v-0 v-1) (Prelude-case--1157-1402 v-1 v-0 (Prelude-C-62_Ord__Integer v-0 v-1))))
(define Prelude-map_Functor__IO (lambda (v-0 v-1 v-2 v-3) ((PrimIO-io_bind 4294 4294 v-3) (lambda (v-4) (PrimIO-io_pure 4294 (v-2 v-4))))))
(define Prelude-join_Monad__IO (lambda (v-0 v-1) (Prelude-C-62C-62C-61_Monad__IO 4294 4294 v-1 (lambda (v-2) v-2))))
(define Prelude-fromInteger_Num__Integer (lambda () (lambda (v-0) v-0)))
(define Prelude-fromInteger_Num__Int (lambda () (lambda (v-0) v-0)))
(define Prelude-compare_Ord__Prec (lambda (v-0 v-1) (let ((sc0 v-0)) (case (get-tag sc0) ((4) (let ((v-2 (vector-ref sc0 1))) (let ((sc1 v-1)) (case (get-tag sc1) ((4) (let ((v-3 (vector-ref sc1 1))) (Prelude-compare_Ord__Nat v-2 v-3)))(else (Prelude-compare_Ord__Integer (Prelude-precCon v-0) (Prelude-precCon v-1)))))))(else (Prelude-compare_Ord__Integer (Prelude-precCon v-0) (Prelude-precCon v-1)))))))
(define Prelude-compare_Ord__Nat (lambda (v-0 v-1) (let ((sc0 v-0)) (cond ((equal? sc0 0) (let ((sc1 v-1)) (cond ((equal? sc1 0) (vector 1 ))(else (let ((v-2 (- v-1 1))) (vector 0 ))))))(else (let ((v-2 (- v-0 1))) (let ((sc0 v-1)) (cond ((equal? sc0 0) (vector 2 ))(else (let ((v-3 (- v-1 1))) (Prelude-compare_Ord__Nat v-2 v-3)))))))))))
(define Prelude-compare_Ord__Integer (lambda (v-0 v-1) (Prelude-case--1152-1419 v-1 v-0 (Prelude-C-60_Ord__Integer v-0 v-1))))
(define Prelude-__Impl_Show_Int (lambda () (vector 0 4294 (lambda (v-0) (Prelude-show_Show__Int v-0)) (lambda (v-0) (lambda (v-1) (((Prelude-showPrec_Show__Int) v-0) v-1))))))
(define Prelude-__Impl_Ord_Prec (lambda () (vector 0 4294 (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Prec v-0 v-1)))) (lambda (v-0) (lambda (v-1) (Prelude-compare_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60C-61_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62C-61_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-max_Ord__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-min_Ord__Prec v-0 v-1))))))
(define Prelude-__Impl_Ord_Nat (lambda () (vector 0 4294 (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Nat v-0 v-1)))) (lambda (v-0) (lambda (v-1) (Prelude-compare_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60C-61_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62C-61_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-max_Ord__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-min_Ord__Nat v-0 v-1))))))
(define Prelude-__Impl_Ord_Integer (lambda () (vector 0 4294 (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Integer v-0 v-1)))) (lambda (v-0) (lambda (v-1) (Prelude-compare_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-60C-61_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-62C-61_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-max_Ord__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-min_Ord__Integer v-0 v-1))))))
(define Prelude-__Impl_Num_Integer (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (((Prelude-C-43_Num__Integer) v-0) v-1))) (lambda (v-0) (lambda (v-1) (((Prelude-C-42_Num__Integer) v-0) v-1))) (lambda (v-0) ((Prelude-fromInteger_Num__Integer) v-0)))))
(define Prelude-__Impl_Num_Int (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (((Prelude-C-43_Num__Int) v-0) v-1))) (lambda (v-0) (lambda (v-1) (((Prelude-C-42_Num__Int) v-0) v-1))) (lambda (v-0) ((Prelude-fromInteger_Num__Int) v-0)))))
(define Prelude-__Impl_Monad_IO (lambda () (vector 0 4294 (vector 0 4294 (vector 0 4294 (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-map_Functor__IO 4294 4294 v-2 v-3)))))) (lambda (v-0) (lambda (v-1) (Prelude-pure_Applicative__IO 4294 v-1))) (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-C-60C-42C-62_Applicative__IO 4294 4294 v-2 v-3)))))) (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-C-62C-62C-61_Monad__IO 4294 4294 v-2 v-3))))) (lambda (v-0) (lambda (v-1) (Prelude-join_Monad__IO 4294 v-1))))))
(define Prelude-__Impl_Functor_IO (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-map_Functor__IO 4294 4294 v-2 v-3))))))))
(define Prelude-__Impl_Eq_Prec (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Prec v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Prec v-0 v-1))))))
(define Prelude-__Impl_Eq_Ordering (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Ordering v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Ordering v-0 v-1))))))
(define Prelude-__Impl_Eq_Nat (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Nat v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Nat v-0 v-1))))))
(define Prelude-__Impl_Eq_Integer (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Integer v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Integer v-0 v-1))))))
(define Prelude-__Impl_Eq_Int (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Int v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Int v-0 v-1))))))
(define Prelude-__Impl_Eq_Char (lambda () (vector 0 4294 (lambda (v-0) (lambda (v-1) (Prelude-C-61C-61_Eq__Char v-0 v-1))) (lambda (v-0) (lambda (v-1) (Prelude-C-47C-61_Eq__Char v-0 v-1))))))
(define Prelude-__Impl_Applicative_IO (lambda () (vector 0 4294 (vector 0 4294 (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-map_Functor__IO 4294 4294 v-2 v-3)))))) (lambda (v-0) (lambda (v-1) (Prelude-pure_Applicative__IO 4294 v-1))) (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (Prelude-C-60C-42C-62_Applicative__IO 4294 4294 v-2 v-3))))))))
(define Prelude-C-62_Ord__Prec (lambda (v-0 v-1) (Prelude-C-61C-61_Eq__Ordering (Prelude-compare_Ord__Prec v-0 v-1) (vector 2 ))))
(define Prelude-C-62_Ord__Nat (lambda (v-0 v-1) (Prelude-C-61C-61_Eq__Ordering (Prelude-compare_Ord__Nat v-0 v-1) (vector 2 ))))
(define Prelude-C-62_Ord__Integer (lambda (v-0 v-1) (let ((sc0 (or (and (> v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-62C-62C-61_Monad__IO (lambda (v-0 v-1 v-2 v-3) ((PrimIO-io_bind 4294 4294 v-2) v-3)))
(define Prelude-C-62C-61_Ord__Prec (lambda (v-0 v-1) (Prelude-C-47C-61_Eq__Ordering (Prelude-compare_Ord__Prec v-0 v-1) (vector 0 ))))
(define Prelude-C-62C-61_Ord__Nat (lambda (v-0 v-1) (Prelude-C-47C-61_Eq__Ordering (Prelude-compare_Ord__Nat v-0 v-1) (vector 0 ))))
(define Prelude-C-62C-61_Ord__Integer (lambda (v-0 v-1) (let ((sc0 (or (and (>= v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-61C-61_Eq__Prec (lambda (v-0 v-1) (let ((sc0 v-0)) (case (get-tag sc0) ((4) (let ((v-2 (vector-ref sc0 1))) (let ((sc1 v-1)) (case (get-tag sc1) ((4) (let ((v-3 (vector-ref sc1 1))) (Prelude-C-61C-61_Eq__Nat v-2 v-3)))(else (Prelude-C-61C-61_Eq__Integer (Prelude-precCon v-0) (Prelude-precCon v-1)))))))(else (Prelude-C-61C-61_Eq__Integer (Prelude-precCon v-0) (Prelude-precCon v-1)))))))
(define Prelude-C-61C-61_Eq__Ordering (lambda (v-0 v-1) (let ((sc0 v-0)) (case (get-tag sc0) ((0) (let ((sc1 v-1)) (case (get-tag sc1) ((0) (vector 0 ))(else (vector 1 ))))) ((1) (let ((sc1 v-1)) (case (get-tag sc1) ((1) (vector 0 ))(else (vector 1 ))))) ((2) (let ((sc1 v-1)) (case (get-tag sc1) ((2) (vector 0 ))(else (vector 1 )))))(else (vector 1 ))))))
(define Prelude-C-61C-61_Eq__Nat (lambda (v-0 v-1) (let ((sc0 v-0)) (cond ((equal? sc0 0) (let ((sc1 v-1)) (cond ((equal? sc1 0) (vector 0 ))(else (vector 1 )))))(else (let ((v-2 (- v-0 1))) (let ((sc0 v-1)) (cond ((equal? sc0 0) (vector 1 ))(else (let ((v-3 (- v-1 1))) (Prelude-C-61C-61_Eq__Nat v-2 v-3)))))))))))
(define Prelude-C-61C-61_Eq__Integer (lambda (v-0 v-1) (let ((sc0 (or (and (= v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-61C-61_Eq__Int (lambda (v-0 v-1) (let ((sc0 (or (and (= v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-61C-61_Eq__Char (lambda (v-0 v-1) (let ((sc0 (or (and (char=? v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-60_Ord__Prec (lambda (v-0 v-1) (Prelude-C-61C-61_Eq__Ordering (Prelude-compare_Ord__Prec v-0 v-1) (vector 0 ))))
(define Prelude-C-60_Ord__Nat (lambda (v-0 v-1) (Prelude-C-61C-61_Eq__Ordering (Prelude-compare_Ord__Nat v-0 v-1) (vector 0 ))))
(define Prelude-C-60_Ord__Integer (lambda (v-0 v-1) (let ((sc0 (or (and (< v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-60C-61_Ord__Prec (lambda (v-0 v-1) (Prelude-C-47C-61_Eq__Ordering (Prelude-compare_Ord__Prec v-0 v-1) (vector 2 ))))
(define Prelude-C-60C-61_Ord__Nat (lambda (v-0 v-1) (Prelude-C-47C-61_Eq__Ordering (Prelude-compare_Ord__Nat v-0 v-1) (vector 2 ))))
(define Prelude-C-60C-61_Ord__Integer (lambda (v-0 v-1) (let ((sc0 (or (and (<= v-0 v-1) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-C-60C-42C-62_Applicative__IO (lambda (v-0 v-1 v-2 v-3) ((PrimIO-io_bind 4294 4294 v-2) (lambda (v-4) ((PrimIO-io_bind 4294 4294 v-3) (lambda (v-5) (PrimIO-io_pure 4294 (v-4 v-5))))))))
(define Prelude-C-47C-61_Eq__Prec (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Prec v-0 v-1))))
(define Prelude-C-47C-61_Eq__Ordering (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Ordering v-0 v-1))))
(define Prelude-C-47C-61_Eq__Nat (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Nat v-0 v-1))))
(define Prelude-C-47C-61_Eq__Integer (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Integer v-0 v-1))))
(define Prelude-C-47C-61_Eq__Int (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Int v-0 v-1))))
(define Prelude-C-47C-61_Eq__Char (lambda (v-0 v-1) (Prelude-not (Prelude-C-61C-61_Eq__Char v-0 v-1))))
(define Prelude-C-43_Num__Integer (lambda () (lambda (v-0) (lambda (v-1) (+ v-0 v-1)))))
(define Prelude-C-43_Num__Int (lambda () (lambda (v-0) (lambda (v-1) (b+ v-0 v-1 63)))))
(define Prelude-C-42_Num__Integer (lambda () (lambda (v-0) (lambda (v-1) (* v-0 v-1)))))
(define Prelude-C-42_Num__Int (lambda () (lambda (v-0) (lambda (v-1) (b* v-0 v-1 63)))))
(define Prelude-a-6869 (lambda () (lambda (v-0) (lambda (v-1) (vector 104 v-0)))))
(define Prelude-a-6867 (lambda () (lambda (v-0) (lambda (v-1) (vector 104 v-0)))))
(define Prelude-_-6857 (lambda () (lambda (v-0) (vector 104 ((Prelude-m-6835) v-0)))))
(define Prelude-_-6840 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (vector 1 v-1 (lambda (v-3) ((Prelude-m-6835) v-0))))))))
(define Prelude-_-6839 (lambda () (lambda (v-0) (lambda (v-1) ((Prelude-m-6835) v-1)))))
(define Prelude-m-6835 (lambda () (lambda (v-0) (vector 104 v-0))))
(define Prelude-_-6821 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (vector 1 v-1 (lambda (v-4) v-0))))))))
(define Prelude-a-6820 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (vector 1 v-1 (lambda (v-4) v-0))))))))
(define Prelude-_-6791 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (vector 104 v-1))))))
(define Prelude-_-6790 (lambda () (lambda (v-0) (lambda (v-1) (vector 104 (vector 1 v-1 (lambda (v-2) v-0)))))))
(define Prelude-f-6781 (lambda () (lambda (v-0) (vector 104 v-0))))
(define Prelude-_-6749 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) ((Prelude-f-6745) v-1))))))
(define Prelude-_-6748 (lambda () (lambda (v-0) (lambda (v-1) (vector 1 v-1 (lambda (v-2) v-0))))))
(define Prelude-f-6745 (lambda () (lambda (v-0) (vector 104 v-0))))
(define Prelude-_-2887 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (lambda (v-4) (lambda (v-5) (lambda (v-6) (vector 1 v-1 (lambda (v-7) (v-2 v-0))))))))))))
(define Prelude-_-2886 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (lambda (v-3) (lambda (v-4) (lambda (v-5) (v-2 v-1)))))))))
(define Prelude-showPrec (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (lambda (v-5) (lambda (v-6) ((v-4 v-5) v-6)))))))))))
(define Prelude-showParens (lambda (v-0 v-1) (let ((sc0 v-0)) (case (get-tag sc0) ((1) v-1) ((0) (Strings-Prelude-C-43C-43 "(" (Strings-Prelude-C-43C-43 v-1 ")")))))))
(define Prelude-show (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (lambda (v-5) (v-3 v-5))))))))))
(define Prelude-pure (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) (let ((v-3 (vector-ref sc0 1))) (let ((v-4 (vector-ref sc0 2))) (let ((v-5 (vector-ref sc0 3))) (let ((v-6 (vector-ref sc0 4))) (lambda (v-7) ((v-5 4294) v-7)))))))))))
(define Prelude-primNumShow (lambda (v-0 v-1 v-2 v-3) (let ((v-4 (v-1 v-3))) (Prelude-showParens (Prelude-C-38C-38 (Prelude-C-62C-61_Ord__Prec v-2 (vector 5 )) (lambda () (Prelude-firstCharIs (lambda (v-5) (Prelude-C-61C-61_Eq__Char v-5 #\-)) v-4))) v-4))))
(define Prelude-precCon (lambda (v-0) (let ((sc0 v-0)) (case (get-tag sc0) ((0) ((Prelude-fromInteger_Num__Integer) 0)) ((1) ((Prelude-fromInteger_Num__Integer) 1)) ((2) ((Prelude-fromInteger_Num__Integer) 2)) ((3) ((Prelude-fromInteger_Num__Integer) 3)) ((4) (let ((v-1 (vector-ref sc0 1))) ((Prelude-fromInteger_Num__Integer) 4))) ((5) ((Prelude-fromInteger_Num__Integer) 5)) ((6) ((Prelude-fromInteger_Num__Integer) 6))))))
(define Prelude-not (lambda (v-0) (let ((sc0 v-0)) (case (get-tag sc0) ((0) (vector 1 )) ((1) (vector 0 ))))))
(define Prelude-natToInteger (lambda (v-0) (let ((sc0 v-0)) (cond ((equal? sc0 0) ((Prelude-fromInteger_Num__Integer) 0))(else (let ((v-1 (- v-0 1))) (((Prelude-C-43_Num__Integer) ((Prelude-fromInteger_Num__Integer) 1)) v-1)))))))
(define Prelude-integerToNat (lambda (v-0) (Prelude-case--3176-3576 v-0 (let ((sc0 (or (and (<= v-0 ((Prelude-fromInteger_Num__Integer) 0)) 1) 0))) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 )))))))
(define Prelude-intToBool (lambda (v-0) (let ((sc0 v-0)) (cond ((equal? sc0 0) (vector 1 ))(else (vector 0 ))))))
(define Prelude-id (lambda (v-0 v-1) v-1))
(define Prelude-fromInteger (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (lambda (v-6) (v-5 v-6)))))))))))
(define Prelude-firstCharIs (lambda (v-0 v-1) (let ((sc0 v-1)) (cond ((equal? sc0 "") (vector 1 ))(else (v-0 (string-ref v-1 0)))))))
(define Prelude-compare (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (let ((v-6 (vector-ref sc0 5))) (let ((v-7 (vector-ref sc0 6))) (let ((v-8 (vector-ref sc0 7))) (let ((v-9 (vector-ref sc0 8))) (let ((v-10 (vector-ref sc0 9))) (lambda (v-11) (lambda (v-12) ((v-4 v-11) v-12)))))))))))))))))
(define Prelude-C-62C-62C-61 (lambda (v-0 v-1 v-2 v-3) (let ((sc0 v-3)) (case (get-tag sc0) ((0) (let ((v-4 (vector-ref sc0 1))) (let ((v-5 (vector-ref sc0 2))) (let ((v-6 (vector-ref sc0 3))) (let ((v-7 (vector-ref sc0 4))) (lambda (v-8) (lambda (v-9) ((((v-6 4294) 4294) v-8) v-9))))))))))))
(define Prelude-C-62C-61 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (let ((v-6 (vector-ref sc0 5))) (let ((v-7 (vector-ref sc0 6))) (let ((v-8 (vector-ref sc0 7))) (let ((v-9 (vector-ref sc0 8))) (let ((v-10 (vector-ref sc0 9))) (lambda (v-11) (lambda (v-12) ((v-8 v-11) v-12)))))))))))))))))
(define Prelude-C-62 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (let ((v-6 (vector-ref sc0 5))) (let ((v-7 (vector-ref sc0 6))) (let ((v-8 (vector-ref sc0 7))) (let ((v-9 (vector-ref sc0 8))) (let ((v-10 (vector-ref sc0 9))) (lambda (v-11) (lambda (v-12) ((v-6 v-11) v-12)))))))))))))))))
(define Prelude-C-61C-61 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (lambda (v-5) (lambda (v-6) ((v-3 v-5) v-6)))))))))))
(define Prelude-C-60 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (let ((v-6 (vector-ref sc0 5))) (let ((v-7 (vector-ref sc0 6))) (let ((v-8 (vector-ref sc0 7))) (let ((v-9 (vector-ref sc0 8))) (let ((v-10 (vector-ref sc0 9))) (lambda (v-11) (lambda (v-12) ((v-5 v-11) v-12)))))))))))))))))
(define Prelude-C-47C-61 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (lambda (v-5) (lambda (v-6) ((v-4 v-5) v-6)))))))))))
(define Strings-Prelude-C-43C-43 (lambda (v-0 v-1) (string-append v-0 v-1)))
(define Prelude-C-43 (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (let ((v-4 (vector-ref sc0 3))) (let ((v-5 (vector-ref sc0 4))) (lambda (v-6) (lambda (v-7) ((v-3 v-6) v-7))))))))))))
(define Prelude-C-38C-38 (lambda (v-0 v-1) (let ((sc0 v-0)) (case (get-tag sc0) ((0) (v-1)) ((1) (vector 1 ))))))
(define PrimIO-case--307-333 (lambda (v-0 v-1 v-2 v-3) (let ((sc0 v-3)) (case (get-tag sc0) ((0) (let ((v-4 (vector-ref sc0 1))) (let ((v-5 (vector-ref sc0 2))) (let ((v-6 (vector-ref sc0 3))) (PrimIO-unsafeDestroyWorld 4294 v-6 v-5)))))))))
(define PrimIO-case--187-250 (lambda (v-0 v-1 v-2 v-3 v-4 v-5 v-6 v-7) (let ((sc0 v-7)) (case (get-tag sc0) ((0) (let ((v-8 (vector-ref sc0 1))) (let ((v-9 (vector-ref sc0 2))) (v-9 v-6))))))))
(define PrimIO-case--172-231 (lambda (v-0 v-1 v-2 v-3 v-4 v-5) (let ((sc0 v-5)) (case (get-tag sc0) ((0) (let ((v-6 (vector-ref sc0 1))) (let ((v-7 (vector-ref sc0 2))) (let ((v-8 (vector-ref sc0 3))) (PrimIO-case--187-250 4294 4294 4294 4294 4294 v-7 v-8 (v-3 v-7))))))))))
(define PrimIO-_-222 (lambda () (lambda (v-0) (lambda (v-1) (lambda (v-2) (vector 1 v-1 (lambda (v-3) (vector 104 v-0))))))))
(define PrimIO-unsafePerformIO (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) (PrimIO-unsafeCreateWorld 4294 (lambda (v-4) (PrimIO-case--307-333 4294 v-3 4294 (v-3 v-4)))))))))))
(define PrimIO-unsafeDestroyWorld (lambda (v-0 v-1 v-2) (let ((sc0 v-1)) (cond ((equal? sc0 #f) v-2)))))
(define PrimIO-unsafeCreateWorld (lambda (v-0 v-1) (v-1 #f)))
(define PrimIO-toPrim (lambda (v-0 v-1) (let ((sc0 v-1)) (case (get-tag sc0) ((0) (let ((v-2 (vector-ref sc0 1))) (let ((v-3 (vector-ref sc0 2))) v-3)))))))
(define PrimIO-putStrLn (lambda (v-0) (PrimIO-putStr (string-append v-0 "\xa;"))))
(define PrimIO-putStr (lambda (v-0) (vector 0 4294 (lambda (v-1) (display v-0) (vector 0 #f (vector 0 ) #f)))))
(define PrimIO-prim__putStr (lambda (v-0 v-1) (display v-0) (vector 0 #f (vector 0 ) #f)))
(define PrimIO-primIO (lambda (v-0 v-1) (vector 0 4294 v-1)))
(define PrimIO-io_pure (lambda (v-0 v-1) (vector 0 4294 (lambda (v-2) (vector 0 4294 v-1 v-2)))))
(define PrimIO-io_bind (lambda (v-0 v-1 v-2) (let ((sc0 v-2)) (case (get-tag sc0) ((0) (let ((v-3 (vector-ref sc0 1))) (let ((v-4 (vector-ref sc0 2))) (lambda (v-5) (vector 0 4294 (lambda (v-6) (PrimIO-case--172-231 4294 4294 4294 v-5 4294 (v-4 v-6))))))))))))
(define Builtin-assert_total (lambda (v-0 v-1) v-1))
(load-shared-object "libsmall.so")
(PrimIO-unsafePerformIO 4294 (Main-main)))