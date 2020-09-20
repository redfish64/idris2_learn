#!/nix/store/awfabvmdbxrb0v2ppfvc7d0i9mqc1yif-chez-scheme-9.5.2/bin/scheme --script

; @generated
(import (chezscheme))
(case (machine-type)
  [(i3le ti3le a6le ta6le) (load-shared-object "libc.so.6")]
  [(i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib")]
  [(i3nt ti3nt a6nt ta6nt) (load-shared-object "msvcrt.dll")                           (load-shared-object "ws2_32.dll")]
  [else (load-shared-object "libc.so")])



(let ()
(define (blodwen-os)
  (case (machine-type)
    [(i3le ti3le a6le ta6le) "unix"]  ; GNU/Linux
    [(i3ob ti3ob a6ob ta6ob) "unix"]  ; OpenBSD
    [(i3fb ti3fb a6fb ta6fb) "unix"]  ; FreeBSD
    [(i3nb ti3nb a6nb ta6nb) "unix"]  ; NetBSD
    [(i3osx ti3osx a6osx ta6osx) "darwin"]
    [(i3nt ti3nt a6nt ta6nt) "windows"]
    [else "unknown"]))

(define blodwen-read-args (lambda (desc)
  (case (vector-ref desc 0)
    ((0) '())
    ((1) (cons (vector-ref desc 2)
               (blodwen-read-args (vector-ref desc 3)))))))
(define b+ (lambda (x y bits) (remainder (+ x y) (ash 1 bits))))
(define b- (lambda (x y bits) (remainder (- x y) (ash 1 bits))))
(define b* (lambda (x y bits) (remainder (* x y) (ash 1 bits))))
(define b/ (lambda (x y bits) (remainder (exact-floor (/ x y)) (ash 1 bits))))

(define integer->bits8 (lambda (x) (modulo x (expt 2 8))))
(define integer->bits16 (lambda (x) (modulo x (expt 2 16))))
(define integer->bits32 (lambda (x) (modulo x (expt 2 32))))
(define integer->bits64 (lambda (x) (modulo x (expt 2 64))))

(define bits16->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits32->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits8 (lambda (x) (modulo x (expt 2 8))))
(define bits64->bits16 (lambda (x) (modulo x (expt 2 16))))
(define bits64->bits32 (lambda (x) (modulo x (expt 2 32))))

(define blodwen-bits-shl (lambda (x y bits) (remainder (ash x y) (ash 1 bits))))
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
    (cond
      ((equal? x "") "")
      ((equal? (string-ref x 0) #\#) "")
      (else x))))
(define cast-string-int
  (lambda (x)
    (floor (cast-num (string->number (destroy-prefix x))))))
(define cast-int-char
  (lambda (x)
    (if (and (>= x 0)
             (<= x #x10ffff))
        (integer->char x)
        0)))
(define exact-floor
  (lambda (x)
    (inexact->exact (floor x))))
(define cast-string-double
  (lambda (x)
    (cast-num (string->number (destroy-prefix x)))))

(define (from-idris-list xs)
  (if (= (vector-ref xs 0) 0)
    '()
    (cons (vector-ref xs 1) (from-idris-list (vector-ref xs 2)))))
(define (string-concat xs) (apply string-append (from-idris-list xs)))
(define (string-pack xs) (apply string (from-idris-list xs)))
(define string-cons (lambda (x y) (string-append (string x) y)))
(define get-tag (lambda (x) (vector-ref x 0)))
(define string-reverse (lambda (x)
  (list->string (reverse (string->list x)))))
(define (string-substr off len s)
    (let* ((l (string-length s))
          (b (max 0 off))
          (x (max 0 len))
          (end (min l (+ b x))))
          (if (> b l)
              ""
              (substring s b end))))

(define either-left
  (lambda (x)
    (vector 0 x)))

(define either-right
  (lambda (x)
    (vector 1 x)))

(define blodwen-error-quit
  (lambda (msg)
    (display msg)
    (newline)
    (exit 1)))

(define (blodwen-get-line p)
    (if (port? p)
        (let ((str (get-line p)))
            (if (eof-object? str)
                ""
                str))
        void))

(define (blodwen-get-char p)
    (if (port? p)
        (let ((chr (get-char p)))
            (if (eof-object? chr)
                #\nul
                chr))
        void))

;; Buffers

(define (blodwen-new-buffer size)
  (make-bytevector size 0))

(define (blodwen-buffer-size buf)
  (bytevector-length buf))

(define (blodwen-buffer-setbyte buf loc val)
  (bytevector-u8-set! buf loc val))

(define (blodwen-buffer-getbyte buf loc)
  (bytevector-u8-ref buf loc))

(define (blodwen-buffer-setbits16 buf loc val)
  (bytevector-u16-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits16 buf loc)
  (bytevector-u16-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits32 buf loc val)
  (bytevector-u32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits32 buf loc)
  (bytevector-u32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setbits64 buf loc val)
  (bytevector-u64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getbits64 buf loc)
  (bytevector-u64-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint32 buf loc val)
  (bytevector-s32-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint32 buf loc)
  (bytevector-s32-ref buf loc (native-endianness)))

(define (blodwen-buffer-setint buf loc val)
  (bytevector-s64-set! buf loc val (native-endianness)))

(define (blodwen-buffer-getint buf loc)
  (bytevector-s64-ref buf loc (native-endianness)))

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

;; Threads

(define blodwen-thread-data (make-thread-parameter #f))

(define (blodwen-thread p)
    (fork-thread (lambda () (p (vector 0)))))

(define (blodwen-get-thread-data ty)
  (blodwen-thread-data))

(define (blodwen-set-thread-data a)
  (blodwen-thread-data a))

(define (blodwen-mutex) (make-mutex))
(define (blodwen-lock m) (mutex-acquire m))
(define (blodwen-unlock m) (mutex-release m))
(define (blodwen-thisthread) (get-thread-id))

(define (blodwen-condition) (make-condition))
(define (blodwen-condition-wait c m) (condition-wait c m))
(define (blodwen-condition-wait-timeout c m t)
  (let ((sec (div t 1000000))
        (micro (mod t 1000000)))
  (condition-wait c m (make-time 'time-duration (* 1000 micro) sec))))
(define (blodwen-condition-signal c) (condition-signal c))
(define (blodwen-condition-broadcast c) (condition-broadcast c))

(define (blodwen-sleep s) (sleep (make-time 'time-duration 0 s)))
(define (blodwen-usleep s)
  (let ((sec (div s 1000000))
        (micro (mod s 1000000)))
       (sleep (make-time 'time-duration (* 1000 micro) sec))))

(define (blodwen-time) (time-second (current-time)))
(define (blodwen-clock-time-utc) (current-time 'time-utc))
(define (blodwen-clock-time-monotonic) (current-time 'time-monotonic))
(define (blodwen-clock-time-duration) (current-time 'time-duration))
(define (blodwen-clock-time-process) (current-time 'time-process))
(define (blodwen-clock-time-thread) (current-time 'time-thread))
(define (blodwen-clock-time-gccpu) (current-time 'time-collector-cpu))
(define (blodwen-clock-time-gcreal) (current-time 'time-collector-real))
(define (blodwen-is-time? clk) (if (time? clk) 1 0))
(define (blodwen-clock-second time) (time-second time))
(define (blodwen-clock-nanosecond time) (time-nanosecond time))

(define (blodwen-args)
  (define (blodwen-build-args args)
    (if (null? args)
        (vector 0) ; Prelude.List
        (vector 1 (car args) (blodwen-build-args (cdr args)))))
    (blodwen-build-args (command-line)))

(define (blodwen-hasenv var)
  (if (eq? (getenv var) #f) 0 1))

(define (blodwen-system cmd)
  (system cmd))

;; Randoms
(define random-seed-register 0)
(define (initialize-random-seed-once)
  (if (= (virtual-register random-seed-register) 0)
      (let ([seed (time-nanosecond (current-time))])
        (set-virtual-register! random-seed-register seed)
        (random-seed seed))))

(define (blodwen-random-seed seed)
  (set-virtual-register! random-seed-register seed)
  (random-seed seed))
(define blodwen-random
  (case-lambda
    ;; no argument, pick a real value from [0, 1.0)
    [() (begin
          (initialize-random-seed-once)
          (random 1.0))]
    ;; single argument k, pick an integral value from [0, k)
    [(k)
      (begin
        (initialize-random-seed-once)
        (if (> k 0)
              (random k)
              (assertion-violationf 'blodwen-random "invalid range argument ~a" k)))]))

;; For finalisers

(define blodwen-finaliser (make-guardian))
(define (blodwen-register-object obj proc)
  (let [(x (cons obj proc))]
       (blodwen-finaliser x)
       x))
(define blodwen-run-finalisers
  (lambda ()
    (let run ()
      (let ([x (blodwen-finaliser)])
        (when x
          (((cdr x) (car x)) 'erased)
          (run))))))
(define IO-Prelude-prim__putStr (lambda (farg-0 farg-1) ((foreign-procedure #f "idris2_putStr" (string) void) farg-0) (vector 0 )))
(define prim__add_Int (lambda (arg-0 arg-1) (b+ arg-0 arg-1 63)))
(define prim__add_Integer (lambda (arg-0 arg-1) (+ arg-0 arg-1)))
(define prim__sub_Int (lambda (arg-0 arg-1) (b- arg-0 arg-1 63)))
(define prim__sub_Integer (lambda (arg-0 arg-1) (- arg-0 arg-1)))
(define prim__mul_Int (lambda (arg-0 arg-1) (b* arg-0 arg-1 63)))
(define prim__mul_Integer (lambda (arg-0 arg-1) (* arg-0 arg-1)))
(define prim__lt_Int (lambda (arg-0 arg-1) (or (and (< arg-0 arg-1) 1) 0)))
(define prim__lt_Integer (lambda (arg-0 arg-1) (or (and (< arg-0 arg-1) 1) 0)))
(define prim__lte_Integer (lambda (arg-0 arg-1) (or (and (<= arg-0 arg-1) 1) 0)))
(define prim__eq_Integer (lambda (arg-0 arg-1) (or (and (= arg-0 arg-1) 1) 0)))
(define prim__eq_Char (lambda (arg-0 arg-1) (or (and (char=? arg-0 arg-1) 1) 0)))
(define prim__gte_Int (lambda (arg-0 arg-1) (or (and (>= arg-0 arg-1) 1) 0)))
(define prim__gte_Integer (lambda (arg-0 arg-1) (or (and (>= arg-0 arg-1) 1) 0)))
(define prim__gt_Integer (lambda (arg-0 arg-1) (or (and (> arg-0 arg-1) 1) 0)))
(define prim__strHead (lambda (arg-0) (string-ref arg-0 0)))
(define prim__strAppend (lambda (arg-0 arg-1) (string-append arg-0 arg-1)))
(define prim__cast_IntString (lambda (arg-0) (number->string arg-0)))
(define prim__cast_IntegerInt (lambda (arg-0) arg-0))
(define Main-case--writeIndexes-4867 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 1))) (let ((e-3 (vector-ref sc0 2))) (Main-n--1824-4821-writeIndexes1 'erased (Num-Prelude-C-45_Neg__Int e-2 1) e-3))))))
(define Main-case--writeIndexesC-44writeIndexes1-4832 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (cond ((equal? sc0 0) arg-1) (else (Main-n--1824-4821-writeIndexes1 'erased (Num-Prelude-C-45_Neg__Int arg-2 1) (Array-Linear-Data-write_MArray__LinArray 'erased arg-1 arg-2 (b+ arg-2 1 63))))))))
(define Main-case--caseC-32blockC-32inC-32foldrC-44doit-4740 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (let ((sc0 arg-7)) (case (vector-ref sc0 0) ((0) (Main-n--1623-4680-doit 'erased 'erased arg-2 arg-3 arg-4 arg-6 (Num-Prelude-C-45_Neg__Int arg-5 1))) (else (let ((e-2 (vector-ref sc0 1))) (Main-n--1623-4680-doit 'erased 'erased arg-2 arg-3 arg-4 ((arg-4 e-2) arg-6) (Num-Prelude-C-45_Neg__Int arg-5 1))))))))
(define Main-case--foldrC-44doit-4691 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (let ((sc0 arg-7)) (cond ((equal? sc0 0) arg-6) (else (let ((mv (Array-Linear-Data-read_Array__IArray 'erased arg-2 arg-5))) (let ((sc1 mv)) (case (vector-ref sc1 0) ((0) (Main-n--1623-4680-doit 'erased 'erased arg-2 arg-3 arg-4 arg-6 (Num-Prelude-C-45_Neg__Int arg-5 1))) (else (let ((e-2 (vector-ref sc1 1))) (Main-n--1623-4680-doit 'erased 'erased arg-2 arg-3 arg-4 ((arg-4 e-2) arg-6) (Num-Prelude-C-45_Neg__Int arg-5 1))))))))))))
(define Main-n--1824-4821-writeIndexes1 (lambda (arg-0 arg-1 arg-2) (Main-case--writeIndexesC-44writeIndexes1-4832 'erased arg-2 arg-1 (EqOrd-Prelude-C-60_Ord__Int arg-1 0))))
(define Main-n--1888-4879-doit (lambda (arg-0) (let ((arr2 (Main-writeIndexes arg-0))) (Array-Linear-Data-toIArray 'erased 'erased arr2 (lambda (eta-0) (Main-foldr_Foldable__IArray 'erased 'erased (lambda (eta-1) (lambda (eta-2) (b+ eta-1 eta-2 63))) 0 eta-0))))))
(define Main-n--1623-4680-doit (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (Main-case--foldrC-44doit-4691 'erased 'erased arg-2 arg-3 arg-4 arg-6 arg-5 (EqOrd-Prelude-C-60_Ord__Int arg-6 0))))
(define Main-foldr_Foldable__IArray (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (let ((s (Array-Linear-Data-size_Array__IArray 'erased arg-4))) (Main-n--1623-4680-doit 'erased 'erased arg-4 arg-3 arg-2 arg-3 (Num-Prelude-C-45_Neg__Int s 1)))))
(define Main-writeIndexes (lambda (arg-0) (Main-case--writeIndexes-4867 'erased (Array-Linear-Data-msize_MArray__LinArray 'erased arg-0))))
(define Main-test (lambda () (Array-Linear-Data-newArray_MArray__LinArray 'erased 'erased 5 (lambda (eta-0) (Main-n--1888-4879-doit eta-0)))))
(define Main-runtest (lambda () (IO-Prelude-putStrLn 'erased (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-0) (let ((act-24 (arg-334 eta-0))) ((arg-335 act-24) eta-0))))))) (lambda (a) (lambda (arg-337) (lambda (eta-0) (let ((act-51 (arg-337 eta-0))) (act-51 eta-0)))))) (lambda (a) (lambda (arg-6366) arg-6366))) (Show-Prelude-show_Show__Int (Main-test)))))
(define Array-Linear-Data-write_MArray__LinArray (lambda (arg-0 arg-1 arg-2 arg-3) (PrimIO-unsafePerformIO 'erased (lambda (eta-0) (let ((act-24 ((IOArray-Data-writeArray 'erased 'erased (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-1) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-1)))))) (lambda (a) (lambda (arg-172) (lambda (eta-1) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-1) (let ((act-17 (arg-173 eta-1))) (let ((act-16 (arg-175 eta-1))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-1) (let ((act-24 (arg-334 eta-1))) ((arg-335 act-24) eta-1))))))) (lambda (a) (lambda (arg-337) (lambda (eta-1) (let ((act-51 (arg-337 eta-1))) (act-51 eta-1)))))) (lambda (a) (lambda (arg-6366) arg-6366))) arg-1 arg-2 arg-3) eta-0))) arg-1)))))
(define Array-Linear-Data-size_Array__IArray (lambda (arg-0 arg-1) (IOArray-Data-max 'erased arg-1)))
(define Array-Linear-Data-read_Array__IArray (lambda (arg-0 arg-1 arg-2) (PrimIO-unsafePerformIO 'erased (IOArray-Data-readArray 'erased 'erased (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-0) (let ((act-24 (arg-334 eta-0))) ((arg-335 act-24) eta-0))))))) (lambda (a) (lambda (arg-337) (lambda (eta-0) (let ((act-51 (arg-337 eta-0))) (act-51 eta-0)))))) (lambda (a) (lambda (arg-6366) arg-6366))) arg-1 arg-2))))
(define Array-Linear-Data-newArray_MArray__LinArray (lambda (arg-0 arg-1 arg-2 arg-3) (arg-3 (PrimIO-unsafePerformIO 'erased (IOArray-Data-newArray 'erased 'erased (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-0) (let ((act-24 (arg-334 eta-0))) ((arg-335 act-24) eta-0))))))) (lambda (a) (lambda (arg-337) (lambda (eta-0) (let ((act-51 (arg-337 eta-0))) (act-51 eta-0)))))) (lambda (a) (lambda (arg-6366) arg-6366))) arg-2)))))
(define Array-Linear-Data-msize_MArray__LinArray (lambda (arg-0 arg-1) (vector 0 (IOArray-Data-max 'erased arg-1) arg-1)))
(define Array-Linear-Data-toIArray (lambda (arg-0 arg-1 arg-2 arg-3) (arg-3 arg-2)))
(define IOArray-Data-case--readArray-3653 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (let ((sc0 arg-5)) (cond ((equal? sc0 0) (let ((sc1 (let ((sc2 (let ((sc3 arg-2)) (let ((e-1 (vector-ref sc3 1))) e-1)))) (let ((e-1 (vector-ref sc2 1))) e-1)))) (let ((e-2 (vector-ref sc1 2))) ((e-2 'erased) (vector 0 ))))) (else (let ((sc1 arg-2)) (let ((e-2 (vector-ref sc1 2))) ((e-2 'erased) (lambda (eta-0) (vector-ref (let ((sc2 arg-4)) (let ((e-3 (vector-ref sc2 2))) e-3)) arg-3))))))))))
(define IOArray-Data-case--writeArray-3580 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6) (let ((sc0 arg-6)) (cond ((equal? sc0 0) (let ((sc1 (let ((sc2 (let ((sc3 arg-2)) (let ((e-1 (vector-ref sc3 1))) e-1)))) (let ((e-1 (vector-ref sc2 1))) e-1)))) (let ((e-2 (vector-ref sc1 2))) ((e-2 'erased) (vector 0 ))))) (else (let ((sc1 arg-2)) (let ((e-2 (vector-ref sc1 2))) ((e-2 'erased) (lambda (eta-0) (vector-set! (let ((sc2 arg-5)) (let ((e-3 (vector-ref sc2 2))) e-3)) arg-4 (vector 1 arg-3)))))))))))
(define IOArray-Data-writeArray (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (IOArray-Data-case--writeArray-3580 'erased 'erased arg-2 arg-5 arg-4 arg-3 (Basics-Prelude-C-124C-124 (EqOrd-Prelude-C-60_Ord__Int arg-4 0) (lambda () (EqOrd-Prelude-C-62C-61_Ord__Int arg-4 (IOArray-Data-max 'erased arg-3)))))))
(define IOArray-Data-readArray (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (IOArray-Data-case--readArray-3653 'erased 'erased arg-2 arg-4 arg-3 (Basics-Prelude-C-124C-124 (EqOrd-Prelude-C-60_Ord__Int arg-4 0) (lambda () (EqOrd-Prelude-C-62C-61_Ord__Int arg-4 (IOArray-Data-max 'erased arg-3)))))))
(define IOArray-Data-newArray (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 (let ((sc1 arg-2)) (let ((e-1 (vector-ref sc1 1))) e-1)))) (let ((e-2 (vector-ref sc0 2))) ((((e-2 'erased) 'erased) (let ((sc1 arg-2)) (let ((e-4 (vector-ref sc1 2))) ((e-4 'erased) (lambda (eta-0) (make-vector arg-3 (vector 0 ))))))) (lambda (bind-0) (let ((sc1 (let ((sc2 (let ((sc3 arg-2)) (let ((e-5 (vector-ref sc3 1))) e-5)))) (let ((e-6 (vector-ref sc2 1))) e-6)))) (let ((e-5 (vector-ref sc1 2))) ((e-5 'erased) (vector 0 arg-3 bind-0))))))))))
(define IOArray-IOArray-Data-maxSize (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define IOArray-Data-max (lambda (arg-0 ext-0) (let ((sc0 ext-0)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define IOArray-IOArray-Data-content (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) e-2))))
(define Basics-Prelude-C-124C-124 (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) 0) (else (arg-1))))))
(define Basics-Prelude-not (lambda (arg-0) (let ((sc0 arg-0)) (cond ((equal? sc0 0) 1) (else 0)))))
(define Basics-Prelude-intToBool (lambda (arg-0) (let ((sc0 arg-0)) (cond ((equal? sc0 0) 1)(else 0)))))
(define Basics-Prelude-id (lambda (arg-0 arg-1) arg-1))
(define Basics-Prelude-C-38C-38 (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (arg-1)) (else 1)))))
(define Builtin-assert_total (lambda (arg-0 arg-1) arg-1))
(define Types-Prelude-case--max-613 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define Types-Prelude-case--min-599 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define Types-Prelude-min_Ord__Nat (lambda (arg-0 arg-1) (Types-Prelude-case--min-599 arg-1 arg-0 (Types-Prelude-C-60_Ord__Nat arg-0 arg-1))))
(define Types-Prelude-max_Ord__Nat (lambda (arg-0 arg-1) (Types-Prelude-case--max-613 arg-1 arg-0 (Types-Prelude-C-62_Ord__Nat arg-0 arg-1))))
(define Types-Prelude-compare_Ord__Nat (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (let ((sc1 arg-1)) (cond ((equal? sc1 0) 1)(else 0))))(else (let ((e-0 (- arg-0 1))) (let ((sc0 arg-1)) (cond ((equal? sc0 0) 2)(else (let ((e-2 (- arg-1 1))) (Types-Prelude-compare_Ord__Nat e-0 e-2)))))))))))
(define Types-Prelude-__Impl_Ord_Nat (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (Types-Prelude-C-61C-61_Eq__Nat arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (Types-Prelude-C-47C-61_Eq__Nat arg-4 arg-5)))) (lambda (arg-369) (lambda (arg-370) (Types-Prelude-compare_Ord__Nat arg-369 arg-370))) (lambda (arg-371) (lambda (arg-372) (Types-Prelude-C-60_Ord__Nat arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (Types-Prelude-C-62_Ord__Nat arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (Types-Prelude-C-60C-61_Ord__Nat arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (Types-Prelude-C-62C-61_Ord__Nat arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (Types-Prelude-max_Ord__Nat arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (Types-Prelude-min_Ord__Nat arg-381 arg-382))))))
(define Types-Prelude-__Impl_Eq_Nat (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (Types-Prelude-C-61C-61_Eq__Nat arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (Types-Prelude-C-47C-61_Eq__Nat arg-4 arg-5))))))
(define Types-Prelude-C-62_Ord__Nat (lambda (arg-0 arg-1) (EqOrd-Prelude-C-61C-61_Eq__Ordering (Types-Prelude-compare_Ord__Nat arg-0 arg-1) 2)))
(define Types-Prelude-C-62C-61_Ord__Nat (lambda (arg-0 arg-1) (EqOrd-Prelude-C-47C-61_Eq__Ordering (Types-Prelude-compare_Ord__Nat arg-0 arg-1) 0)))
(define Types-Prelude-C-61C-61_Eq__Nat (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (let ((sc1 arg-1)) (cond ((equal? sc1 0) 0)(else 1))))(else (let ((e-0 (- arg-0 1))) (let ((sc0 arg-1)) (cond ((equal? sc0 0) 1)(else (let ((e-1 (- arg-1 1))) (Types-Prelude-C-61C-61_Eq__Nat e-0 e-1)))))))))))
(define Types-Prelude-C-60_Ord__Nat (lambda (arg-0 arg-1) (EqOrd-Prelude-C-61C-61_Eq__Ordering (Types-Prelude-compare_Ord__Nat arg-0 arg-1) 0)))
(define Types-Prelude-C-60C-61_Ord__Nat (lambda (arg-0 arg-1) (EqOrd-Prelude-C-47C-61_Eq__Ordering (Types-Prelude-compare_Ord__Nat arg-0 arg-1) 2)))
(define Types-Prelude-C-47C-61_Eq__Nat (lambda (arg-0 arg-1) (Basics-Prelude-not (Types-Prelude-C-61C-61_Eq__Nat arg-0 arg-1))))
(define Strings-Types-Prelude-C-43C-43 (lambda (arg-0 arg-1) (string-append arg-0 arg-1)))
(define Num-Prelude-negate_Neg__Int (lambda (arg-0) (b- 0 arg-0 63)))
(define Num-Prelude-fromInteger_Num__Int (lambda (ext-0) ext-0))
(define Num-Prelude-__Impl_Num_Int (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (b+ arg-2 arg-3 63))) (lambda (arg-4) (lambda (arg-5) (b* arg-4 arg-5 63))) (lambda (arg-6) arg-6))))
(define Num-Prelude-__Impl_Neg_Int (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (b+ arg-2 arg-3 63))) (lambda (arg-4) (lambda (arg-5) (b* arg-4 arg-5 63))) (lambda (arg-6) arg-6)) (lambda (arg-56) (Num-Prelude-negate_Neg__Int arg-56)) (lambda (arg-57) (lambda (arg-58) (Num-Prelude-C-45_Neg__Int arg-57 arg-58))))))
(define Num-Prelude-C-45_Neg__Int (lambda (ext-0 ext-1) (b- ext-0 ext-1 63)))
(define Num-Prelude-C-43_Num__Int (lambda (ext-0 ext-1) (b+ ext-0 ext-1 63)))
(define Num-Prelude-C-42_Num__Int (lambda (ext-0 ext-1) (b* ext-0 ext-1 63)))
(define Num-Prelude-C-45 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-3 (vector-ref sc0 3))) (lambda (arg-2) (lambda (arg-3) ((e-3 arg-2) arg-3)))))))
(define EqOrd-Prelude-case--caseC-32blockC-32inC-32compare-1279 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 1) (else 2)))))
(define EqOrd-Prelude-case--compare-1262 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) 0) (else (EqOrd-Prelude-case--caseC-32blockC-32inC-32compare-1279 arg-0 arg-1 (EqOrd-Prelude-C-61C-61_Eq__Integer arg-1 arg-0)))))))
(define EqOrd-Prelude-case--max-1245 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define EqOrd-Prelude-case--min-1231 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define EqOrd-Prelude-min_Ord__Integer (lambda (arg-0 arg-1) (EqOrd-Prelude-case--min-1231 arg-1 arg-0 (EqOrd-Prelude-C-60_Ord__Integer arg-0 arg-1))))
(define EqOrd-Prelude-max_Ord__Integer (lambda (arg-0 arg-1) (EqOrd-Prelude-case--max-1245 arg-1 arg-0 (EqOrd-Prelude-C-62_Ord__Integer arg-0 arg-1))))
(define EqOrd-Prelude-compare_Ord__Integer (lambda (arg-0 arg-1) (EqOrd-Prelude-case--compare-1262 arg-1 arg-0 (EqOrd-Prelude-C-60_Ord__Integer arg-0 arg-1))))
(define EqOrd-Prelude-__Impl_Ord_Integer (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (EqOrd-Prelude-C-61C-61_Eq__Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (EqOrd-Prelude-C-47C-61_Eq__Integer arg-4 arg-5)))) (lambda (arg-369) (lambda (arg-370) (EqOrd-Prelude-compare_Ord__Integer arg-369 arg-370))) (lambda (arg-371) (lambda (arg-372) (EqOrd-Prelude-C-60_Ord__Integer arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (EqOrd-Prelude-C-62_Ord__Integer arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (EqOrd-Prelude-C-60C-61_Ord__Integer arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (EqOrd-Prelude-C-62C-61_Ord__Integer arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (EqOrd-Prelude-max_Ord__Integer arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (EqOrd-Prelude-min_Ord__Integer arg-381 arg-382))))))
(define EqOrd-Prelude-__Impl_Eq_Ordering (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (EqOrd-Prelude-C-61C-61_Eq__Ordering arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (EqOrd-Prelude-C-47C-61_Eq__Ordering arg-4 arg-5))))))
(define EqOrd-Prelude-__Impl_Eq_Integer (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (EqOrd-Prelude-C-61C-61_Eq__Integer arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (EqOrd-Prelude-C-47C-61_Eq__Integer arg-4 arg-5))))))
(define EqOrd-Prelude-C-62_Ord__Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (> arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-62C-61_Ord__Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (>= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-62C-61_Ord__Int (lambda (arg-0 arg-1) (let ((sc0 (or (and (>= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-61C-61_Eq__Ordering (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 0) (let ((sc1 arg-1)) (cond ((equal? sc1 0) 0)(else 1)))) ((equal? sc0 1) (let ((sc1 arg-1)) (cond ((equal? sc1 1) 0)(else 1)))) ((equal? sc0 2) (let ((sc1 arg-1)) (cond ((equal? sc1 2) 0)(else 1))))(else 1)))))
(define EqOrd-Prelude-C-61C-61_Eq__Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-61C-61_Eq__Char (lambda (arg-0 arg-1) (let ((sc0 (or (and (char=? arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-60_Ord__Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (< arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-60_Ord__Int (lambda (arg-0 arg-1) (let ((sc0 (or (and (< arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-60C-61_Ord__Integer (lambda (arg-0 arg-1) (let ((sc0 (or (and (<= arg-0 arg-1) 1) 0))) (cond ((equal? sc0 0) 1)(else 0)))))
(define EqOrd-Prelude-C-47C-61_Eq__Ordering (lambda (arg-0 arg-1) (Basics-Prelude-not (EqOrd-Prelude-C-61C-61_Eq__Ordering arg-0 arg-1))))
(define EqOrd-Prelude-C-47C-61_Eq__Integer (lambda (arg-0 arg-1) (Basics-Prelude-not (EqOrd-Prelude-C-61C-61_Eq__Integer arg-0 arg-1))))
(define EqOrd-Prelude-compare (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-2) (lambda (arg-3) ((e-2 arg-2) arg-3)))))))
(define EqOrd-Prelude-C-62 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-4 (vector-ref sc0 4))) (lambda (arg-2) (lambda (arg-3) ((e-4 arg-2) arg-3)))))))
(define EqOrd-Prelude-C-61C-61 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) (lambda (arg-2) (lambda (arg-3) ((e-1 arg-2) arg-3)))))))
(define EqOrd-Prelude-C-60 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-3 (vector-ref sc0 3))) (lambda (arg-2) (lambda (arg-3) ((e-3 arg-2) arg-3)))))))
(define Interfaces-Prelude-__Monad_C-40ApplicativeC-32mC-41 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define Interfaces-Prelude-pure (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-3) ((e-2 'erased) arg-3))))))
(define Interfaces-Prelude-C-62C-62C-61 (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-3)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-4) (lambda (arg-5) ((((e-2 'erased) 'erased) arg-4) arg-5)))))))
(define PrimIO-case--unsafePerformIO-405 (lambda (arg-0 arg-1 arg-2 arg-3) (PrimIO-unsafeDestroyWorld 'erased 'erased arg-3)))
(define PrimIO-case--caseC-32blockC-32inC-32io_bind-326 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5 arg-6 arg-7) (arg-7 arg-6)))
(define PrimIO-case--io_bind-304 (lambda (arg-0 arg-1 arg-2 arg-3 arg-4 arg-5) (PrimIO-case--caseC-32blockC-32inC-32io_bind-326 'erased 'erased 'erased 'erased 'erased arg-5 'erased (arg-3 arg-5))))
(define PrimIO-unsafePerformIO (lambda (arg-0 arg-1) (PrimIO-unsafeCreateWorld 'erased (lambda (w) (PrimIO-case--unsafePerformIO-405 'erased 'erased 'erased (arg-1 w))))))
(define PrimIO-unsafeDestroyWorld (lambda (arg-0 arg-1 arg-2) arg-2))
(define PrimIO-unsafeCreateWorld (lambda (arg-0 arg-1) (arg-1 #f)))
(define PrimIO-io_pure (lambda (arg-0 arg-1 ext-0) arg-1))
(define PrimIO-io_bind (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (PrimIO-case--io_bind-304 'erased 'erased 'erased arg-3 'erased (arg-2 ext-0))))
(define PrimIO-fromPrim (lambda (arg-0 arg-1) arg-1))
(define Show-Prelude-case--max-5250 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define Show-Prelude-case--min-5236 (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (cond ((equal? sc0 0) arg-1) (else arg-0)))))
(define Show-Prelude-show_Show__Int (lambda (arg-0) (Show-Prelude-showPrec_Show__Int (vector 0 ) arg-0)))
(define Show-Prelude-showPrec_Show__Int (lambda (ext-0 ext-1) (Show-Prelude-primNumShow 'erased (lambda (eta-0) (number->string eta-0)) ext-0 ext-1)))
(define Show-Prelude-min_Ord__Prec (lambda (arg-0 arg-1) (Show-Prelude-case--min-5236 arg-1 arg-0 (Show-Prelude-C-60_Ord__Prec arg-0 arg-1))))
(define Show-Prelude-max_Ord__Prec (lambda (arg-0 arg-1) (Show-Prelude-case--max-5250 arg-1 arg-0 (Show-Prelude-C-62_Ord__Prec arg-0 arg-1))))
(define Show-Prelude-compare_Ord__Prec (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (case (vector-ref sc0 0) ((4) (let ((e-0 (vector-ref sc0 1))) (let ((sc1 arg-1)) (case (vector-ref sc1 0) ((4) (let ((e-1 (vector-ref sc1 1))) (Types-Prelude-compare_Ord__Nat e-0 e-1)))(else (EqOrd-Prelude-compare_Ord__Integer (Show-Prelude-precCon arg-0) (Show-Prelude-precCon arg-1)))))))(else (EqOrd-Prelude-compare_Ord__Integer (Show-Prelude-precCon arg-0) (Show-Prelude-precCon arg-1)))))))
(define Show-Prelude-__Impl_Show_Int (lambda () (vector 0 (lambda (x) (Show-Prelude-show_Show__Int x)) (lambda (d) (lambda (x) (Show-Prelude-showPrec_Show__Int d x))))))
(define Show-Prelude-__Impl_Ord_Prec (lambda () (vector 0 (vector 0 (lambda (arg-2) (lambda (arg-3) (Show-Prelude-C-61C-61_Eq__Prec arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (Show-Prelude-C-47C-61_Eq__Prec arg-4 arg-5)))) (lambda (arg-369) (lambda (arg-370) (Show-Prelude-compare_Ord__Prec arg-369 arg-370))) (lambda (arg-371) (lambda (arg-372) (Show-Prelude-C-60_Ord__Prec arg-371 arg-372))) (lambda (arg-373) (lambda (arg-374) (Show-Prelude-C-62_Ord__Prec arg-373 arg-374))) (lambda (arg-375) (lambda (arg-376) (Show-Prelude-C-60C-61_Ord__Prec arg-375 arg-376))) (lambda (arg-377) (lambda (arg-378) (Show-Prelude-C-62C-61_Ord__Prec arg-377 arg-378))) (lambda (arg-379) (lambda (arg-380) (Show-Prelude-max_Ord__Prec arg-379 arg-380))) (lambda (arg-381) (lambda (arg-382) (Show-Prelude-min_Ord__Prec arg-381 arg-382))))))
(define Show-Prelude-__Impl_Eq_Prec (lambda () (vector 0 (lambda (arg-2) (lambda (arg-3) (Show-Prelude-C-61C-61_Eq__Prec arg-2 arg-3))) (lambda (arg-4) (lambda (arg-5) (Show-Prelude-C-47C-61_Eq__Prec arg-4 arg-5))))))
(define Show-Prelude-C-62_Ord__Prec (lambda (arg-0 arg-1) (EqOrd-Prelude-C-61C-61_Eq__Ordering (Show-Prelude-compare_Ord__Prec arg-0 arg-1) 2)))
(define Show-Prelude-C-62C-61_Ord__Prec (lambda (arg-0 arg-1) (EqOrd-Prelude-C-47C-61_Eq__Ordering (Show-Prelude-compare_Ord__Prec arg-0 arg-1) 0)))
(define Show-Prelude-C-61C-61_Eq__Prec (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (case (vector-ref sc0 0) ((4) (let ((e-0 (vector-ref sc0 1))) (let ((sc1 arg-1)) (case (vector-ref sc1 0) ((4) (let ((e-1 (vector-ref sc1 1))) (Types-Prelude-C-61C-61_Eq__Nat e-0 e-1)))(else (EqOrd-Prelude-C-61C-61_Eq__Integer (Show-Prelude-precCon arg-0) (Show-Prelude-precCon arg-1)))))))(else (EqOrd-Prelude-C-61C-61_Eq__Integer (Show-Prelude-precCon arg-0) (Show-Prelude-precCon arg-1)))))))
(define Show-Prelude-C-60_Ord__Prec (lambda (arg-0 arg-1) (EqOrd-Prelude-C-61C-61_Eq__Ordering (Show-Prelude-compare_Ord__Prec arg-0 arg-1) 0)))
(define Show-Prelude-C-60C-61_Ord__Prec (lambda (arg-0 arg-1) (EqOrd-Prelude-C-47C-61_Eq__Ordering (Show-Prelude-compare_Ord__Prec arg-0 arg-1) 2)))
(define Show-Prelude-C-47C-61_Eq__Prec (lambda (arg-0 arg-1) (Basics-Prelude-not (Show-Prelude-C-61C-61_Eq__Prec arg-0 arg-1))))
(define Show-Prelude-showPrec (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-2) (lambda (arg-3) ((e-2 arg-2) arg-3)))))))
(define Show-Prelude-showParens (lambda (arg-0 arg-1) (let ((sc0 arg-0)) (cond ((equal? sc0 1) arg-1) (else (Strings-Types-Prelude-C-43C-43 "(" (Strings-Types-Prelude-C-43C-43 arg-1 ")")))))))
(define Show-Prelude-primNumShow (lambda (arg-0 arg-1 arg-2 arg-3) (let ((str (arg-1 arg-3))) (Show-Prelude-showParens (Basics-Prelude-C-38C-38 (Show-Prelude-C-62C-61_Ord__Prec arg-2 (vector 5 )) (lambda () (Show-Prelude-firstCharIs (lambda (arg-4) (EqOrd-Prelude-C-61C-61_Eq__Char arg-4 #\-)) str))) str))))
(define Show-Prelude-precCon (lambda (arg-0) (let ((sc0 arg-0)) (case (vector-ref sc0 0) ((0) 0) ((1) 1) ((2) 2) ((3) 3) ((4) 4) ((5) 5) (else 6)))))
(define Show-Prelude-firstCharIs (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (cond ((equal? sc0 "") 1)(else (arg-0 (string-ref arg-1 0)))))))
(define IO-Prelude-pure_Applicative__IO (lambda (arg-0 arg-1 ext-0) arg-1))
(define IO-Prelude-map_Functor__IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-3 (arg-3 ext-0))) (arg-2 act-3))))
(define IO-Prelude-liftIO_HasIO__IO (lambda (arg-0 arg-1) arg-1))
(define IO-Prelude-join_Monad__IO (lambda (arg-0 arg-1 ext-0) (let ((act-2 (arg-1 ext-0))) (act-2 ext-0))))
(define IO-Prelude-__Impl_Monad_IO (lambda () (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-0) (let ((act-24 (arg-334 eta-0))) ((arg-335 act-24) eta-0))))))) (lambda (a) (lambda (arg-337) (lambda (eta-0) (let ((act-29 (arg-337 eta-0))) (act-29 eta-0))))))))
(define IO-Prelude-__Impl_HasIO_IO (lambda () (vector 0 (vector 0 (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16))))))))) (lambda (b) (lambda (a) (lambda (arg-334) (lambda (arg-335) (lambda (eta-0) (let ((act-24 (arg-334 eta-0))) ((arg-335 act-24) eta-0))))))) (lambda (a) (lambda (arg-337) (lambda (eta-0) (let ((act-51 (arg-337 eta-0))) (act-51 eta-0)))))) (lambda (a) (lambda (arg-6366) arg-6366)))))
(define IO-Prelude-__Impl_Functor_IO (lambda (ext-4 ext-1 ext-2 ext-3 ext-0) (IO-Prelude-map_Functor__IO 'erased 'erased ext-2 ext-3 ext-0)))
(define IO-Prelude-__Impl_Applicative_IO (lambda () (vector 0 (lambda (b) (lambda (a) (lambda (func) (lambda (arg-62) (lambda (eta-0) (IO-Prelude-map_Functor__IO 'erased 'erased func arg-62 eta-0)))))) (lambda (a) (lambda (arg-172) (lambda (eta-0) arg-172))) (lambda (b) (lambda (a) (lambda (arg-173) (lambda (arg-175) (lambda (eta-0) (let ((act-17 (arg-173 eta-0))) (let ((act-16 (arg-175 eta-0))) (act-17 act-16)))))))))))
(define IO-Prelude-__HasIO_C-40MonadC-32ioC-41 (lambda (arg-0 arg-1) (let ((sc0 arg-1)) (let ((e-1 (vector-ref sc0 1))) e-1))))
(define IO-Prelude-C-62C-62C-61_Monad__IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-1 (arg-2 ext-0))) ((arg-3 act-1) ext-0))))
(define IO-Prelude-C-60C-42C-62_Applicative__IO (lambda (arg-0 arg-1 arg-2 arg-3 ext-0) (let ((act-6 (arg-2 ext-0))) (let ((act-5 (arg-3 ext-0))) (act-6 act-5)))))
(define IO-Prelude-putStrLn (lambda (arg-0 arg-1 arg-2) (IO-Prelude-putStr 'erased arg-1 (string-append arg-2 "\xa;"))))
(define IO-Prelude-putStr (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-1)) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) (lambda (eta-0) (IO-Prelude-prim__putStr arg-2 eta-0)))))))
(define IO-Prelude-primIO (lambda (arg-0 arg-1 arg-2 arg-3) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) ((e-2 'erased) arg-3)))))
(define IO-Prelude-liftIO (lambda (arg-0 arg-1 arg-2) (let ((sc0 arg-2)) (let ((e-2 (vector-ref sc0 2))) (lambda (arg-3) ((e-2 'erased) arg-3))))))
(define Prims-IOArray-Data-prim__newArray (lambda (arg-0 arg-1 arg-2 arg-3) (make-vector arg-1 arg-2)))
(define Prims-IOArray-Data-prim__arraySet (lambda (arg-0 arg-1 arg-2 arg-3 arg-4) (vector-set! arg-1 arg-2 arg-3)))
(define Prims-IOArray-Data-prim__arrayGet (lambda (arg-0 arg-1 arg-2 arg-3) (vector-ref arg-1 arg-2)))
(load-shared-object "libidris2_support.so")
(collect-request-handler (lambda () (collect) (blodwen-run-finalisers)))
(PrimIO-unsafePerformIO 'erased (Main-runtest))(collect 4)
(blodwen-run-finalisers))
