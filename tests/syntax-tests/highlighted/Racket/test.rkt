[38;2;248;248;242m#lang racket[0m

[38;2;248;248;242m([0m[38;2;249;38;114mrequire[0m[38;2;248;248;242m [0m[38;2;248;248;242m"main.rkt" rackunit)[0m

[38;2;117;113;94m;; Helper for test cases with multiple outputs[0m
[38;2;117;113;94m;; See: https://stackoverflow.com/questions/41081395/unit-testing-in-racket-with-multiple-outputs[0m
[38;2;248;248;242m(define-syntax check-values-equal?[0m
[38;2;248;248;242m  (syntax-rules ()[0m
[38;2;248;248;242m    [(_ a b) (check-equal? (call-with-values (thunk a) list) b)]))[0m


[38;2;117;113;94m;; Named POSIX semaphores[0m
[38;2;248;248;242m(test-begin[0m
[38;2;248;248;242m  [0m[38;2;248;248;242m([0m[38;2;249;38;114mdefine[0m[38;2;248;248;242m [0m[38;2;166;226;46mtest-sem-name[0m[38;2;248;248;242m [0m[38;2;248;248;242m"/test-nix-[0m[38;2;190;132;255m1[0m[38;2;248;248;242m")[0m

[38;2;248;248;242m  [0m[38;2;117;113;94m;; Unlink if already exists[0m
[38;2;248;248;242m  (sem-unlink test-sem-name)[0m

[38;2;248;248;242m  [0m[38;2;117;113;94m;; Open and unlink[0m
[38;2;248;248;242m  [0m[38;2;248;248;242m([0m[38;2;249;38;114mdefine[0m[38;2;248;248;242m [0m[38;2;166;226;46mtest-sem-p[0m[38;2;248;248;242m [0m[38;2;248;248;242m(sem-open test-sem-name (+ O_CREAT O_EXCL)))[0m
[38;2;248;248;242m  (check-not-false test-sem-p)[0m
[38;2;248;248;242m  (check-not-equal? test-sem-p (void))[0m
[38;2;248;248;242m  (check-exn exn:fail?[0m
[38;2;248;248;242m             [0m[38;2;248;248;242m([0m[38;2;249;38;114mlambda[0m[38;2;248;248;242m [0m[38;2;248;248;242m() (sem-open test-sem-name (+ O_CREAT O_EXCL)))[0m
[38;2;248;248;242m             [0m[38;2;230;219;116m"Permission denied"[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (check-exn exn:fail?[0m
[38;2;248;248;242m             [0m[38;2;248;248;242m([0m[38;2;249;38;114mlambda[0m[38;2;248;248;242m [0m[38;2;248;248;242m() (sem-open test-sem-name (+ O_CREAT O_EXCL))))[0m

[38;2;248;248;242m  [0m[38;2;117;113;94m;; Change values[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m0[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (sem-post test-sem-p)[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m1[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (sem-wait test-sem-p)[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m0[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (sem-post test-sem-p)[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m1[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (sem-post test-sem-p)[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m2[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (sem-trywait test-sem-p)[0m
[38;2;248;248;242m  (check-equal? (sem-getvalue test-sem-p) [0m[38;2;190;132;255m2[0m[38;2;248;248;242m)[0m

[38;2;248;248;242m  [0m[38;2;117;113;94m;; Can't unlink twice[0m
[38;2;248;248;242m  (check-not-false (sem-unlink test-sem-name))[0m
[38;2;248;248;242m  (check-false (sem-unlink test-sem-name)))[0m


[38;2;117;113;94m;; Named POSIX shared memory[0m
[38;2;248;248;242m(test-begin[0m
[38;2;248;248;242m  [0m[38;2;248;248;242m([0m[38;2;249;38;114mdefine[0m[38;2;248;248;242m [0m[38;2;166;226;46mtest-shm-name[0m[38;2;248;248;242m [0m[38;2;248;248;242m"test-nix-mem-[0m[38;2;190;132;255m1[0m[38;2;248;248;242m")[0m

[38;2;248;248;242m  [0m[38;2;117;113;94m;; Open and unlink[0m
[38;2;248;248;242m  (shm-unlink test-shm-name)[0m
[38;2;248;248;242m  [0m[38;2;248;248;242m([0m[38;2;249;38;114mdefine[0m[38;2;248;248;242m [0m[38;2;166;226;46mtest-shm-fd[0m[38;2;248;248;242m [0m[38;2;248;248;242m(shm-open test-shm-name (+ O_EXCL O_CREAT O_RDWR) #o644))[0m
[38;2;248;248;242m  (check > test-shm-fd [0m[38;2;190;132;255m0[0m[38;2;248;248;242m)[0m
[38;2;248;248;242m  (check-not-false (shm-unlink test-shm-name))[0m
[38;2;248;248;242m  (check-false (shm-unlink test-shm-name)))[0m
