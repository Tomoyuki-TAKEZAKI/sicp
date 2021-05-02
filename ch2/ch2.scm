;;; EXERCISE 2,1

;; 正と負の両方の引数を扱うことができる改良版 make-rat

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (> d 0) + -)))
    (cons (/ (sign n) g) (/ (sign d) g))))

