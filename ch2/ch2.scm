;;; EXERCISE 2.1

;; 正と負の両方の引数を扱うことができる改良版 make-rat

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (> d 0) + -)))
    (cons (/ (sign n) g) (/ (sign d) g))))


;;; EXERCISE 2.2

;; point

(define (point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

;; segment

(define (make-segment p-start p-end)
  (cons p-start p-end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let (
    (p1 (start-segment segment))
    (p2 (end-segment segment)))
  (point 
    (average (x-point p1) (x-point p2))
    (average (y-point p1) (y-point p2)))))


;;; EXERCISE 2.3

;; 長方形を、その上のある一点から伸びる二つの線分の組として表現する

;; rectangle

;; TODO 二つの線分が同じ始点をもつことを assert する
;; TOOD 二つの線分が直交していることを assert する
(define (make-rectangle one-segment another-segment)
  (cons one-segment another-segment))

(define (one-segment-of-rect rectangle)
  (car rectangle))

(define (another-segment-of-rect rectangle)
  (cdr rectangle))

;; segment

(define (length-of-segment segment)
  (let (
    (p1 (start-segment segment))
    (p2 (end-segment segment)))
  (sqrt 
    (+ (square (- (x-point p1) (x-point p2)))
       (square (- (y-point p1) (y-point p2)))))))

;; square

(define (area-of-rect rectangle)
  (let (
    (l1 (one-segment-of-rect rectangle))
    (l2 (another-segment-of-rect rectangle)))  
  (* (length-of-segment l1)
     (length-of-segment l2))))

;; (area-of-rect (make-rectangle (make-segment (point 0 0) (point 3 0)) (make-segment (point 0 0) (point 0 4))))
;; 12

;; circumference

(define (circumference-of-rect rectangle)
  (let (
    (l1 (one-segment-of-rect rectangle))
    (l2 (another-segment-of-rect rectangle)))  
  (+ (* 2 (length-of-segment l1))
     (* 2 (length-of-segment l2)))))

;; (circumference-of-rect (rectangle (make-segment (point 0 0) (point 3 0)) (make-segment (point 0 0) (point 0 4))))
;; 14

;; WIP
;;
;; 長方形の異なる表現として、他には「長方形の対角線の組」が考えられる。
;; この場合、線分のベクトル和を定義することで、以下の二つの表現を相互に変換できる
;; 
;; 1. 一つの頂点から伸びる異なる二辺の組としての表現
;; 2. 対角線の組としての表現
;; 
;; 前者を side1, side2,  後者を diag1, diag2 とすると、関係は次の通り
;;
;; side1 = (diag1 + diag2)/2
;; side2 = (diag1 - diag2)/2
;; 


;;; EXERCISE 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


;;; EXERCISE 2.5

;; この実装では整数の入力を実数で取り出してしまう

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (if (= (modulo z 3) 0)
      (car (/ z 3))
      (log z 2)))

(define (cdr z)
  (if (= (modulo z 2) 0)
      (cdr (/ z 2))
      (log z 3)))


