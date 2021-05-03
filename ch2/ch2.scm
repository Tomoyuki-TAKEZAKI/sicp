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


;;; EXERCISE 2.7

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;;; EXERCISE 2.8

;; 区間の (-1) 倍を考える
;; これは区間の上限と下限を入れ替え、 (-1) 倍すれば良い
;; 区間の差は、一つ目の区間と二つ目の区間の (-1) 倍との和に等しい

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;;; EXERCISE 2.9

;; 区間 (a, b), (c, d) を考える。
;; ここで区間 (x, y) は下限 x, 上限 y であるとする。

;; 区間の和
;; (a+c, b+d)
;; この区間の幅は、元の区間の幅の和となる。
;; (b+d) - (a+c) = (b-a) + (d-c)

;; 区間の差
;; (a-d, b-c)
;; この区間の幅は、元の区間の幅の和となる。
;; (b-c) - (a-d) = (b-a) + (d-c)

;; 区間の積,商
;; 一方、区間の積や商の幅は、元の区間の幅の積や商ではない。
;; 次の具体例を考える。
;;
;; (1,2),(3,4)
;; 
;; 区間の積は (3,8) となるが、この幅 5 は元の区間の幅の積 1 とは等しくない。
;; 区間の商は (1/4, 2/3) となるが、この幅 5/12 は元の区間の幅の積 1 とは等しくない。


;; EXERCISE 2.10

;; TODO エラーメッセージ表示を改善
;; 現状だと print-interval が評価された後に "Division ... zero #void" が表示される

(define (div-interval x y)
  (let ((ub-y (upper-bound y))
        (lb-y (lower-bound y)))
    (if (< (* ub-y lb-y) 0) 
        (error "Division by section across zero" (print-interval y))
        (mul-interval
          x
          (make-interval (/ 1.0 ub-y)
                         (/ 1.0 lb-y))))))

(define (print-interval interval)
  (display "interval: [ lower-bound: ")
  (display (lower-bound interval))
  (display ", upper-bound: ")
  (display (upper-bound interval))
  (display " ]"))


;;; EXERCISE 2.11

;; 区間を次の三つに分類する。
;;
;; 1. 区間の下端が正
;; 2. ゼロをまたぐ区間
;; 3. 区間の上端が負
;; 
;; したがって、二つの区間の積は9パターンに分類できる。これらをさらに次の通り分類する。
;;
;; a. 二つの区間がともに 2. の場合
;; 上端と下端の可能な4つの積がいずれも上端もしくは下端となりうる。
;; したがって、合計4回の掛け算が必要になる。
;; 
;; WIP その他の場合分け

; (define (mul-interval x y)
;   (let ((lb-x (lower-bound x))
;         (lb-y (lower-bound y))
;         (ub-x (upper-bound x))
;         (ub-y (upper-bound y)))
;     (if (and (< (* lb-x ub-x) 0) (< (* lb-y ub-y) 0))
;         (let
;           ((p1 (* lb-x lb-y))
;            (p2 (* lb-x ub-y))
;            (p3 (* ub-x lb-y))
;            (p4 (* ub-x ub-y)))
;           (make-interval (min p1 p2 p3 p4)
;                          (max p1 p2 p3 p4)))
;         (let
;           ((p1 (* lb-x ub-y))
;            (p2 (* lb-y ub-x)))
;           (make-interval (min p1 p2) (max p1 p2)))
;         )))


;;; EXERCISE 2.17

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))


;;; EXERCISE 2.18

(define (reverse items)
  (define (iter items answer)
    (if (null? items)
        answer
        (iter (cdr items) (cons (car items) answer))))
  (iter items nil))


;;; EXERCISE 2.20

;; TODO EXERCISE 2.22 と同じく逆順になる
;; reverse を使わずに対処するように修正する

(define (same-parity . items)
  (define (iter x items answer)
    (define (same-parity? x y)
      (let ((parity (modulo (- x y) 2)))
        (= 0 parity)))
    (if (null? items)
        (reverse answer)
        (if (same-parity? x (car items))
            (iter x (cdr items) (cons (car items) answer))
            (iter x (cdr items) answer))))
  (iter (car items) items nil))


;;; EXERCISE 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; (define (square x) (* x x))


;;; EXRECISE 2.22

;; Louis による square-list 手続きは次の通り。

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things) 
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; cons の引数の順序が逆になっているため、答えとなるリストは望むものの逆順になる。
;; 例として、 (list 1 2 3) に対する処理を考えると分かりやすい。
;; イテレーションごとの iter の引数は次の通り
;;
;; (cons 1 (cons 2 (cons 3 nil))), nil
;; (cons 2 (cons 3 nil)), (cons 1 nil)
;; (cons 3 nil), (cons 4 (cons 1 nil))
;; nil, (cons 9 (cons 4 (cons 1 nil)))

;; Louis による修正版の手続きは次の通り。

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer
;;                     (square (car things))))))
;;   (iter items nil))

;; この場合、チェーン内の次のペアが cdr ではなく car に入るため、結果がリストとならない。
;; 例として、 (list 1 2 3) に対する処理を考えると分かりやすい。
;; イテレーションごとの iter の引数は次の通り。
;;
;; (cons 1 (cons 2 (cons 3 nil))), nil
;; (cons 2 (cons 3 nil)), (cons nil 1)
;; (cons 3 nil), (cons (cons nil 1) 4)
;; nil, (cons (cons (cons nil 1) 4) 9)


;;; EXERCISE 2.23

(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      #t
      (for-each proc (cdr items))))

