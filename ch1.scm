;;; EXERCISE 1.2

(/ (+ 5 4
        (- 2
           (- 3
              (+ 6
                 (/ 4 5)))))
     (* 3
        (- 6 2)
        (- 2 7)))


;;; EXERCISE 1.3

(define (f a b c)
    (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
               ((and (>= b a) (>= c a)) (sum-of-squares b c))
               ((and (>= c b) (>= a b)) (sum-of-squares c a))))


;;; EXERCISE 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; 式 (if (> b 0) + -) は、bが正の値であれば演算子+を、そうでなければ演算子-を返す
;;; したがって、 式 (a-plus-abs-b a b) は、b の絶対値と a の和を返す
;;; 数式で表すと a + |b| となる


;;; EXERCISE 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))

;;; 正規順序評価の場合
;;; この評価方法は、「完全に展開してから簡約する」。

;;; (test 0 (p))

;;; (if (= x 0)
;;;      0
;;;      (p))

;;; 0

;;; インタプリタは式を 0 と評価する。


;;; 適用順序評価の場合
;;; この評価方法は、「引数を評価してから適用する」。

;;; (test 0 (p))

;;; インタプリタは (p) の評価を行うが、(p) が再帰的に評価され無限ループに陥る。
;;; test 式の述語式の評価に到達せず、インタプリタは値を返すことができない。


