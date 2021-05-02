;;; EXERCISE 1.1

10

;; 10

(+ 5 3 4)

;; 12

(- 9 1)

;; 8

(/ 6 2)

;; 3

(+ (* 2 4) (- 4 6))

;; 6

(define a 3)
(define b (+ a 1))
(+ a b (* a b))

;; 19

(= a b)

;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)

;; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

;; 16

(+ 2 (if (> b a) b a))

;; 6

(* (cond((> a b) a)
        ((< a b) b)
        (else -1))
   (+ a 1))

;; 16


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

;; 式 (if (> b 0) + -) は、bが正の値であれば演算子+を、そうでなければ演算子-を返す
;; したがって、 式 (a-plus-abs-b a b) は、b の絶対値と a の和を返す
;; 数式で表すと a + |b| となる


;;; EXERCISE 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))

;; 正規順序評価の場合
;; この評価方法は、「完全に展開してから簡約する」。

;; (test 0 (p))

;; (if (= x 0)
;;      0
;;      (p))

;; 0

;; インタプリタは式を 0 と評価する。


;; 適用順序評価の場合
;; この評価方法は、「引数を評価してから適用する」。

;; (test 0 (p))

;; インタプリタは (p) の評価を行うが、(p) が再帰的に評価され無限ループに陥る。
;; test 式の述語式の評価に到達せず、インタプリタは値を返すことができない。


;;; EXERCISE 1.6

;; 特殊形式 if の一般形式は次の通りであった。

;; (if <predicate> <consequent> <alternative>)

;; インタプリタは if 式の <predicate> を最初に評価し、
;; <predicate> の 評価結果が #t である場合 <consequent> を、
;; #f である場合 <alternative> を評価して値を返す。

;; 一方、 new-if の定義は次の通りである。

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; new-if は特殊形式ではないため、通常の手続きと同様に Lisp インタプリタによって
;; 適用順序評価される。すなわち new-if は predicate, then-clause, else-clause
;; の全てが評価されてはじめて値を返す。

;; したがって、手続き sqrt-iter において if を new-if で置き換えた場合、
;; 手続き good-enough? が #t を返しても sqrt-iter の呼び出しを続け、プログラムは停止しない。
;; コンピュータのメモリは有限なので、最終的にスタックオーバーフローが発生する。


;;; EXERCISE 1.8

(define (cube-root x)

  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))

  (define (cube x) (* x x x))

  (cube-root-iter 1.0))


;;; EXERCISE 1.9


(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; このプロセスは再帰である。

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; このプロセスは反復である。

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9


;;; EXERCISE 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; ...
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
;; ...
;; 65535

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; ...
;; (A 2 (A 0 2))
;; (A 2 4)
;; 65535


(define (f n) (A 0 n))

;; n = 0 のとき
;; (f 0)
;; (A 0 0)
;; 0

;; n > 0 のとき
;; (f n)
;; (A 0 n)
;; 2n

;; $ f(n) = 2n $


(define (g n) (A 1 n))

;; (g n)

;; n = 0 のとき
;; (g 0)
;; (A 1 0)
;; 0

;; n > 0 のとき
;; (g n)
;; (A 1 n)
;; (A 0 (A 1 (- n 1)))
;; ...
;; (A 0 (A 0 ... (A 0 (A 1 1)) ...))
;; (A 0 (A 0 ... (A 0 2) ...))

;; ここで、 括弧のネストは n 重であることに注意すると
;; $ g(n) = 0    (n = 0) $
;; $ g(n) = 2^n  (n > 0) $


(define (h n) (A 2 n))

;; n = 0 のとき
;; (h 0)
;; (A 2 0)
;; 0

;; n > 0 のとき
;; (h n)
;; (A 2 n)
;; (A 1 (A 2 (- n 1)))
;; (g (h (- n 1)))
;; (g (g ... (h 1) ...))

;; ここで、括弧のネストは n 重であることに注意すると
;; $ h(n) = 0          (n = 0) $
;; $ h(n) = 2^2^...^2  (n > 0) $
;; ただし、n > 0 の場合 2 の個数は n に等しい。すなわち
;; $ h(1) = 2 = 2 $
;; $ h(2) = 2^2 = 4 $
;; $ h(3) = 2^2^2 = 2^4 = 16 $
;; $ h(4) = 2^2^2^2 = 2^16 = 65536 $
;; ...


;;; EXERCISE 1.11

;; 再帰プロセスによる手続き

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;; 反復プロセスによる手続き

(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* b 2) (* c 3)) a b (- count 1))))


;;; EXERCISE 1.13

;; まず、 $ Fib(n) = (\phi^n - \psi^n) / sqrt(5) $ を証明する。
;; ここで $ \phi = (1 + sqrt(5)) / 2 $、 $ \psi = (1 - sqrt(5)) / 2 $ である。

;; n = 0, 1 のとき、式は明らかに成り立つ。
;; n = k のときに式が成り立つと仮定する。このとき

;; $ \phi^(k + 1) - \psi^(k + 1) $
;; $ = (1 - \psi) \phi^k - (1 - \phi) \psi^k $
;; $ = (\phi^k - \psi^k) + (\phi^(k-1) - \phi(k-1)) $

;; ここで、１番目の等号では $ \phi + \psi = 1 $ を、
;; ２番目の等号では $ \phi \psi = -1 $ をそれぞれ用いた。
;; n = k+1 のとき示すべき式は $ Fib(k) + Fib(k-1) $ に等しいことがわかる。

;; さて、いま示した恒等式から次の式が成立する。

;; $ Fib(n) - \phi^n / sqrt(5) = - \psi^n / sqrt(5) $

;; $ \psi$  の絶対値が 1 未満であることから、 右辺の絶対値は n の増加とともに指数関数的に減少する。
;; 右辺の絶対値は n = 0 のときに最大値 0.27 をとり、これは 0.5 よりも小さい。

;; したがって、 Fib(n) は $ \phi^n / sqrt(5) $ に最も近い整数である。 ■


;;; EXERCISE 1.15

;; a
;; 手続き p は、 sine の引数を 3.0 で割り続け、引数が 0.1 より小さくなるまでネストする。

;; (sine 12.5)
;; (p (sine 4.16))
;; (p (p (sine 1.38)))
;; (p (p (p (sine 0.46))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p 0.05)))))

;; したがって、(sine 12.5) を評価する際に手続き p は 5 回適用される

;; b
;; 手続き cube によって生成されるプロセスが使用する空間とステップ数は入力によらず一定である。
;; したがって、 手続き p のプロセスが使用する空間とステップ数の増加オーダーを考えればよい。
;; 手続き sine の引数 a が 自然数 n を用いて次のように表せるとする。

;; $ a = 0.1 * 3^n $

;; (sine a) を評価するに際、手続き p のプロセスはおよそ n 重のネストとなり、
;; 使用する空間もステップ数も n に比例する。
;; したがって、手続き (sine a) によって生成されるプロセスが使用する
;; 空間とステップ数の増加オーダーは $ \Theta( \log a ) $ 。
;; 対数の底の変換は項に定数倍の影響しか与えないため省略した。


;;; EXERCISE 1.16

;; 基数 b, 指数 n に対して、状態 (b, n, a) を定義する
;; そして、ab^n が不変量となるような状態変換を次の通り与える

;; n > 0 かつ n が偶数のとき
;; (b, n, a) -> (b^2, n/2, a)

;; n > 0 かつ n が奇数のとき
;; (b, n, a) -> (b, n-1, ab)

;; 指数計算を対数的ステップ数で実行する反復的プロセスを生成する手続きを
;; 次の通り与えることができる

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b) (/ counter 2) product))
        (else (fast-expt-iter b (- counter 1) (* b product)))))
  
(define (even? n)
  (= (remainder n 2) 0))


;;; EXERCISE 1.17

;; 掛け算を double, halve, 足し算の対数的ステップ数で実行する線形再帰プロセスを生成する手続きを
;; 次の通り与えることができる

(define (times a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (times (double a) (halve b)))
        (else (+ a (times a (- b 1))))))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))


;;; EXERCISE 1.18

;; a, b に対して、状態 (a, b, c) を定義する
;; そして、 ab + c が不変量となるような状態変換を次の通り与える

;; b > 1 かつ b が偶数のとき
;; (a, b, c) -> ((double a), (halve b), c)

;; b > 1 かつ b が奇数のとき
;; (a, b, c) -> (a, b-1, c + a)

;; 掛け算を double, halve, 足し算の対数的ステップ数で実行する反復プロセスを生成する手続きを
;; 次の通り与えることができる

(define (times a b)
  (times-iter a b 0))

(define (times-iter a b c)
  (cond ((= b 0) 0)
        ((= b 1) (+ a c))
        ((even? b) (times-iter (double a) (halve b) c))
        (else (times-iter a (- b 1) (+ a c) ))))


;;; EXERCISE 1.19

;; ペア (a,b) に対し、変換族 $ T_{pq} $ を次の通り定義する

;; a <- bq + aq + ap
;; b <- bp + aq

;; $ T_{pq} $ を二回適用した変換もまた同じ形式を $ T_{p'q'} $ 一回適用するのと同じことであり、
;; 愚直な計算から p', q' は次の通り与えられる 

;; p' = p^2 + q^2
;; q' = 2pq + q^2

;; この事実を利用して、フィボナッチ数を対数的ステップ数で実行する反復プロセスを生成する手続きを
;; 次の通り与えることができる

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;; EXERCISE 1.21

(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7


;;; EXERCISE 1.26


;; Louis による expmod 手続きは次の通り

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base
                        (expmod base (- exp 1) m)) 
                     m))))

;; この実装では、expmod 手続きの中で expmod 手続きが二回使われている。
;; expmod の呼び出しが k 重でネストしているとき、その合計呼び出し回数は最大で

;; 2^0 + 2^1 + ... + 2^(k-1) = 2^k

;; となる。一方、 square を使う場合は呼び出し回数が最大で k 回である。
;; 入力を n とすると k はおよそ $ \log (n) $ に等しい。
;; したがって、元の square を使う手続きが $ \Theta ( \log n ) $ であるのに対して、
;; Louis の手続きは $ \Theta (n) $ である。



;;; EXERCISE 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;;; EXERCISE 1.31

;; a 
;; 特定範囲の点における関数の値の積を返す手続きは次の通り

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; factorial は次の通り定義できる

(define (factorial n)
  (product identity 1 inc n))

;; ここで、次の手続きを利用した

(define (inc n) (+ n 1))
(define (identity x) x)

;; pi/4 の近似値は次の関数で与えられる

(define (pi-over-four-approx n)
  (define (numerator n)
    (define (term m)
      (if (even? m)
          (+ m 2)
          (+ m 1)))
    (product term 1.0 inc n))
  (define (denominator n)
    (define (term m)
      (if (even? m)
          (+ m 1)
          (+ m 2)))
    (product term 1.0 inc n))
  (/ (numerator n) (denominator n)))

;; b

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
      (product term (next a) next b))))


;; EXERCISE 1.32

;; a

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
        (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

;; b

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;;; EXERCISE 1.33

(define (filterd-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner
                        result
                        (if (filter a)
                            (term a)
                            null-value)))))
  (iter a null-value))

;; WIP filter が 引数 a を取ることを知っている前提になっている
;; より一般的な記述にする

;; a
(define (sum-of-squares-of-prime a b)
  (filterd-accumulate prime? + 0 square a inc b))

;; b

;; WIP


;;; EXERCISE 1.35

;; 黄金比 $ \phi $ が x -> 1 + 1/x という変形の不動点であることは、
;; 黄金比が次の式を満たすことから明らかである。

;; $ \phi^2 = \phi + 1 $

;; 黄金比を求める手続きは次の通り。

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))


;;; EXERCISE 1.36

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; $ x -> \log(1000) / \log(x) $ の不動点を求めることによって
;; x^x = 1000 の解を求める

;; 平均緩和法を利用しない場合

(define (exercise-1.36)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               10.0))

;; 33 ステップを要した

;; 平均緩和法を利用する場合

(define (exercise-1.36-avg)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               10.0))

;; 10 ステップを要した


