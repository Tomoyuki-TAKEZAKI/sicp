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
