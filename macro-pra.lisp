
;;;; //// macro-pra////

;; //// 素数判定する関数 （全探索） ////
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

;; //// 与えられた引数と同じかそれより大きい次の素数を返す関数 ////
(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; //// こんな風に書きたい。とする ////
;(do-primes (p 0 19)
;  (format t "~d " p))

;; //// macroがなければ以下のような形になるだろう、つまりこの形が展開形だ。
;(do (( p (next-prime 0) (next-prime (1+ p))))
;    ((> p 19))
;  (format t "~d " p))

;;;; //// こういう書き方はできるが ////
;(defmacro do-primes (var-and-range &rest body)
;  (let ((var (first var-and-range))
;        (start (second var-and-range))
;        (end (third var-and-range)))
;    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;         ((> ,var ,end))
;       ,@body)))
;
;;slimeでの下に出てくる表記(do-primes var-and-range &rest body)

;;;; //// こちらの方が良い。なぜならSLIMEなどの開発環境で分配パラメータリストを使用
;するとマクロ呼び出しのシンタックスを教えてくれるからだ。
;(defmacro do-primes ((var start end) &body body)
;  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;       ((> ,var ,end))
;     ,@body))

;;;; //// さらに漏れを無くすためにend一度だけ評価し結果の値を
;;;;      後で使うように保存しておくコードを生成する方法がより良い ////
;(defmacro do-prime ((var start end) &body body)
;  `(do ((ending-value ,end)
;        (,var (next-prime ,start) (next-prime (1+ ,var))))
;       ((> ,var ending-value))
;     ,@body))
;
; しかし、これでは、
;; (do-primes (p 0 (random 100)) (format t "~d " p))
; のようなランダムなどの要素が与えられるとうまく行かない場合がでてくる
; これを（抽象化の漏れ）と呼ぶ。
































