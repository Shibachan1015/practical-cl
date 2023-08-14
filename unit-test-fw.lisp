


;;;; //// どの関数から呼ばれてテストに落ちたのかがわかるようにするため
;;;;      トップレベルダイナミック変数を宣言しておく。
(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[⛔FAIL~;✅pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;;;
;(defmacro check (form)
;  `(report-result ,form ',form))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

;;;; //// 以下の関数定義からダイナミック変数、テストの名前の部分はマクロで抽象化できる。
;(defmacro deftest (name parameters &body body)
;  `(defun ,name ,parameters
;     (let ((*test-name* ',name))
;       ,@body)))
;;;; ↓

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))



;(defun test-+ ()
;  (let ((*test-name* 'test-+))
;    (check
;      (= (+ 1 2) 3)
;      (= (+ 1 2 3) 6)
;      (= (+ -1 -3) -4))))

;(defun test-* ()
;  (let ((*test-name* 'test-*))
;    (check
;      (= (* 2 2) 4)
;      (= (* 3 5) 15))))

;;;; ↑をマクロで抽象化した
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)
    (= (* 4 5) 10)))


;(defun test-arithmetic ()
;  (combine-results
;    (test-+)
;    (test-*)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;;; 上のコードと等価
;(defun test-+ ()
;  (progn
;    (report-result (= + 1 2) 3) '(= 1 2) 3)
;    (report-result (= + 1 2 3) 6) '(= 1 2 3) 6)
;    (report-result (= + -1 -3) -4) '(= -1 -3) -4))))


;;;; //// 9.3 戻り値を手直しする
;ANDを使いたいが、ANDは一つのテストに失敗した時点で、残りのテストをせず短絡する。
;だから今欲しいのはこんなマクロ
;(combine-results
;  (foo)
;  (bar)
;  (baz))
;次のように展開されてほしい。
;(let ((result t))
;  (unless (foo) (setf result nil))
;  (unless (bar) (setf result nil))
;  (unless (baz) (setf result nil))
;  result)

;;;; //// マクロの展開形に出てくる変数の名前にリテラルを使うと、マクロによる
;抽象化が漏れる危険性がある。だからユニークな名前をつくらなければいけない。
;そこで、gensymマクロを使う。
;;;; //// 以下のコードの今のところの理解としては、nがnameの数になるまでgensymで
;でユニークな名前を毎回つけろというマクロを定義した。というところか？
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))





























