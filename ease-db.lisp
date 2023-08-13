


(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

;; //// 整形してプリント ////
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))


;; //// prompt input ////
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; //// make-cd + prompt-read "Title" から一行ずつ入力できる関数 ///
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Riped [y/n]: ")))

;; //// returnが返されるまでCDの登録フォームをループさせる ////
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]; ")) (return))))


;; //// file操作 ////
(defun save-db (filename)
  (with-open-file (out filename
                        :direction :output
                        :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
;; /////////////


;; //// artist名だけをselectする関数 ////
;(defun select-by-artist (artist)
;  (remove-if-not
;   #'(lambda (cd) (equal (getf cd :artist) artist))
;   *db*))
;これではartistだけなので、より使いやすくselectを以下のように一般化


;; //// select関数 ////
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
;;上記のようなセレクタを他にも用意する必要があるが、似たような関数をいくつも書くのは避けたい。そのために以下の汎用セレクタ関数生成機を作成する。

;; //// 汎用セレクタ関数生成機 = where関数 ////
;;(defun where (&key title artist rating (ripped nil ripped-p))
;;  #'(lambda (cd)
;;      (and
;;       (if title     (equal (getf cd :title)  title)  t)
;;       (if artist    (equal (getf cd :artist) artist) t)
;;       (if rating    (equal (getf cd :rating) rating) t)
;;       (if ripped-p  (equal (getf cd :ripped) ripped) t))))

;; これで以下のような検索が可能になった。
;; CL-USER> (select (where :artist "Dexie Chicks"))
;; CL-USER> ((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
;;           (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T))
;;
;; CL-USER> (select (where :rating 10 :ripped nil))
;; CL-USER> ((:TITLE "Man down" :ARTIST "Rehanna" :RATING 10 :RIPPED NIL))

;; //// UPDATE関数 ////

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; //// DELETE関数 ////
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


;; //// 逆順にするマクロ ////
;;;;(defmacro backwords (expr) (reverse expr))
;; CL-USER> (backwords ("hello, world" t format))
;; hello, world
;; NIL

;; //// MACROのお勉強 ////
; where関数のところで、自動的に以下のリストを出してほしい。
;; (equal (getf cd field) value)
;;;; (defun make-comparison-expr (field value)
;;;;  (list 'equal (list 'getf 'cd field) value))
;; CL-USER> (make-comparison-expr :title "Give Us a Break")
;; (EQUAL (GETF CD :TITLE) "GIive Us a Break")
;; 狙い通り出力された。が、以下のやり方でmacroのを使いはじめるためにちょうど良い方法だ。
;; (`)バッククォートの後ろは評価しない。が、(,)の後ろの単語だけは評価する。
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))






