
; S式の中から、未宣言の変数を検索する
;   vars - 宣言済み変数のリスト
;   s-exp - S式
; returns
;   未宣言の変数のリスト
(defun strictsub (vars s-exp / warnings word add-warn test-var call-self eval-rest)
  (setq warnings nil)

  (setq add-warn (lambda (x)
    (setq warnings (cons x warnings))
  ))

  (setq call-self (lambda (v x)
      (setq warnings (append warnings (strictsub v x)))
  ))

  (setq eval-rest (lambda (a / tmp)
    (if (listp a)
      (foreach tmp a
        (if (listp tmp)
          (call-self vars tmp)
        )
      )
    )
  ))

  (setq test-var (lambda (v)
    (if (and v (not (member v vars)))
      (add-warn v)
    )
  ))

  (cond
    ((or (not s-exp) (not (listp s-exp)))
         ; do nothing
    )
    ((= (setq word (car s-exp)) 'LAMBDA)
      (call-self (append vars (cadr s-exp)) (cddr s-exp))
    )
    ((= word 'FOREACH)
      (test-var (cadr s-exp))
      (eval-rest (cddr s-exp))
    )
    ((= word 'SETQ)
      ((lambda (/ equtions)
        (setq equtions (cdr s-exp))
        (while equtions
            (test-var (car equtions))

            (if (listp (cadr equtions))
              (call-self vars (cadr equtions))
            )

            (setq equtions (cddr equtions))
        )
      ))
    )
    (T
      (if (listp (car s-exp))
        (call-self vars (car s-exp))
      )
      (eval-rest (cdr s-exp))
    )
  )
  warnings
)

; Lisp のソースで定義されている関数内で、
; ローカル宣言されていない変数に setq / foreach していたら、
; 表示する
(defun strict (fname / tmp fd source s-exp warnings uniq w)
  ; 拡張子がなければ付加
  (if (or (< (strlen fname) 4)
          (/= (strcase (substr fname (- (strlen fname) 3))) ".LSP"))
    (setq fname (strcat fname ".lsp"))
  )
  (cond
    ; 検索パスからフルパス検索できなければエラー
    ((not (setq tmp (findfile fname)))
      (alert (strcat "can not find " fname))
    )
    ; ソースをロードできなければエラー
    ((not (setq fd (open tmp "r")))
      (alert (strcat "can not open " tmp))
    )
    (T
      ; ソースを全て読み込む
      (while (setq tmp (read-line fd))
        (setq source (if source (strcat source "\n" tmp) tmp))
      )
      ; S式化
      (setq s-exp (read (strcat "(\n" source "\n)")))
      (foreach tmp s-exp
        (if (= (car tmp) 'DEFUN)
          (if (setq warnings (strictsub (caddr tmp) (cdddr tmp)))
            (progn
              ; 未宣言の変数が見付かったのでリポート
              ; 関数名
              (princ "\nOn ")
              (prin1 (cadr tmp))

              ; 変数リストを重複を削って表示
              (setq uniq nil)
              (foreach w warnings
                (if (not (member w uniq))
                  (setq uniq (cons w uniq))
                )
              )
              (foreach w uniq
                (princ "\n  ")(prin1 w)
              )
            )
            ; else
            (progn
              (terpri)
              (prin1 (cadr tmp))
              (princ " -> Ok")
            )
          )
        )
      )
    )
  )
  (if fd (close fd))
  (princ)
)
