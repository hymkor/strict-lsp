
; S式の中から、未宣言の変数を検索する
;   vars - 宣言済み変数のリスト
;   s-exp - S式
; returns
;   未宣言の変数のリスト
(defun strictsub (vars s-exp / equtions warnings tmp)
  (setq warnings nil)
  (cond
    ((or (not s-exp) (not (listp s-exp)))
         ; do nothing
    )
    ((= (car s-exp) 'LAMBDA)
      (setq warnings (append warnings (strictsub (append vars (cadr s-exp)) (cddr s-exp))))
    )
    ((= (car s-exp) 'SETQ)
      (setq equtions (cdr s-exp))
      (while equtions
          (if (not (member (car equtions) vars))
            (setq warnings (cons (car equtions) warnings))
          )
          (if (listp (cadr equtions))
            (setq warnings (append warnings (strictsub vars (cadr equtions))))
          )
          (setq equtions (cddr equtions))
      )
    )
    (T
      (if (listp (car s-exp))
        (setq warnings (append warnings (strictsub vars (car s-exp))))
      )
      (if (listp (cdr s-exp))
        (setq warnings (append warnings (strictsub vars (cdr s-exp))))
      )
    )
  )
  warnings
)

; Lisp のソースで定義されている関数内で、
; ローカル宣言されていない変数に setq していたら、
; 表示する
(defun strict (fname / tmp fd source s-exp warnings uniq)
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
          )
        )
      )
    )
  )
  (if fd (close fd))
  (princ)
)
