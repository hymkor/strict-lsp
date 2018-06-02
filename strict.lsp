
; (strict) の下請け関数
; S式の中から、未宣言の変数を検索する
;   vars - 宣言済み変数のリスト
;   s-exp - S式
; returns
;   ( 未宣言の変数のリスト 使用された変数のリスト)
(defun strictsub (vars s-exp / warnings word add-warn add-used test-var call-self eval-rest used)
  (setq warnings nil)
  (setq used nil)

  (defun add-used (x)
    (if (not (member x used))
      (setq used (cons x used))
    )
  )

  (setq add-warn (lambda (x)
    (if (not (member x warnings))
      (setq warnings (cons x warnings))
    )
  ))

  (setq call-self (lambda (v x / r tmp)
      (setq r (strictsub v x))
      (foreach tmp (car r)
        (add-warn tmp)
      )
      (foreach tmp (cadr r)
        (add-used tmp)
      )
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
      (add-used v)
    )
  ))

  (cond
    ((or (not s-exp) (not (listp s-exp)))
         ; do nothing
    )
    ((= (setq word (car s-exp)) 'LAMBDA)
      (call-self (append vars (cadr s-exp)) (cddr s-exp))
    )
    ((= word 'DEFUN)
      (test-var (cadr s-exp))
      (call-self (append vars (caddr s-exp)) (cdddr s-exp))
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
  (list warnings used)
)

; Lisp のソースで定義されている関数内で、
; ローカル宣言されていない変数に setq / foreach していたら、
; 表示する
(defun strict (fname / tmp fd source s-exp r vars)
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
          ((lambda ( / r funcname v)
            (setq funcname (cadr tmp))
            (setq r (strictsub (setq vars (caddr tmp)) (cdddr tmp)))

            (foreach v (car r)
              (terpri)
              (prin1 funcname)
              (princ ": ")
              (prin1 v)
              (princ " is not declared.")
            )
            (foreach v (cdr (member '/ vars))
              (if (not (member v (cadr r)))
                (progn
                  (terpri)
                  (prin1 funcname)
                  (princ ": ")
                  (prin1 v)
                  (princ " is unused.")
                )
              )
            ) ; foreach
          )) ; lambda
        ) ; if
      ) ; foreach
    ) ; T
  ) ; cond
  (if fd (close fd))
  (princ)
)
