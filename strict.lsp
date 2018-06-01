
; S���̒�����A���錾�̕ϐ�����������
;   vars - �錾�ςݕϐ��̃��X�g
;   s-exp - S��
; returns
;   ���錾�̕ϐ��̃��X�g
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

; Lisp �̃\�[�X�Œ�`����Ă���֐����ŁA
; ���[�J���錾����Ă��Ȃ��ϐ��� setq / foreach ���Ă�����A
; �\������
(defun strict (fname / tmp fd source s-exp warnings uniq w)
  ; �g���q���Ȃ���Εt��
  (if (or (< (strlen fname) 4)
          (/= (strcase (substr fname (- (strlen fname) 3))) ".LSP"))
    (setq fname (strcat fname ".lsp"))
  )
  (cond
    ; �����p�X����t���p�X�����ł��Ȃ���΃G���[
    ((not (setq tmp (findfile fname)))
      (alert (strcat "can not find " fname))
    )
    ; �\�[�X�����[�h�ł��Ȃ���΃G���[
    ((not (setq fd (open tmp "r")))
      (alert (strcat "can not open " tmp))
    )
    (T
      ; �\�[�X��S�ēǂݍ���
      (while (setq tmp (read-line fd))
        (setq source (if source (strcat source "\n" tmp) tmp))
      )
      ; S����
      (setq s-exp (read (strcat "(\n" source "\n)")))
      (foreach tmp s-exp
        (if (= (car tmp) 'DEFUN)
          (if (setq warnings (strictsub (caddr tmp) (cdddr tmp)))
            (progn
              ; ���錾�̕ϐ������t�������̂Ń��|�[�g
              ; �֐���
              (princ "\nOn ")
              (prin1 (cadr tmp))

              ; �ϐ����X�g���d��������ĕ\��
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
