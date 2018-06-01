
; S���̒�����A���錾�̕ϐ�����������
;   vars - �錾�ςݕϐ��̃��X�g
;   s-exp - S��
; returns
;   ���錾�̕ϐ��̃��X�g
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

; Lisp �̃\�[�X�Œ�`����Ă���֐����ŁA
; ���[�J���錾����Ă��Ȃ��ϐ��� setq ���Ă�����A
; �\������
(defun strict (fname / tmp fd source s-exp warnings uniq)
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
          )
        )
      )
    )
  )
  (if fd (close fd))
  (princ)
)
