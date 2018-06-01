strict.lsp
==========

In the commandline of AutoCAD or BricsCAD:

Put `strict.lsp` on CAD's search-path and type as below.

```
(load "strict")
(strict "SCRIPT.LSP")
```

and it shows

- unused variables whose declarations exist in `(defun)`
- used vairables without declaration in `(defun)`

about SCRIPT.LSP.

- `(let)` is not supported because autolisp does not have it.

For example
-----------

foo.lsp:

```
(defun hoge (/ foo)
  (setq bar 1)
)
```

do on the commandline of CAD:

```
: (strict "foo")

HOGE: BAR is not declared.
HOGE: FOO is unused.
```
