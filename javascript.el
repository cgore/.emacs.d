;;; JavaScript and JSON

;; https://github.com/mooz/js2-mode
;; http://elpa.gnu.org/packages/js2-mode.html
(require 'js2-mode)

;; https://github.com/magnars/js2-refactor.el
;;
;;     ee is expand-node-at-point: Expand bracketed list according to node type at point (array, object, function, call args).
;;     cc is contract-node-at-point: Contract bracketed list according to node type at point (array, object, function, call args).
;;     ef is extract-function: Extracts the marked expressions out into a new named function.
;;     em is extract-method: Extracts the marked expressions out into a new named method in an object literal.
;;     tf is toggle-function-expression-and-declaration: Toggle between function name() {} and var name = function ();
;;     ta is toggle-arrow-function-and-expression: Toggle between function expression to arrow function.
;;     ts is toggle-function-async: Toggle between an async and a regular function.
;;     ip is introduce-parameter: Changes the marked expression to a parameter in a local function.
;;     lp is localize-parameter: Changes a parameter to a local var in a local function.
;;     wi is wrap-buffer-in-iife: Wraps the entire buffer in an immediately invoked function expression
;;     ig is inject-global-in-iife: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;;     ag is add-to-globals-annotation: Creates a /*global */ annotation if it is missing, and adds the var at point to it.
;;     ev is extract-var: Takes a marked expression and replaces it with a var.
;;     el is extract-let: Similar to extract-var but uses a let-statement.
;;     ec is extract-const: Similar to extract-var but uses a const-statement.
;;     iv is inline-var: Replaces all instances of a variable with its initial value.
;;     rv is rename-var: Renames the variable on point and all occurrences in its lexical scope.
;;     vt is var-to-this: Changes local var a to be this.a instead.
;;     ao is arguments-to-object: Replaces arguments to a function call with an object literal of named arguments.
;;     3i is ternary-to-if: Converts ternary operator to if-statement.
;;     sv is split-var-declaration: Splits a var with multiple vars declared, into several var statements.
;;     ss is split-string: Splits a string.
;;     st is string-to-template: Converts a string into a template string.
;;     uw is unwrap: Replaces the parent statement with the selected region.
;;     lt is log-this: Adds a console.log() statement for what is at point (or region). With a prefix argument, use JSON pretty-printing.
;;     dt is debug-this: Adds a debug() statement for what is at point (or region).
;;     sl is forward-slurp: Moves the next statement into current function, if-statement, for-loop or while-loop.
;;     ba is forward-barf: Moves the last child out of current function, if-statement, for-loop or while-loop.
;;     k is kill: Kills to the end of the line, but does not cross semantic boundaries.
;;
;; There are also some minor conveniences bundled:
;;
;;     C-S-down and C-S-up moves the current line up or down. If the line is an element in an object or array literal, it makes sure that the commas are still correctly placed.
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; https://github.com/NicolasPetton/xref-js2
;;
;; On OSX: $ brew install ag
;;
;; xref-js2 uses the xref, so the same keybindings and UI as other xref backends is used:
;;     M-. Jump to definition
;;     M-? Jump to references
;;     M-, Pop back to where M-. was last invoked
(require 'xref-js2)
(define-key js2-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setq js-indent-level 2 ; js-mode
      javascript-indent-level 2 ; javascript-mode
      web-mode-markup-indent-offset 2
      web-mode-markup-indent-offset 2  ; web-mode, html tag in html file
      web-mode-css-indent-offset 2 ; web-mode, css in html file
      web-mode-code-indent-offset 2  ; web-mode, js code in html file
      css-indent-level 2)  ; css-mode


;; (setq web-mode-content-types-alist
;;       '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.geojson$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; NodeJS stuff

(require 'nodejs-repl) ; https://github.com/abicky/nodejs-repl.el
(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-c M-j") 'nodejs-repl)
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-k") 'nodejs-repl-send-buffer)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
(defun nvm-which ()
  (let* ((shell (concat (getenv "SHELL") " -l -c 'nvm which'"))
         (output (shell-command-to-string shell)))
    (cadr (split-string output "[\n]+" t))))
(setq nodejs-repl-command #'nvm-which)
