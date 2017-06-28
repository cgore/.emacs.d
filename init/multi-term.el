;;;; Copyright © 2013-2016, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(require 'multi-term)
(setq multi-term-program "zsh")
(defun term-send-escape ()
  (interactive)
  (term-send-raw-string "\e"))

(defun reset-term-colors ()
  (interactive)
  (set-foreground-color (face-foreground 'default))
  (set-background-color (face-background 'default))
  (custom-set-variables
   '(term-default-bg-color (face-background 'default))
   '(term-default-fg-color (face-foreground 'default))))

;; Limit the buffer for shells.
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 10000)

(when window-system
  (reset-term-colors)
  (custom-set-variables
   '(term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)
                           ("C-p" . previous-line)
                           ("C-n" . next-line)
                           ("C-s" . isearch-forward)
                           ("C-r" . isearch-backward)
                           ("C-m" . term-send-raw)
                           ("M-f" . term-send-forward-word)
                           ("M-b" . term-send-backward-word)
                           ("M-o" . term-send-backspace)
                           ("M-p" . term-send-up)
                           ("M-n" . term-send-down)
                           ("M-M" . term-send-forward-kill-word)
                           ("M-N" . term-send-backward-kill-word)
                           ("M-r" . term-send-reverse-search-history)
                           ("M-," . term-send-input)
                           ("M-." . comint-dynamic-complete)
                           ;; These were defined in term but undefined by
                           ;; multi-term.  I want them back.
                           ("C-c C-j" . term-line-mode)
                           ("C-c C-k" . term-char-mode)
                           ("C-c C-e" . term-send-escape)))))

(defun bash ()
  (interactive)
  (let ((multi-term-program "bash")
        (multi-term-buffer-name "bash"))
    (multi-term)))

(defun python ()
  (interactive)
  (let ((multi-term-program "python")
        (multi-term-buffer-name "python"))
    (multi-term)))

;;; Normally you should just use SLIME.
(defun sbcl ()
  (interactive)
  (let ((multi-term-program "sbcl")
        (multi-term-buffer-name "sbcl"))
    (multi-term)))

(defun tig ()
  (interactive)
  (let ((multi-term-program "tig")
        (multi-term-program-switches "-a")
        (multi-term-buffer-name "tig"))
    (multi-term)))
(global-set-key (kbd "<f2> t") 'tig)

(defun zsh ()
  (interactive)
  (let ((multi-term-program "zsh")
        (multi-term-buffer-name "zsh"))
    (multi-term)))
(global-set-key (kbd "<f2> z") 'zsh)

(defun su ()
  (interactive)
  (let ((multi-term-program "su")
        (multi-term-buffer-name "su")
        (multi-term-program-switches "--login"))
    (multi-term)))
(global-set-key (kbd "<f6> r") 'su)

(defun ssh (ssh-to)
  (interactive "sSSH to: ")
  (let ((multi-term-program "ssh")
        (multi-term-buffer-name ssh-to)
        (multi-term-program-switches ssh-to))
    (multi-term)))
(global-set-key (kbd "<f6> s") 'ssh)

(defun ri (query)
  (interactive "sRI Query: ")
  (let ((multi-term-program "ri")
        (multi-term-buffer-name (concat "ri " query))
        (multi-term-program-switches query))
    (multi-term)))
