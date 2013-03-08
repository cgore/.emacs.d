;;;; Copyright (c) 2013, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
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

(add-to-list 'load-path "~/.emacs.d/third-party/")

(setq default-directory "/home/chris")
(set-frame-font "Bitstream Vera Sans Mono")

;;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; three lines at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time

;;; 80-wide for M-q.
(setq-default fill-column 80)

;;; SLIME setup.
(setq inferior-lisp-program "/usr/bin/sbcl") ; I like SBCL, and this is where it lives.
(add-to-list 'load-path "/home/chris/slime/") ; This is my SLIME directory.
(require 'slime-autoloads)
(slime-setup)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(when window-system ; Only if we are in a GUI.
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil
			   :stipple nil
			   :inverse-video nil
			   :box nil
			   :strike-through nil
			   :overline nil
			   :underline nil
			   :slant normal
			   :weight normal
			   :height 98
			   :width normal
			   :foundry "bitstream"
			   :family "Bitstream Vera Sans Mono")))))
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-classic))

;;; Multi-Term
(require 'multi-term)
(setq multi-term-program "zsh")
(custom-set-variables
 '(term-default-bg-color "#2F4F4F")
 '(term-default-fg-color "#FAEBD7"))

(defun bash ()
  (interactive)
  (let ((multi-term-program "bash")
	(multi-term-buffer-name "bash"))
    (multi-term)))

(defun zsh ()
  (interactive)
  (let ((multi-term-program "zsh")
	(multi-term-buffer-name "zsh"))
    (multi-term)))

(defun irb ()
  (interactive)
  (let ((multi-term-program "irb")
	(multi-term-buffer-name "irb"))
    (multi-term)))

(defun python ()
  (interactive)
  (let ((multi-term-program "python")
	(multi-term-buffer-name "python"))
    (multi-term)))

(defun sbcl ()
  (interactive)
  (let ((multi-term-program "sbcl")
	(multi-term-buffer-name "sbcl"))
    (multi-term)))

;;; Ruby stuff
(require 'yari) ; ri interface

;;; ERC: Emacs IRC
(setq erc-log-channels-directory "~/.emacs.d/erc/log/"
      erc-save-buffer-on-part t
      erc-save-queries-on-quit t
      erc-log-write-after-send t
      erc-log-write-after-insert t)
