(defun cygwin? ()
  "Iff this is Cygwin, basically Unix in Windows."
  (eq system-type 'cygwin))
(defun darwin? ()
  "Iff this is Darwin (OSX)."
  (eq system-type 'darwin))
(defun linux? ()
  "Iff this is Linux."
  (eq system-type 'gnu/linux))
(defun windows? ()
  "Iff this is windows-nt, basically raw Windows."
  (eq system-type 'windows-nt))

(defun abaddon? ()
  "This was my old Camber workstation."
  (string= (system-name) "abaddon"))
(defun corinth? ()
  "This is my personal Samsung RV510 laptop (2012)."
  (string= (system-name) "corinth"))
(defun ezekiel? ()
  "This is my Linode in California."
  (string= (system-name) "ezekiel"))
(defun habakkuk? ()
  "This is my very-long-lived yuge tower at  home."
  (string= (system-name) "habakkuk"))
(defun naaman? ()
  "This is my Linode in Atlanta."
  (string= (system-name) "naaman"))
(defun nephesh? ()
  "This is my personal MacBook Pro 15\" from 2014, formerly my Outpace laptop."
  (string= (system-name) "nephesh"))

(require 's) ; https://github.com/magnars/s.el

(defun dividend-finance-cgore? ()
  "My Dividend Finance macbook."
  (interactive)
  (s-match "^L-SFO-211020-02" (system-name)))
