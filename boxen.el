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

(defun cgore-host? (short-name)
  (or (string= (system-name) short-name)
      (string= (system-name) (concat short-name ".cgore.com"))))

(defun abaddon? ()
  "This was my old Camber workstation."
  (cgore-host? "abaddon"))
(defun alamoth? ()
  "This is my Fedora VM on Habakkuk"
  (cgore-host? "alamoth"))
(defun corinth? ()
  "This is my personal Samsung RV510 laptop (2012)."
  (cgore-host? "corinth"))
(defun ezekiel? ()
  "This is my Linode in California."
  (cgore-host? "ezekiel"))
(defun habakkuk? ()
  "This is my very-long-lived yuge tower at  home."
  (cgore-host? "habakkuk"))
(defun naaman? ()
  "This is my Linode in Atlanta."
  (cgore-host? "naaman"))
(defun nephesh? ()
  "This is my personal MacBook Pro 15\" from 2014, formerly my Outpace laptop."
  (cgore-host? "nephesh"))

(require 's) ; https://github.com/magnars/s.el

(defun dividend-finance-cgore? ()
  "My Dividend Finance macbook."
  (interactive)
  (s-match "^L-SFO-211020-02" (system-name)))
