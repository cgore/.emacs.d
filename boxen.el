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
(defun tcc-cgore-2015? ()
  "This was my old Climate MacBook Pro 15\" from 2015, now dead."
  (or (string= (system-name) "tcc-cgore")
      (string= (system-name) "tcc-cgore.corp.climate.com")))
(defun tcc-cgore-2018? ()
  "This is my Climate MacBook Pro 15\" from 2018."
  (string= (system-name) "C02VJ80DHTD6"))
(defun tcc-cgore? ()
  "Any TCC box."
  (or (tcc-cgore-2015?)
      (tcc-cgore-2018?)))

(require 's) ; https://github.com/magnars/s.el
(defun gr-macbook-pro? ()
  "True if this is a Guaranteed Rate Macbook Pro."
  (interactive)
  (s-match "^GR-[A-Z0-9]+mbp" (system-name)))
(defun gr-cgore-2018? ()
  "Guaranteed Rate Macbook Pro 15\" 2018."
  (interactive)
  (s-match "^GR-032503mbp" (system-name)))
(defun gr-cgore-2020? ()
  "Guaranteed Rate Macbook Pro 16\" 2020."
  (interactive)
  (s-match "^GR-ZNRC2MD6Nmbp" (system-name)))

(defun gr-cgore? ()
  (or (gr-cgore-2020?)
      (gr-cgore-2018?)))
