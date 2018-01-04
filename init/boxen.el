;;;; Copyright © 2013-2018, Christopher Mark Gore,
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


(defun cygwin? ()
  "Iff this is Cygwin, basically Unix in Windows."
  (eq system-type 'cygwin))
(defun darwin? ()
  "Iff this is Darwin (OSX)."
  (eq system-type 'darwin))
(defun linux? ()
  "Iff this is Linux."
  (eq system-type 'gnu/linux))

(defun abaddon? ()
  "This was my old Camber workstation."
  (string= system-name "abaddon"))
(defun corinth? ()
  "This is my personal Samsung RV510 laptop (2012)."
  (string= system-name "corinth"))
(defun ezekiel? ()
  "This is my Linode in California."
  (string= system-name "ezekiel"))
(defun habakkuk? ()
  "This is my very-long-lived yuge tower at  home."
  (string= system-name "habakkuk"))
(defun naaman? ()
  "This is my Linode in Atlanta."
  (string= system-name "naaman"))
(defun nephesh? ()
  "This is my personal MacBook Pro 15\" from 2014, formerly my Outpace laptop."
  (string= system-name "nephesh"))
(defun tcc-cgore-2015? ()
  "This was my old Climate MacBook Pro 15\" from 2015, now dead."
  (or (string= system-name "tcc-cgore")
      (string= system-name "tcc-cgore.corp.climate.com")))
(defun tcc-cgore-2018? ()
  "This is my Climate MacBook Pro 15\" from 2018."
  (string= system-name "C02VJ80DHTD6"))
(defun tcc-cgore? ()
  "Any TCC box."
  (or (tcc-cgore-2015?)
      (tcc-cgore-2018?)))
