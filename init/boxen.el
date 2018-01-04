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
  (eq system-type 'cygwin))
(defun darwin? ()
  (eq system-type 'darwin))
(defun linux? ()
  (eq system-type 'gnu/linux))

(defun abaddon? () ; Old Camber workstation
  (string= system-name "abaddon"))
(defun corinth? () ; Samsung RV510 laptop (2012)
  (string= system-name "corinth"))
(defun ezekiel? () ; California
  (string= system-name "ezekiel"))
(defun habakkuk? () ; home
  (string= system-name "habakkuk"))
(defun naaman? () ; Atlanta
  (string= system-name "naaman"))
(defun nephesh? () ; MacBook Pro 15" (2014)
  (string= system-name "nephesh"))
(defun tcc-cgore-2015? () ; Climate MacBook Pro 15" (2015)
  (or (string= system-name "tcc-cgore")
      (string= system-name "tcc-cgore.corp.climate.com")))
(defun tcc-cgore-2018? () ; Climate MacBook Pro 15" (2018)
  (string= system-name "C02VJ80DHTD6"))
(defun tcc-cgore? ()
  (or (tcc-cgore-2015?)
      (tcc-cgore-2018?)))
