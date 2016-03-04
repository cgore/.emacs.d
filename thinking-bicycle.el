;;;; Copyright © 2013 - 2016, Christopher Mark Gore,
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


;;; ThinkingBicycle.com stuff.

(defun thinking-bicycle ()
  (interactive)
  (browse-url "http://thinkingbicycle.com"))
(defun thinking-bicycle-bookmarks ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/bookmarks"))
(defun thinking-bicycle-folders ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/folders"))
(defun thinking-bicycle-recent-notes ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/recent_notes"))
(defun thinking-bicycle-read-laters ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/read_laters"))
(defun thinking-bicycle-received-shares ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/received_shares"))
(defun thinking-bicycle-my-posts ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/posts"))
(defun thinking-bicycle-my-posts-replies ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/users/posts_replies"))
(defun thinking-bicycle-search ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/keywords/search"))
(defun thinking-bicycle-advanced-search ()
  (interactive)
  (browse-url "http://thinkingbicycle.com/keywords/advanced_search"))
(defun thinking-bicycle-add-uri ()
  (interactive)
  (when (string= major-mode "w3m-mode")
    (w3m-browse-url (concat "http://thinkingbicycle.com/links/bookmarklet"
                            "?version=3"
                            "&uri="   w3m-current-url
                            "&title=" w3m-current-title))))

(global-set-key (kbd "<f7> <f7>") 'thinking-bicycle)
(global-set-key (kbd "<f7> b")    'thinking-bicycle-bookmarks)
(global-set-key (kbd "<f7> f")    'thinking-bicycle-folders)
(global-set-key (kbd "<f7> f")    'thinking-bicycle-recent-notes)
(global-set-key (kbd "<f7> l")    'thinking-bicycle-read-laters)
(global-set-key (kbd "<f7> p")    'thinking-bicycle-my-posts)
(global-set-key (kbd "<f7> P")    'thinking-bicycle-my-posts-replies)
(global-set-key (kbd "<f7> s")    'thinking-bicycle-received-shares)
(global-set-key (kbd "<f7> S")    'thinking-bicycle-search)
(global-set-key (kbd "<f7> +")    'thinking-bicycle-add-uri)
