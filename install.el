(defun on-path? (command)
  "Return T when the COMMAND exists on the path, return NIL if not."
  (= 0 (shell-command (concat "which " command))))

(defun install-brew ()
  (when (not (on-path? "brew"))
    (shell-command "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)\"")))
(install-brew)

(defun brew-tap (tap)
  (shell-command (concat "brew tap " tap)))

(defun brew-install (thing)
  (shell-command (concat "brew install " thing)))

(defun brew-cask-install (thing)
  (shell-command (concat "brew cask install " thing)))
