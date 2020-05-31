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

(when (darwin?)
    (brew-install "git")
  ;; echo 'export PATH="/usr/local/opt/gettext/bin:$PATH"' >> ~/.zshrc
  ;; export LDFLAGS="-L/usr/local/opt/gettext/lib"
  ;; export CPPFLAGS="-I/usr/local/opt/gettext/include"

  (brew-tap "homebrew/cask-fonts")
  (brew-cask-install "font-fira-code")

  (brew-install "bash")

  (brew-install "zsh")
  ;; echo 'export PATH="/usr/local/opt/ncurses/bin:$PATH"' >> ~/.zshrc
  ;; export LDFLAGS="-L/usr/local/opt/ncurses/lib"
  ;; export CPPFLAGS="-I/usr/local/opt/ncurses/include"

  (brew-install "awscli")

  (brew-install "ack")


  (brew-install "vault")
  (brew-install "python-yq")
  (brew-install "leiningen")

  ;; sudo ln -sfn /usr/local/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
  ;; echo 'export PATH="/usr/local/opt/openjdk/bin:$PATH"' >> ~/.zshrc
  ;; export CPPFLAGS="-I/usr/local/opt/openjdk/include"
  (brew-install "openjdk")

  ;; https://github.com/Homebrew/homebrew-core/issues/50536
  ;; https://clojure.org/news/2020/02/28/clojure-tap
  (brew-install "clojure/tools/clojure"))
