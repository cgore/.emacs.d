;; $ brew install go
;; $ export PATH=$PATH:/usr/local/opt/go/libexec/bin
;; $ export GOPATH=$HOME/go
;; $ export PATH=$PATH:$GOPATH/bin

;; Go REPL mode sits on top of GORE - https://github.com/motemen/gore
;; $ go get -u github.com/motemen/gore
(add-hook 'go-mode-hook #'gorepl-mode)
