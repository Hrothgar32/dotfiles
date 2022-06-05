(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" default))
 '(debug-on-error t)
 '(org-agenda-files
   '("~/Org/Disco Elysium.org" "/home/hrothgar32/Documents/Egyetem/AdatbazisII/AdatIIProjekt/.taskok.org" "/home/hrothgar32/Org/agenda.org"))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "qmake Labor/QtFramework.pro && make -j8 -C ./build-QtFramework-Desktop-Debug/")
     (projectile-project-run-cmd . "cd ../build-CAGD-moenish-Desktop-Debug/ && ./CAGD-moenish")
     (projectile-project-compilation-cmd . "qmake CAGD-moenish.pro && make -j8 -C ../build-CAGD-moenish-Desktop-Debug/")
     (projectile-project-run-cmd . "cd ../build_folder/ && ./QtFramework")
     (projectile-project-compilation-cmd . "qmake QtFramework.pro && make -j8 -C ../build_folder/")
     (eval progn
           (defun my-project-specific-function nil))
     (projectile-project-run-cmd . "cd ../build-QtFramework-Desktop-Debug/ && ./QtFramework")
     (projectile-project-compilation-cmd . "qmake QtFramework.pro && make -j8 -C ../build-QtFramework-Desktop-Debug/")
     (secret-ftp-password . "secret")))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(warning-suppress-log-types '((after-save-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-tags-face ((t (:foreground "#906CFF" :height 1.0)))))
(put 'narrow-to-region 'disabled nil)
