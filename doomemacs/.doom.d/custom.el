(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "3d47380bf5aa650e7b8e049e7ae54cdada54d0637e7bac39e4cc6afb44e8463b" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" default))
 '(debug-on-error t)
 '(elfeed-feeds '("https://kristofferbalintona.me/index.xml"))
 '(ignored-local-variable-values '((cider-shadow-cljs-default-options . "app")))
 '(inferior-octave-startup-args '("-i" "--line-editing"))
 '(org-agenda-files
   '("~/Dropbox/Org/agenda/inbox.org" "/home/hrothgar32/Dropbox/Org/agenda/personal.org" "/home/hrothgar32/Dropbox/Org/agenda/egyetem.org" "/home/hrothgar32/Documents/Egyetem/AdatbazisII/AdatIIProjekt/.taskok.org"))
 '(safe-local-variable-values
   '((cider-shadow-cljs-default-options . "app")
     (projectile-project-compilation-cmd . "mvn verify")
     (projectile-project-compilation-cmd . "qmake QtFramework/QtFramework.pro && make -j8 -C ./build-QtFramework-Desktop-Debug/")
     (projectile-project-compilation-cmd . "qmake Labor/QtFramework.pro && make -j8 -C ./build-QtFramework-Desktop-Debug/")
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
 '(warning-suppress-log-types '((emacs) (emacs) (emacs) (defvaralias)))
 '(warning-suppress-types '((emacs) (emacs) (emacs) (emacs) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-tags-face ((t (:foreground "#906CFF" :height 1.0)))))
(put 'narrow-to-region 'disabled nil)
