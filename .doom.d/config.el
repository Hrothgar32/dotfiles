(defun alm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun alm/connect-to-nextcloud()
  (start-process-shell-command
   "bash" nil "mount ~/nextcloud 1>/dev/null"))

(defun alm/kill-and-close ()
  (interactive)
  "Kill a buffer, and if possible, close it's window."
   (kill-current-buffer)
   (delete-window))

(defun alm/exwm-update-title ()
  (pcase exwm-class-name
    ("Brave-browser" (exwm-workspace-rename-buffer (format "Brave: %s" exwm-title)))))

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'alm/exwm-update-title)

(defun alm/set-chosen-wallpaper(wallpaper-file)
  (start-process-shell-command
   "feh" nil (concat "feh --bg-scale " wallpaper-file)))

(defun alm/set-random-wallpaper()
  (interactive)
  (let* ((pictures (directory-files "~/.lightdm_images" t directory-files-no-dot-files-regexp)))
   (alm/set-chosen-wallpaper (nth (random (length pictures)) pictures))))

(defun alm/choose-wallpaper(wallpaper-file)
  "Choose a wallpaper."
  (interactive (list(read-file-name "Select wallpaper :" "~/.lightdm_images/")))
  (alm/set-chosen-wallpaper wallpaper-file))

(defun alm/set-wallpaper()
  (interactive)
  (start-process-shell-command
   "feh" nil "feh --bg-scale /home/hrothgar32/.lightdm_images/futuristic.jpg"))

(defun alm/lock-screen()
  (interactive)
  (start-process-shell-command)
  "i3lock-fancy" nil "i3lock-fancy")

(defun alm/spotify-toggle()
  (interactive)
  (start-process-shell-command
   "playerctl" nil "playerctl --player=spotify play-pause"))

(defun alm/spotify-previous()
  (interactive)
  (start-process-shell-command
   "playerctl" nil "playerctl --player=spotify previous"))

(defun alm/spotify-next()
  (interactive)
  (start-process-shell-command
   "playerctl" nil "playerctl --player=spotify next"))

(defun alm/exwm-init-hook ()
  (alm/start-panel))

(add-hook 'exwm-update-class-hook #'alm/exwm-update-class)
(add-hook 'exwm-init-hook #'alm/exwm-init-hook)

(map! :leader "d w" 'alm/choose-wallpaper)
(map! :leader "d r" 'alm/set-random-wallpaper)

(use-package exwm
  :config
  (setq exwm-workspace-number 5)


  ;; Key resolution
  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")

  (alm/set-random-wallpaper)
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-enable)
  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\
      ))
;; Adding Space to the exwm-input-prefix
(push ?\x20 exwm-input-prefix-keys)


  (map! :map exwm-mode-map
"C-q" 'exwm-input-send-next-key)

  (setq exwm-input-global-keys
        `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)
        ([?\s-f] . exwm-layout-toggle-fullscreen)

        ;; Move between windows
        ([?\s-h] . windmove-left)
        ([?\s-l] . windmove-right)
        ([?\s-k] . windmove-up)
        ([?\s-j] . windmove-down)
        ([?\s-S] . alm/spotify-toggle)
        ([?\s-A] . alm/spotify-previous)
        ([?\s-D] . alm/spotify-next)
        ([?\s-Q] . alm/kill-and-close)
        ([?\s-X] . alm/lock-screen)


        ;; Launching applications
        ;; ([?\s-d] . (lambda (command)
        ;;         (interactive (list (read-shell-command "$ ")))
        ;;         (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                (number-sequence 0 9))))
  (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)
  (exwm-enable))

(defun alm/kill-panel()
  (interactive)
  (when alm/polybar-process
    (ignore-errors
      (kill-process alm/polybar-process)))
  (setq alm/polybar-process nil)
  )

(defun alm/start-panel()
  (interactive)
  (setq alm/polybar-process (start-process-shell-command "poly" nil "polybar main")))

;; (defun geci ()
;;   (pcase exwm--selected-input-mode
;;     ('line-mode' )
;;     ('char-mode' )
;;     ))

;; (defun alm/send-polybar-mode-hook ()
;;   (setq szam (geci))
;;   (start-process-shell-command "polybar-msg" nil
;;                                "polybar-msg hook exwm-mode 1"))

;; (add-hook 'exwm-input-input-mode-change-hook #'alm/send-polybar-mode-hook)

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("cpp" . "src C++"))

(setq user-full-name  "Almos-Agoston Zediu"
      user-mail-address "zold.almos@gmail.com")

(map! "C-i" 'evil-jump-forward)
(setq-default tab-width 4)

(map! :map makefile-mode-map
      "." 'better-jumper-jump-forward)

(setq package-native-compile t)

(use-package counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(require 'bind-key)
(bind-key* "s-l" 'windmove-right)

(global-hl-line-mode)

(defun alm/reload-emacs-config ()
"It relods my config."
(interactive)
  (load "~/.doom.d/config.el"))

(map! :leader "h r c" 'alm/reload-emacs-config)

(defun alm/visual-fill()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
        display-line-numbers nil)
  (visual-fill-column-mode 1))


(add-hook 'org-mode-hook #'alm/visual-fill)
(add-hook 'dired-mode-hook #'alm/visual-fill)

(defun alm/scale-text ()
  (text-scale-increase 1))

(add-hook 'org-mode-hook #'alm/scale-text)
(add-hook 'dired-mode-hook #'alm/scale-text)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq +workspaces-on-switch-project-behavior nil)

(load-theme 'doom-challenger-deep t)
(set-frame-parameter (selected-frame) 'alpha '(89 . 89))
(add-to-list 'default-frame-alist '(alpha . (89 . 89)))
(defun load-dark-mode ()
  "It loads my dark configuration."
        (interactive)
        (load-theme 'doom-challenger-deep t)
        (set-frame-parameter (selected-frame) 'alpha '(89 . 75))
        (add-to-list 'default-frame-alist '(alpha . (89 . 75))))

(defun load-light-mode ()
  "It loads my light configuration."
        (interactive)
        (load-theme 'doom-gruvbox-light t)
        (set-frame-parameter (selected-frame) 'alpha '(89 . 89))
        (add-to-list 'default-frame-alist '(alpha . (89 . 89))))

(map! :leader "t m d" 'load-dark-mode)
(map! :leader "t m l" 'load-light-mode)

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "rms died for our sins.")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/gnu.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
      (bookmarks . "book"))))
(setq doom-fallback-buffer "*dashboard*")

(defun terminal ()
  "Initialize or toggle terminal emulator
 If the terminal window is visible hide it.
 If a terminal buffer exists, but is not visible, show it.
 If no terminal buffer exists for the current frame create and show it."
  (interactive)
  (multi-vterm-dedicated-toggle)
  (evil-window-decrease-height 18))
(map! :leader "l" #'terminal)

(map! "s-<return>" 'multi-vterm )



(use-package! python-black
  :after python)
(add-hook 'python-mode-hook 'python-black-on-save-mode)
(add-hook 'python-mode-hook #'lsp) ; or lsp-deferred
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'js2-mode-hook 'lsp)

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup)

(require 'treemacs)
(map! :leader "x" 'treemacs)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")
        ("django"   . "\\.html\\."))
)
(setq web-mode-enable-engine-detection t)

(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-directory "~/org/")
(setq org-hide-block-startup t)
(with-eval-after-load 'ox
  (require 'ox-hugo))
(setq org-priority-faces '((65: foreground-color "#660000")
                           (66: foreground-color "#99FFFF")
                           (67: foreground-color "#009150")))
(use-package! org-fancy-priorities
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(defun alm/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.doom.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook  #'alm/org-babel-tangle-config)))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs "~/.doom.d/snippets/emacs-lisp-mode")
  (add-to-list 'yas-snippet-dirs "~/.doom.d/snippets/emacs-lisp-mode/cmake-mode")
  (yas-global-mode 1))

(defun create-cmake-root(cmake-project-dir-string)
  (let* ((cmake-file-string (concat cmake-project-dir-string "/CMakeLists.txt"))
         (main-cpp-string (concat cmake-project-dir-string "/main.cpp"))
         (build-folder-string (concat cmake-project-dir-string "/build"))
         (debug-folder-string (concat build-folder-string "/Debug"))
         (release-folder-string (concat build-folder-string "/Release")))
        (dired-create-directory build-folder-string)
        (dired-create-directory debug-folder-string)
        (dired-create-directory release-folder-string)
        (with-temp-buffer
        (cmake-mode)
        (yas-minor-mode)
        (yas-expand-snippet (yas-lookup-snippet "cmake_project" 'cmake-mode))
        (when (file-writable-p cmake-file-string)
        (write-region (point-min)
                        (point-max)
                        cmake-file-string))
        (delete-region (point-min)
                       (point-max))
        (cpp-mode)
        (yas-minor-mode)
        (yas-expand-snippet (yas-lookup-snippet "main_cpp" 'cpp-mode))
        (when (file-writable-p main-cpp-string)
        (write-region (point-min)
                        (point-max)
                        main-cpp-string)))
    ))

(defun create-cmake-project (project-root string)
  "Creates a new C++ CMake project"
  (interactive (list (read-directory-name "Select project root: ")
                     (read-string "Name of the project: ")))
  (setq cmake-project-name string)
  (let* ((cmake-project-dir-string (concat project-root string)))
                                  (dired-create-directory cmake-project-dir-string)
                                  (create-cmake-root cmake-project-dir-string)
                                  (projectile-add-known-project cmake-project-dir-string)))

(defun build-cmake-project(mode)
  "Builds a CMake project."
  (let* ((release-mode-string "cmake -S . -B build/ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Release && cmake --build build/ && ln -fs build/compile_commands.json")
         (debug-mode-string "cmake -S . -B build/ -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug && cmake --build build/ && ln -fs build/compile_commands.json"))
    (if (equal mode "Debug")
        (message "%s" (concat "Debug mode:\n" (shell-command-to-string debug-mode-string)))
        (message "%s" (concat "Release mode:\n" (shell-command-to-string release-mode-string)))
    )))

(defun build-cmake-project-debug()
  "Builds a CMake project in Debug mode."
  (interactive)
  (build-cmake-project "Debug"))

(defun build-cmake-project-release()
  "Builds a CMake project in Release mode."
  (interactive)
  (build-cmake-project "Release")
  )

(defun run-cmake-project (mode args)
  "Run the CMake project."
  (let* ((status-code-string "; echo \"Process exited with status code: $?\"")
         (release-mode-string (concat "time ./build/Release/" (+workspace-current-name) " " args status-code-string))
         (debug-mode-string (concat "time ./build/Debug/" (+workspace-current-name) " " args status-code-string)))
    (if (equal mode "Debug")
        (message "%s" (concat "Debug mode:\n" (shell-command-to-string debug-mode-string)))
      (message "%s" (concat "Release mode:\n" (shell-command-to-string release-mode-string))))
  ))

(defun run-cmake-project-debug ()
  "Run project in Debug mode."
  (interactive)
  (let* ((args (read-string "Give arguments, if any: ")))
    (run-cmake-project "Debug" args))
  )

(defun run-cmake-project-release ()
  "Run project in Release mode."
  (interactive)
  (let* ((args (read-string "Give arguments, if any: ")))
    (run-cmake-project "Release" args))
  )

(map! :leader :desc "Create a CMake project" "m p" #'create-cmake-project)
(map! :leader :desc "Build CMake project in Release mode." "m r" #'build-cmake-project-release)
(map! :leader :desc "Build CMake project in Debug mode." "m z" #'build-cmake-project-debug)
(map! :leader :desc "Run CMake project in Debug mode." "m Z" #'run-cmake-project-debug)
(map! :leader :desc "Run CMake project in Release mode." "m R" #'run-cmake-project-release)

(require 'dap-lldb)

(use-package mu4e
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-root-maildir "~/Mail")

  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")
  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"    :key ?i)
        (:maildir "/[Gmail]/Sent Mail" :key ?s)
        (:maildir "/[Gmail]/Trash"     :key ?t)
        (:maildir "/[Gmail]/Drafts"    :key ?d)
        (:maildir "/[Gmail]/All Mail"  :key ?a)))
  (setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-compose-signature "Almos Zediu")
)

(use-package org-mime
  :ensure t)
(map! :leader "o m" 'mu4e)

;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
;; (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

(use-package dired-hide-details
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; (add-to-list 'dired-compress-files-alist '("\\.gz\\'" . "tar $o -r --filesync $i"))

  (defun dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	  (progn
	    (set (make-local-variable 'dired-dotfiles-show-p) nil)
	    (message "h")
	    (dired-mark-files-regexp "^\\\.")
	    (dired-do-kill-lines))
	(progn (revert-buffer) ; otherwise just revert to re-show
	       (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(use-package! org-super-agenda
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '(
          (:name "Today"
           :date today
           :time-grid t
           :todo "TODAY")
          (:name "Important"
           :tag "számla"
           :priority "A")
          (:not)))
  )
  :after
  (setq org-super-agenda-header-map nil)

(setq org-agenda-files '("~/Org/agenda.org"
                         "~/Org/orarend.org"
                         "~/Org/projektek"))
(setq org-todo-keywords-for-agenda
      (quote ((sequence "TODO(t)" "NEXT(p)" "WAIT(w)" "CANCELLED" "DONE(r)")
              (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(p)" "WAIT(w)" "CANCELLED" "DONE(r)")
              (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-capture-templates
      (quote
            (("t" "Personal todo" entry
            (file+headline "~/Org/agenda.org" "Random napi feladatok")
            "* TODO %?\nSCHEDULED: <%(org-read-date)>")
            ("p" "Templates for projects")
            ("pt" "Project-local todo" entry
            (file+headline +org-capture-project-todo-file "Inbox")
            "* TODO %?\n%i\n%a" :prepend t)
            ("pn" "Project-local notes" entry
            (file+headline +org-capture-project-notes-file "Inbox")
            "* %U %?\n%i\n%a" :prepend t)
            ("pc" "Project-local changelog" entry
            (file+headline +org-capture-project-changelog-file "Unreleased")
            "* %U %?\n%i\n%a" :prepend t)
            ("o" "Centralized templates for projects")
            ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
            ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
            ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))
       ))

(defun alm/build-and-deploy-blog()
  "Builds and deploys my blog."
  (interactive)
  (let* ((build-string "hugo && rsync -avz --delete public/ almer:/var/www/html/almos-blog/public"))
         (message "%s" (shell-command-to-string build-string))))
(map! :leader :desc "Deploy the blog." "d b" #'alm/build-and-deploy-blog)
