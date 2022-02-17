(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq lsp-log-io nil)

(defun alm/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun alm/everywhere()
  (interactive)
  (start-process-shell-command
   "bash" nil "doom nowhere"))

(defun alm/connect-to-nextcloud()
  (start-process-shell-command
   "bash" nil "mount ~/nextcloud 1>/dev/null"))

(defun alm/reboot()
  (interactive)
  (start-process-shell-command
   "bash" nil "reboot"))

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
  (start-process-shell-command
  "i3lock-fancy" "*i3lock*" "i3lock-fancy -n"))

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
  (alm/start-panel)
  (alm/set-chosen-wallpaper "~/.lightdm_images/vhs_neon.png"))

(add-hook 'exwm-update-class-hook #'alm/exwm-update-class)
(add-hook 'exwm-init-hook #'alm/exwm-init-hook)

(defun alm/make-screenshot(picture-dir picture-name)
  (interactive (list (read-directory-name "Select image directory: ")
                     (read-string "Image name: ")))
  (let* ((image-absolute-path (concat picture-dir picture-name))
         (shell-string (concat "scrot -s -e 'mv $f " image-absolute-path "'")))
    (start-process-shell-command
     "scrot" nil shell-string)))

(map! :leader "d w" 'alm/choose-wallpaper)
(map! :leader "d r" 'alm/set-random-wallpaper)
(map! :leader "r r" 'alm/reboot)

;; (use-package exwm
;;   :init
;;   (setq exwm-workspace-number 5
;;         mouse-autoselect-windiw nil
;;         focus-follows-mouse t
;;         exwm-workspace-warp-cursor t)
;;   :config
;;   ;; Key resolution
;;   (require 'exwm-randr)
;;   (exwm-randr-enable)
;;   (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")
;;   ;; (require 'exwm-systemtray)
;;   ;; (setq exwm-systemtray-height 32)
;;   ;; (exwm-systemtray-enable)
;;   ;; Automatically send the mouse cursor to the selected workspace's display
;;   (setq exwm-workspace-warp-cursor t)

;; ;; These keys should always pass through to Emacs
;; (setq exwm-input-prefix-keys
;;     '(?\C-x
;;       ?\C-u
;;       ?\C-h
;;       ?\M-x
;;       ?\M-`
;;       ?\M-&
;;       ?\M-:
;;       ?\C-\M-j  ;; Buffer list
;;       ?\C-\
;;       ))
;; ;; Adding Space to the exwm-input-prefix
;; (push ?\x20 exwm-input-prefix-keys)


;;   (map! :map exwm-mode-map
;; "C-q" 'exwm-input-send-next-key)

;;   (setq exwm-input-global-keys
;;         `(
;;         ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
;;         ([?\s-r] . exwm-reset)
;;         ([?\s-f] . exwm-layout-toggle-fullscreen)

;;         ;; Move between windows
;;         ([?\s-h] . windmove-left)
;;         ([?\s-l] . windmove-right)
;;         ([?\s-k] . windmove-up)
;;         ([?\s-j] . windmove-down)
;;         ([?\s-S] . alm/spotify-toggle)
;;         ([?\s-A] . alm/spotify-previous)
;;         ([?\s-D] . alm/spotify-next)
;;         ([?\s-Q] . alm/kill-and-close)
;;         ([?\s-X] . alm/lock-screen)
;;         ([?\s-C] . alm/make-screenshot)
;;         ([?\s-e] . alm/everywhere)


;;         ;; Launching applications
;;         ;; ([?\s-d] . (lambda (command)
;;         ;;         (interactive (list (read-shell-command "$ ")))
;;         ;;         (start-process-shell-command command nil command)))

;;         ;; Switch workspace
;;         ([?\s-w] . exwm-workspace-switch)

;;         ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
;;         ,@(mapcar (lambda (i)
;;                 `(,(kbd (format "s-%d" i)) .
;;                         (lambda ()
;;                         (interactive)
;;                         (exwm-workspace-switch-create ,i))))
;;                 (number-sequence 0 9))))
;;   (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)
;;   (alm/connect-to-nextcloud))

      (defun alm/rg-in-dir(initial-dir)
        (interactive (read-directory-name "Select directory for search:"))
        (counsel-rg "" initial-dir))
    (map! :leader "]" 'alm/rg-in-dir)

(defun alm/kill-panel()
  (interactive)
  (when alm/polybar-process
    (ignore-errors
      (kill-process alm/polybar-process)))
  (setq alm/polybar-process nil)
  )

(defun alm/start-panel()
  (interactive)
  (start-process-shell-command "python" nil "python3 ~/.config/set_desktop_name.py")
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
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))

(defun alm/set-dictionary-to-hungarian ()
  (interactive)
  (flyspell-mode-off)
  (ispell-change-dictionary "hu_HU")
  (flyspell-mode))

(map! :leader "d h" 'alm/set-dictionary-to-hungarian)

(defun alm/set-dictionary-to-english()
  (interactive)
  (flyspell-mode-off)
  (ispell-change-dictionary "en_US")
  (flyspell-mode))

(map! :leader "d e" 'alm/set-dictionary-to-english)

(setq user-full-name "Álmos-Ágoston Zediu")

(map! "C-i" 'evil-jump-forward)
(setq-default tab-width 4)

(map! :map makefile-mode-map
      "." 'better-jumper-jump-forward)
(map! :leader "[" 'counsel-fzf)
(map! :leader "=" 'counsel-rg)

(setq package-native-compile t)

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

(add-hook 'dired-mode-hook #'alm/visual-fill)

(defun alm/scale-text ()
  (text-scale-increase 1))

(add-hook 'dired-mode-hook #'alm/scale-text)

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq +workspaces-on-switch-project-behavior t)

(defun load-dark-mode ()
  "It loads my dark configuration."
        (interactive)
        (load-theme 'doom-challenger-deep t)
        (add-to-list 'default-frame-alist '(alpha . (89 . 75))))

(defun load-light-mode ()
  "It loads my light configuration."
        (interactive)
        (load-theme 'spacemacs-light t))

(defun alm/disable-transparency ()
  (interactive)
  "It disables transparency."
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
)

(defun alm/enable-transparency ()
  (interactive)
  "It enables transparency"
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(89 . 75))
)

(map! :leader "t m d" 'load-dark-mode)
(map! :leader "t m l" 'load-light-mode)
(map! :leader "t t e" 'alm/enable-transparency)
(map! :leader "t t d" 'alm/disable-transparency)

(setq dashboard-startup-banner "~/dotfiles/gnu.png")
(load-dark-mode)

(use-package dashboard
  :custom
  (dashboard-items '((recents . 4)
                     (projects . 4)
                     (agenda . 4)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

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

(add-hook 'js2-mode-hook 'lsp)

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
;; (require 'slime)
;; (slime-setup)

(require 'treemacs)
(map! :leader "x" 'treemacs)

(defun alm/web-mode-hook ()
 (setq web-mode-code-indent-offset 2))

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
(setq web-mode-code-indent-offset 2)
(add-hook 'web-mode-hook 'alm/web-mode-hook)

(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'menu-bar--display-line-numbers-mode-none)
(setq org-directory "~/Org/")
(setq org-hide-block-startup t)
(setq org-bullets-bullet-list '(" "))
(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(with-eval-after-load 'ox
  (require 'ox-hugo))
(setq org-priority-faces '((65: foreground-color "#660000")
                           (66: foreground-color "#99FFFF")
                           (67: foreground-color "#009150")))
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  )
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(defun alm/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/dotfiles/doomemacs/.doom.d/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook  #'alm/org-babel-tangle-config)))

(use-package! org-roam
  :custom
  (org-roam-directory "/home/hrothgar32/Documents/Projects/braindump/RoamNotes")
  (org-roam-dailies-directory "daily/")
  (org-roam-capture-templates
    '(("d" "default" plain "%?" :target
    (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
      ("r" "bibliography reference" plain
         (file "~/Documents/Projects/braindump/RoamNotes/templates/noter.org")
         :target
         (file+head "references/${citekey}.org" "#+title: ${title}\n")))
   )
  (org-roam-dailies-capture-templates
        '(("d" "default" entry
        "* %?"
        :target (file+head "%<%Y-%m-%d>.org"
                                "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-complete-everywhere t))

(when (daemonp)
        (add-to-list 'org-roam-buffer-postrender-functions (lambda () (org--latex-preview-region (point-min) (point-max))) t)
        (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
        )

(defun benmezger/org-roam-export-all ()
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (dolist (f (org-roam-list-files))
    (with-current-buffer (find-file f)
        (org-hugo-export-wim-to-md)
      )))

(require 'find-lisp)
(defun alm/publish (file)
  (with-current-buffer (find-file-noselect file)
    (setq org-hugo-base-dir "/home/hrothgar32/Documents/Projects/braindump")
    (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
      (org-hugo-export-wim-to-md))))

(defun org-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (when (bobp) (user-error "Already a top-level node"))
    (org-id-get-create)
    (save-buffer)
    (org-roam-db-update-file)
    (let* ((template-info nil)
           (node (org-roam-node-at-point))
           (template (org-roam-format-template
                      (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                      (lambda (key default-val)
                        (let ((fn (intern key))
                              (node-fn (intern (concat "org-roam-node-" key)))
                              (ksym (intern (concat ":" key))))
                          (cond
                           ((fboundp fn)
                            (funcall fn node))
                           ((fboundp node-fn)
                            (funcall node-fn node))
                           (t (let ((r (completing-read (format "%s: " key) nil nil nil default-val)))
                                (plist-put template-info ksym r)
                                r)))))))
           (file-path (read-file-name "Extract node to: " org-roam-directory template nil template)))
      (when (file-exists-p file-path)
        (user-error "%s exists. Aborting" file-path))
      (org-cut-subtree)
      (save-buffer)
      (with-current-buffer (find-file-noselect file-path)
        (org-paste-subtree)
        (save-buffer)
        (org-roam-promote-entire-buffer)
        (save-buffer)))))

(use-package! deft
  :custom
  (deft-directory "~/RoamNotes"))

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

(setq lsp-java-autobuild-enabled nil)
(defun lsp-java--completing-read-multiple (message items initial-selection)
    (if (functionp 'ivy-read)
        (let (result)
          (ivy-read message (mapcar #'car items)
                    :action (lambda (c) (setq result (list (cdr (assoc c items)))))
                    :multi-action
                    (lambda (canditates)
                      (setq result (mapcar (lambda (c) (cdr (assoc c items))) canditates))))
          result)
      (let ((deps initial-selection) dep)
        (while (setq dep (cl-rest (lsp--completing-read
                                   (if deps
                                       (format "%s (selected %s): " message (length deps))
                                     (concat message ": "))
                                   items
                                   (-lambda ((name . id))
                                     (if (-contains? deps id)
                                         (concat name " ✓")
                                       name)))))
          (if (-contains? deps dep)
              (setq deps (remove dep deps))
            (cl-pushnew dep deps)))
        deps)))
(map! :map ivy-mode-map "C-p" 'ivy-mark)
(map! :map ivy-mode-map "C-u p" 'ivy-unmark)

(require 'dap-cpptools)

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

(setq org-todo-keywords-for-agenda
      (quote ((sequence "TODO(t)" "NEXT(p)" "WAIT(w)" "CANCELLED" "DONE(r)")
              (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(p)" "WAIT(w)" "CANCELLED" "DONE(r)")
              (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))
(setq org-agenda-files '("/home/hrothgar32/Org/agenda.org"))

(setq org-capture-templates
      (quote
            (("t" "Personal todo" entry
            (file+headline "~/Org/agenda.org" "Taskok")
            "* TODO %?\nSCHEDULED: <%(org-read-date)>")
            )
       ))

(defun alm/build-and-deploy-blog()
  "Builds and deploys my blog."
  (interactive)
  (let* ((build-string "hugo && rsync -avz --delete public/ almer:/var/www/html/almos-blog/public"))
         (message "%s" (shell-command-to-string build-string))))
(map! :leader :desc "Deploy the blog." "d b" #'alm/build-and-deploy-blog)

(defun alm/build-and-deploy-diskette()
  "Builds and deploys my diskette."
  (interactive)
  (let* ((build-string "hugo && rsync -avz --delete public/ second:/var/www/html/diskette-archives/public"))
         (message "%s" (shell-command-to-string build-string))))
(map! :leader :desc "Deploy the site." "d d" #'alm/build-and-deploy-diskette)

(use-package mathpix.el
  :custom ((mathpix-app-id "zold_almos_gmail_com_673916_1f69c5")
           (mathpix-app-key "cab0eeec91a7c89af9a62a0cf31b1f5465c985b92b29035c8508cda789ff79d6"))
  :bind
  ("C-x m" . mathpix-screenshot))

;; jaja
(setq +latex-viewers '(zathura))

(map! :map cdlatex-mode-map
    :i "TAB" #'cdlatex-tab)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-hook 'after-save-hook (lambda () (TeX-command "LatexMk" #'TeX-master-file)) nil t)))

(map!
  :map LaTeX-mode-map
  :nv
  "z a" 'outline-toggle-children)

(setq! bibtex-completion-bibliography '("/home/hrothgar32/Documents/Projects/braindump/RoamNotes/testLib.bib"))
(setq! org-cite-global-bibliography '("/home/hrothgar32/Documents/Projects/braindump/RoamNotes/testLib.bib"))
(setq org-cite-csl-styles-dir "~/Zotero/styles")
(use-package! org-roam-bibtex
  :after org-roam
  :custom
  (orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))
  (orb-roam-ref-format 'org-cite)
  :config
  (org-roam-bibtex-mode))

(set-email-account! "gmail"
  '((mu4e-sent-folder       . "/gmail/[Gmail]/Sent Mail")
    (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
    (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
    (smtpmail-smtp-user     . "zold.almos@gmail.com")
    (user-mail-address     .  "zold.almos@gmail.com")
    (mu4e-compose-signature . "---\n Almos Zediu")
    )
  t)
(set-email-account! "sasmail"
  '((mu4e-sent-folder       . "/gmail2/[Gmail]/Sent Mail")
    (mu4e-drafts-folder     . "/gmail2/[Gmail]/Drafts")
    (mu4e-trash-folder      . "/gmail2/[Gmail]/Trash")
    (smtpmail-smtp-user     . "sasokcsapat@gmail.com")
    (user-mail-address     .  "sasokcsapat@gmail.com")
    (mu4e-compose-signature . "---\n Almos Zediu")
    )
  t)
(set-email-account! "ubboutlook"
  '((mu4e-sent-folder       . "/ubboutlook/Sent Items")
    (mu4e-drafts-folder     . "/ubboutlook/Drafts")
    (mu4e-trash-folder      . "/ubboutlook/Deleted Items")
    (smtpmail-smtp-user     . "almos.zediu@stud.ubbcluj.ro")
    (user-mail-address      . "almos.zediu@stud.ubbcluj.ro")
    (mu4e-compose-signature . "---\n Almos Zediu")
    )
  t)

;; (setq +mu4e-gmail-accounts '(("zold.almos@gmail.com" . "zold.almos")))

(setq mu4e-context-policy 'ask-if-none
      mu4e-compose-context-policy 'always-ask)

(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))
