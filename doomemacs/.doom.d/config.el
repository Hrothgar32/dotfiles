(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq lsp-log-io nil)

;; (defun alm/exwm-update-class ()
;;   (exwm-workspace-rename-buffer exwm-class-name))

;; (defun alm/everywhere()
;;   (interactive)
;;   (start-process-shell-command
;;    "bash" nil "doom nowhere"))

;; (defun alm/connect-to-nextcloud()
;;   (start-process-shell-command
;;    "bash" nil "mount ~/nextcloud 1>/dev/null"))

;; (defun alm/reboot()
;;   (interactive)
;;   (start-process-shell-command
;;    "bash" nil "reboot"))

;; (defun alm/kill-and-close ()
;;   (interactive)
;;   "Kill a buffer, and if possible, close it's window."
;;    (kill-current-buffer)
;;    (delete-window))

;; (defun alm/exwm-update-title ()
;;   (pcase exwm-class-name
;;     ("Brave-browser" (exwm-workspace-rename-buffer (format "Brave: %s" exwm-title)))))

;;   ;; When window title updates, use it to set the buffer name
;;   (add-hook 'exwm-update-title-hook #'alm/exwm-update-title)

;; (defun alm/set-chosen-wallpaper(wallpaper-file)
;;   (start-process-shell-command
;;    "feh" nil (concat "feh --bg-scale " wallpaper-file)))

;; (defun alm/set-random-wallpaper()
;;   (interactive)
;;   (let* ((pictures (directory-files "~/.lightdm_images" t directory-files-no-dot-files-regexp)))
;;    (alm/set-chosen-wallpaper (nth (random (length pictures)) pictures))))

;; (defun alm/choose-wallpaper(wallpaper-file)
;;   "Choose a wallpaper."
;;   (interactive (list(read-file-name "Select wallpaper :" "~/.lightdm_images/")))
;;   (alm/set-chosen-wallpaper wallpaper-file))

;; (defun alm/set-wallpaper()
;;   (interactive)
;;   (start-process-shell-command
;;    "feh" nil "feh --bg-scale /home/hrothgar32/.lightdm_images/futuristic.jpg"))

;; (defun alm/lock-screen()
;;   (interactive)
;;   (start-process-shell-command
;;   "i3lock-fancy" "*i3lock*" "i3lock-fancy -n"))

;; (defun alm/spotify-toggle()
;;   (interactive)
;;   (start-process-shell-command
;;    "playerctl" nil "playerctl --player=spotify play-pause"))

;; (defun alm/spotify-previous()
;;   (interactive)
;;   (start-process-shell-command
;;    "playerctl" nil "playerctl --player=spotify previous"))

;; (defun alm/spotify-next()
;;   (interactive)
;;   (start-process-shell-command
;;    "playerctl" nil "playerctl --player=spotify next"))

;; (defun alm/exwm-init-hook ()
;;   (alm/start-panel)
;;   (alm/set-chosen-wallpaper "~/.lightdm_images/vhs_neon.png"))

;; (add-hook 'exwm-update-class-hook #'alm/exwm-update-class)
;; (add-hook 'exwm-init-hook #'alm/exwm-init-hook)

;; (defun alm/make-screenshot(picture-dir picture-name)
;;   (interactive (list (read-directory-name "Select image directory: ")
;;                      (read-string "Image name: ")))
;;   (let* ((image-absolute-path (concat picture-dir picture-name))
;;          (shell-string (concat "scrot -s -e 'mv $f " image-absolute-path "'")))
;;     (start-process-shell-command
;;      "scrot" nil shell-string)))

;; (map! :leader "d w" 'alm/choose-wallpaper)
;; (map! :leader "d r" 'alm/set-random-wallpaper)
;; (map! :leader "r r" 'alm/reboot)
(map! :nv "g z e" 'mc/mark-more-like-this-extended)

      (defun alm/rg-in-dir(initial-dir)
        (interactive (read-directory-name "Select directory for search:"))
        (counsel-rg "" initial-dir))
    (map! :leader "]" 'alm/rg-in-dir)

(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "templates/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )

;; (use-package desktop-environment
;;   :after exwm
;;   :config (desktop-environment-mode)
;;   :custom
;;   (desktop-environment-brightness-small-increment "2%+")
;;   (desktop-environment-brightness-small-decrement "2%-")
;;   (desktop-environment-brightness-normal-increment "5%+")
;;   (desktop-environment-brightness-normal-decrement "5%-"))

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
(use-package! counsel
  :defer t
  :init
  (define-key!
    [remap projectile-compile-project] #'projectile-compile-project))

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
        (load-theme 'doom-gruvbox t))

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

(set-popup-rule! "^/*vterminal*/*$")
;; (defun terminal ()
;; "Initialize or toggle terminal emulator
;; If the terminal window is visible hide it.
;; If a terminal buffer exists, but is not visible, show it.
;; If no terminal buffer exists for the current frame create and show it."
;; (interactive)
;; (multi-vterm-dedicated-toggle)
;; (evil-window-decrease-height 18))
(map! :leader "j" #'multi-vterm-next)
(map! :leader "k" #'multi-vterm-prev)

(setq projectile-auto-discover nil)

(use-package dap-mode
  :custom
  (dap-auto-configure-features '(locals controls))
  (dap-auto-show-output t))

(map! :leader "c h" 'dap-hydra)

(use-package! python-black
  :after python)
(add-hook 'python-mode-hook 'python-black-on-save-mode)
(add-hook 'python-mode-hook #'lsp) ; or lsp-deferred
(require 'dap-python)
(setq dap-python-debugger 'debugpy)

(add-hook 'js2-mode-hook 'lsp)
(require 'dap-node)

(use-package! paredit
  :after clojure-mode)
(use-package! cider
  :after clojure-mode
  :config
  (set-lookup-handlers! 'cider-mode nil)
  )

(setq cider-merge-sessions 'project)

(add-hook 'clojure-mode-hook 'paredit-mode)

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil))

(require 'clj-deps-new)
(setq cider-edit-jack-in-command t)

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
(add-hook 'org-mode-hook 'writeroom-mode)

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
  (org-roam-dailies-directory "./daily")
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

(setq org-fold-core-style 'overlays)

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

(use-package dired-hide-details
  :hook (dired-mode . dired-hide-details-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
(setq org-agenda-files '("/home/hrothgar32/Dropbox/Org/agenda.org" "/home/hrothgar32/Dropbox/Org/agenda/"))

(setq org-capture-templates
      (quote
            (("t" "Personal todo" entry
            (file "~/Dropbox/Org/agenda/personal.org")
            "* TODO %?\nSCHEDULED: <%(org-read-date)>")
             ("a" "Assignment" entry
            (file+headline "~/Dropbox/Org/agenda/egyetem.org" "Assignments")
            "* TODO [#B] %? :@egyetem:@assignment: \nDEADLINE: <%(org-read-date)>")
            ("e" "Exam" entry
            (file+headline "~/Dropbox/Org/agenda/egyetem.org" "Vizsgák")
            "* TODO [#A] %? :@egyetem:@vizsga: \nSCHEDULED: <%(org-read-date)>")
            ("i" "inbox" entry
            (file "~/Dropbox/Org/agenda/inbox.org")
            "* TODO %?\nSCHEDULED: <%(org-read-date)>"))
       ))

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

(setq! bibtex-completion-bibliography '("/home/hrothgar32/Documents/allamvizsga/allamvizsga.bib"))
(setq! bibtex-completion-pdf-field "File")
(setq! org-cite-global-bibliography '("/home/hrothgar32/Documents/allamvizsga/allamvizsga.bib"))
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

(org-cite-register-processor 'my-bibtex-org-cite-follow
  :follow (lambda (_ _) (ivy-bibtex)))
(setq org-cite-follow-processor 'my-bibtex-org-cite-follow)

(setq org-noter-notes-search-path (list (concat org-roam-directory "/references")))
(setq org-noter-always-create-frame t)

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

(use-package! dashboard
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(require 'bison-mode)

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (after! pdf-annot
    (defun +pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook! 'pdf-view-mode-hook
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  :config
  (defadvice! +pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  ;; Despite its namesake, this does not call `pdf-tools-install', it only sets
  ;; up hooks, auto-mode-alist/magic-mode-alist entries, global modes, and
  ;; refreshes pdf-view-mode buffers, if any.
  ;;
  ;; I avoid calling `pdf-tools-install' directly because `pdf-tools' is easy to
  ;; prematurely load in the background (e.g. when exporting an org file or by
  ;; packages like org-pdftools). And I don't want pdf-tools to suddenly block
  ;; Emacs and spew out compiler output for a few minutes in those cases. It's
  ;; abysmal UX. The `pdf-view-mode' advice above works around this with a less
  ;; cryptic failure message, at least.
  (pdf-tools-install-noverify)

  ;; For consistency with other special modes
  (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Handle PDF-tools related popups better
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("^\\*Edit Annotation " :quit nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; HACK Refresh FG/BG for pdfs when `pdf-view-midnight-colors' is changed by a
  ;;      theme or with `setq!'.
  ;; TODO PR this upstream?
  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (+pdf-reload-midnight-minor-mode-h)
               ;; Defer refresh for buffers that aren't visible, to avoid
               ;; blocking Emacs for too long while changing themes.
               (add-hook 'doom-switch-buffer-hook #'+pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw))))


(use-package! saveplace-pdf-view
  :after pdf-view)
