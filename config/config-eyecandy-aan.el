(after 'config-eyecandy

  (defcustom dotemacs-eyecandy/theme
    'none
    "List of hooks to automatically start up in Evil Emacs state."
    :type '(radio
            (const :tag "none" none)
            (const :tag "zenburn" 'zenburn)
            (const :tag "color-theme-sanityinc-tomorrow-nigh" 'color-theme-sanityinc-tomorrow-night)
            (const :tag "color-theme-sanityinc-tomorrow-day" 'color-theme-sanityinc-tomorrow-day)
            (const :tag "material-light" 'material-light)
            (const :tag "material" 'material)
            (const :tag "dracula" 'dracula)
            (const :tag "kaolin-dark" 'kaolin-dark)
            (const :tag "kaolin-light" 'kaolin-light)
            (const :tag "kaolin-valley-light" 'kaolin-valley-light)
            (const :tag "apropospriate-light" 'apropospriate-light)
            (const :tag "gruvbox-light" 'gruvbox-light)
            (const :tag "wombat" 'wombat)
            (const :tag "ample-light" 'ample-light)
            )
    :group 'dotemacs-eyecandy)


  (defcustom dotemacs-eyecandy/line-numbering
    'relative
    "Line numbering in emacs"
    :type '(radio
            (const :tag "relative" relative)
            (const :tag "absolute" absolute)
            (const :tag "none" none))
    :group 'dotemacs-eyecandy)

  ;; Themes config
  (cond
   ((eq dotemacs-eyecandy/theme 'none)

    (defface org-block-begin-line
      '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
      "Face used for the line delimiting the begin of source blocks.")

    (defface org-block-background
      '((t (:background "#FFFFEA")))
      "Face used for the source block background.")

    ;; (defface org-block-end-line
    ;;   '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
    ;;   "Face used for the line delimiting the end of source blocks.")
    
    ;; Helm selection color
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "yellow"
                          :foreground "black")
      )
    
    ;; Selection color of text selection.
    ;; (set-face-attribute 'region nil :background "pale turquoise")
    (setq org-ellipsis " ▼")
    )
   ((eq dotemacs-eyecandy/theme 'zenburn)
    (require-package 'zenburn-theme)
    (load-theme 'zenburn t)
    (setq zenburn-use-variable-pitch t)
    (setq zenburn-scale-org-headlines t)
    (setq zenburn-scale-outline-headlines t)
    )
   ((eq dotemacs-eyecandy/theme 'color-theme-sanityinc-tomorrow-night)
    (require-package 'color-theme-sanityinc-tomorrow)
    (require 'color-theme-sanityinc-tomorrow)
    (color-theme-sanityinc-tomorrow-night)
    )
   ((eq dotemacs-eyecandy/theme 'color-theme-sanityinc-tomorrow-day)
    (require-package 'color-theme-sanityinc-tomorrow)
    (require 'color-theme-sanityinc-tomorrow)
    (color-theme-sanityinc-tomorrow-day)
    )
   ((eq dotemacs-eyecandy/theme 'material)
    (require-package 'material-theme)
    (load-theme 'material t)
    )
   ((eq dotemacs-eyecandy/theme 'material-light)
    (require-package 'material-light-theme)
    (load-theme 'material-light t)
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "grey"
                          :foreground "black")
      )
    )
   ((eq dotemacs-eyecandy/theme 'dracula)
    (require-package 'dracula-theme)
    (load-theme 'dracula 't)
    )
   ((eq dotemacs-eyecandy/theme 'kaolin-dark)
    (require-package 'kaolin-themes)
    (load-theme 'kaolin-dark t)
    (after [all-the-icons treemacs]
      (kaolin-treemacs-theme))
    )
   ((eq dotemacs-eyecandy/theme 'kaolin-light)
    (require-package 'kaolin-themes)
    (load-theme 'kaolin-light t)
    (after [all-the-icons treemacs]
      (kaolin-treemacs-theme))
    )
   ((eq dotemacs-eyecandy/theme 'kaolin-valley-light)
    (require-package 'kaolin-themes)
    (load-theme 'kaolin-valley-light t)
    (after [all-the-icons treemacs]
      (kaolin-treemacs-theme))
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "grey"
                          :foreground "black")
      )
    )
   ((eq dotemacs-eyecandy/theme 'gruvbox-light)
    (require-package 'gruvbox-theme)
    (load-theme 'gruvbox-light-hard t)
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "grey"
                          :foreground "black")
      )
    )
   ((eq dotemacs-eyecandy/theme 'apropospriate-light)
    (require-package 'apropospriate-theme)
    (require 'apropospriate)
    (load-theme 'apropospriate-light t)
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "grey"
                          :foreground "black")
      )
    )
   ((eq dotemacs-eyecandy/theme 'wombat)
    (load-theme 'wombat t)
    (set-face-attribute 'region nil :background "#999")
    )
   ((eq dotemacs-eyecandy/theme 'ample-light)
    (require-package 'ample-theme)
    ;; then in your init you can load all of the themes
    ;; without enabling theme (or just load one)
    (load-theme 'ample t t)
    (load-theme 'ample-flat t t)
    (load-theme 'ample-light t t)
    ;; choose one to enable
    ;; (enable-theme 'ample)
    ;; (enable-theme 'ample-flat)
    (enable-theme 'ample-light)
    ;; (set-face-attribute 'region nil :background "#999")
    (after 'helm
      (set-face-attribute 'helm-selection nil 
                          :background "grey"
                          :foreground "black")
      )
    )
   )

  ;; Line numbering
  (unless (eq dotemacs-eyecandy/line-numbering 'none)
    (when (version<= "26.0.50" emacs-version )
      (global-display-line-numbers-mode)
      ))
  (when (eq dotemacs-eyecandy/line-numbering 'relative)
    (add-hook 'display-line-numbers-mode-hook (lambda () (setq display-line-numbers 'relative))))

  ;; Fonts and fint sizes
  (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 120
                        :weight 'normal)
    )
  (unless (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :height 120
                        :weight 'normal)
    )

  )

(provide 'config-eyecandy-aan)
