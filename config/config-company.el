(when (eq dotemacs-completion-engine 'company)

  (defgroup dotemacs-company nil
    "Configuration options for company-mode."
    :group 'dotemacs
    :prefix 'dotemacs-company)

  (require-package 'company)

  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)

  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)

  (setq company-dabbrev-code-ignore-case t)
  (setq company-dabbrev-code-everywhere t)

  (setq company-etags-ignore-case t)

  (setq company-global-modes
        '(not
          comint-mode
          erc-mode
          eshell-mode
          term-char-mode
          term-line-mode
          term-mode
          text-mode))

  (global-company-mode)

  ;; has big performance impact
  ;; (require-package 'company-flx)
  ;; (company-flx-mode)

  ;; (when (display-graphic-p)
  ;;   (require-package 'company-quickhelp)
  ;;   (setq company-quickhelp-delay 0.2)
  ;;   (company-quickhelp-mode t))

  (require-package 'company-posframe)
  (company-posframe-mode 2)

  (after 'yasnippet
    (setq company-backends
          (mapcar
           (lambda (backend)
             (if (and (listp backend) (member 'company-yasnippet backend))
                 backend
               (append (if (consp backend) backend (list backend))
                       '(:with company-yasnippet))))
           company-backends)))

  (unless (face-attribute 'company-tooltip :background)
    (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
    (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
    (set-face-attribute 'company-preview nil :background "black")
    (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
    (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
    (set-face-attribute 'company-scrollbar-fg nil :background "gray40"))


  (require-package 'company-box)
  (add-hook 'company-mode-hook 'company-box-mode)
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        ;; company-box-icons-functions
        ;; (cons #'+company-box-icons--elisp-fn
        ;;       (delq 'company-box-icons--elisp
        ;;             company-box-icons-functions))
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))



  )

(provide 'config-company)
