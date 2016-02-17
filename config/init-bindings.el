(defmacro bind (&rest commands)
  "Convenience macro which creates a lambda interactive command."
  `(lambda (arg)
     (interactive "P")
     ,@commands))


(require-package 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(which-key-mode)


(after 'evil
  (require-package 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  (after 'evil-leader
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "w" 'save-buffer
      "e" 'eval-last-sexp
      ", e" 'eval-defun
      "E" 'eval-defun
      "f" 'ctl-x-5-prefix
      "c" 'my-new-eshell-split
      "C" 'customize-group
      "b d" 'kill-this-buffer
      "v" (kbd "C-w v C-w l")
      "s" (kbd "C-w s C-w j")
      "P" 'package-list-packages
      "V" (bind (term "vim"))
      "h" help-map
      "h h" 'help-for-help-internal)

    (after "paradox-autoloads"
      (evil-leader/set-key "P" 'paradox-list-packages)))

  (define-key evil-normal-state-map (kbd "SPC t") #'my-toggle-hydra/body)
  (define-key evil-normal-state-map (kbd "SPC q") #'my-quit-hydra/body)

  (after "magit-autoloads"
    (define-key evil-normal-state-map (kbd "SPC g") #'my-git-hydra/body))

  (after "evil-numbers-autoloads"
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt))

  (after "git-gutter+-autoloads"
    (define-key evil-normal-state-map (kbd "[ h") #'git-gutter+-next-hunk)
    (define-key evil-normal-state-map (kbd "] h") #'git-gutter+-previous-hunk)

    (define-key evil-normal-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
    (define-key evil-normal-state-map (kbd ", g r") 'git-gutter+-revert-hunks)

    (define-key evil-visual-state-map (kbd ", g a") 'git-gutter+-stage-hunks)
    (define-key evil-visual-state-map (kbd ", g r") 'git-gutter+-revert-hunks)
    (evil-ex-define-cmd "Gw" (bind
                              (git-gutter+-stage-whole-buffer))))

  (define-key evil-visual-state-map (kbd "SPC SPC") 'execute-extended-command)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
  (define-key evil-normal-state-map (kbd "SPC e") #'my-errors-hydra/body)
  (define-key evil-normal-state-map (kbd "SPC b") #'my-buffer-hydra/body)
  (define-key evil-normal-state-map (kbd "SPC j") #'my-jump-hydra/body)
  (define-key evil-normal-state-map (kbd "SPC o") #'my-jump-hydra/bookmark-jump-and-exit)
  (define-key evil-normal-state-map (kbd "SPC f") #'my-file-hydra/body)
  (define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)

  (when (fboundp 'fzf)
    (define-key evil-normal-state-map (kbd "SPC F") 'fzf))

  (after "helm-autoloads"
    (define-key evil-normal-state-map (kbd "SPC h") #'my-helm-hydra/body)
    (define-key evil-normal-state-map (kbd "g b") 'helm-mini))

  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)

  (define-key evil-normal-state-map (kbd "[ SPC") (bind (evil-insert-newline-above) (forward-line)))
  (define-key evil-normal-state-map (kbd "] SPC") (bind (evil-insert-newline-below) (forward-line -1)))
  (define-key evil-normal-state-map (kbd "[ e") (kbd "ddkP"))
  (define-key evil-normal-state-map (kbd "] e") (kbd "ddp"))
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)

  (define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

  (after "etags-select-autoloads"
    (define-key evil-normal-state-map (kbd "g ]") 'etags-select-find-tag-at-point))

  (global-set-key (kbd "C-w") 'evil-window-map)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-w C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-w C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)

  (define-key evil-normal-state-map "p" #'my-paste-hydra/evil-paste-after)
  (define-key evil-normal-state-map "P" #'my-paste-hydra/evil-paste-before)

  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "Q") 'my-window-killer)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

  (define-key evil-visual-state-map (kbd ", e") 'eval-region)

  ;; emacs lisp
  (evil-define-key 'normal emacs-lisp-mode-map "K" (bind (help-xref-interned (symbol-at-point))))
  (after "elisp-slime-nav-autoloads"
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point))

  (after 'coffee-mode
    (evil-define-key 'visual coffee-mode-map (kbd ", p") 'coffee-compile-region)
    (evil-define-key 'normal coffee-mode-map (kbd ", p") 'coffee-compile-buffer))

  (after 'stylus-mode
    (define-key stylus-mode-map [remap eval-last-sexp] 'my-stylus-compile-and-eval-buffer)
    (evil-define-key 'visual stylus-mode-map (kbd ", p") 'my-stylus-compile-and-show-region)
    (evil-define-key 'normal stylus-mode-map (kbd ", p") 'my-stylus-compile-and-show-buffer))

  (after "projectile-autoloads"
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
    (define-key evil-normal-state-map (kbd "SPC /")
      (bind
       (if current-prefix-arg
           (cond
            ((executable-find "ag")
             (call-interactively #'projectile-ag))
            ((executable-find "pt")
             (call-interactively #'projectile-pt))
            ((executable-find "ack")
             (call-interactively #'projectile-ack))
            (t
             (call-interactively #'projectile-grep)))
         (cond
          ((eq dotemacs-switch-engine 'ivy)
           (counsel-ag nil (projectile-project-root)))
          ((eq dotemacs-switch-engine 'helm)
           (helm-do-ag (projectile-project-root)))))))
    (when (eq dotemacs-switch-engine 'helm)
      (after "helm-projectile-autoloads"
        (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile))))

  (after "multiple-cursors-autoloads"
    (after 'js2-mode
      (evil-define-key 'normal js2-mode-map (kbd "g r") 'js2r-rename-var))
    (define-key evil-normal-state-map (kbd "g r") 'mc/mark-all-like-this-dwim))

  (after "avy-autoloads"
    (define-key evil-operator-state-map (kbd "z") 'avy-goto-char-2)
    (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
    (define-key evil-motion-state-map (kbd "S-SPC") 'avy-goto-line))

  (after 'eshell
    (add-hook 'eshell-mode-hook
              (lambda ()
                (local-set-key (kbd "C-h") 'evil-window-left)
                (local-set-key (kbd "C-j") 'evil-window-down)
                (local-set-key (kbd "C-k") 'evil-window-up)
                (local-set-key (kbd "C-l") 'evil-window-right))))

  (cond ((eq dotemacs-switch-engine 'ivy)
         (define-key evil-normal-state-map (kbd "SPC l") 'swiper)
         (define-key evil-normal-state-map (kbd "SPC y") 'counsel-yank-pop))
        ((eq dotemacs-switch-engine 'helm)
         (define-key evil-normal-state-map (kbd "SPC l") 'helm-swoop)
         (define-key evil-normal-state-map (kbd "SPC L") 'helm-multi-swoop)
         (define-key evil-normal-state-map (kbd "SPC y") 'helm-show-kill-ring)))

  ;; butter fingers
  (evil-ex-define-cmd "Q" 'evil-quit)
  (evil-ex-define-cmd "Qa" 'evil-quit-all)
  (evil-ex-define-cmd "QA" 'evil-quit-all))

;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)


(after 'ivy
  (define-key ivy-mode-map [escape] (kbd "C-g")))


(after "magit-autoloads"
  (global-set-key (kbd "C-x g") 'magit-dispatch-popup))


(after "project-explorer-autoloads"
  (global-set-key [f2] 'project-explorer-open)
  (autoload 'pe/show-file "project-explorer")
  (global-set-key [f3] 'pe/show-file)
  (after 'project-explorer
    (define-key project-explorer-mode-map (kbd "C-l") 'evil-window-right)))


(after "multiple-cursors-autoloads"
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))


(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))


(after 'auto-complete
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous))


(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (after "helm-company-autoloads"
    (define-key company-mode-map (kbd "<C-return>") 'helm-company)
    (define-key company-active-map (kbd "<C-return>") 'helm-company)))


(after "expand-region-autoloads"
  (global-set-key (kbd "C-=") 'er/expand-region))


;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))


(after 'compile
  (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "k") 'compilation-previous-error))


(after 'help-mode
  (define-key help-mode-map (kbd "n") 'next-line)
  (define-key help-mode-map (kbd "p") 'previous-line)
  (define-key help-mode-map (kbd "j") 'next-line)
  (define-key help-mode-map (kbd "k") 'previous-line))


(after "helm-autoloads"
  (global-set-key (kbd "C-c h") #'my-helm-hydra/body))


(after "counsel-autoloads"
  (global-set-key (kbd "C-c i") #'my-ivy-hydra/body))


(global-set-key [prior] 'previous-buffer)
(global-set-key [next] 'next-buffer)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c s") 'my-goto-scratch-buffer)
(global-set-key (kbd "C-c e") 'my-eval-and-replace)
(global-set-key (kbd "C-c t") 'my-new-eshell-split)

(global-set-key (kbd "C-x c") 'calculator)
(global-set-key (kbd "C-x C") 'calendar)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(global-set-key (kbd "C-x p") 'proced)
(after "vkill-autoloads"
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))


;; replace with [r]eally [q]uit
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(after 'evil
  (defadvice evil-quit (around advice-for-evil-quit activate)
    (message "Thou shall not quit!"))
  (defadvice evil-quit-all (around advice-for-evil-quit-all activate)
    (message "Thou shall not quit!")))

(provide 'init-bindings)
