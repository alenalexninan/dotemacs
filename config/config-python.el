;; Instructions:
;; -------------
;; Python assumed to be in path for python2 and python3 for windows. Anaconda not in path for windows.Anaconda in path for linux.
;; Make selection in select radio in "dotemacs-python/select".
;; Edit to point to the required folders in variables below.
;; Edit config for python for use of vertual environment in the toggle "dotemacs-python/venv".
;;Adding python to autoloads
(/boot/lazy-major-mode "\\.py$" python-mode)



(defgroup dotemacs-python nil
  "Configuration options for org-mode."
  :group 'dotemacs
  :prefix 'dotemacs-python)

;; Select python to use
(defcustom dotemacs-python/select
  'scimax
  "Python programming mode config"
  :type '(radio
          (const :tag "python3" python3)
          (const :tag "python" python2)
          (const :tag "anaconda" anaconda)
          (const :tag "scimax" scimax)
          )
  :group 'dotemacs-python)

;; Toggle to enable vertual environment for python. 
(defcustom dotemacs-python/venv
  t
  "Integrates with venv. Venv used if non `nil'"
  :type 'boolean
  :group 'dotemacs-python)

;; Check if windows.
(defcustom dotemacs-python/system-check (eq system-type 'windows-nt)
  "When non-nil, disables integration with `vc.el'.
  This is non-nil by default on Windows machines"
  :type 'boolean
  :group 'dotemacs-python)

;; Edit to configure
;; When windows
(when dotemacs-python/system-check
  (setq default-venv-name "pipesim2017") ;; edit name of default virtual environment
  (setq default-env-directory "c:/Users/alena/anaconda3/envs") ;; edit location for the default folder with virtual environment
  (setq anaconda-directory "c:/Users/alena/anaconda3") ;; edit the location for anaconda installation directory if selecetd.
  )
;; When other Operating system-type
(unless dotemacs-python/system-check
  (setq default-venv-name "flowassurance") ;; edit name of default virtual environment
  (setq default-env-directory "/home/alenalexninan/anaconda3/envs") ;; edit location for the default folder with virtual environment
  (setq anaconda-directory "/home/alenalexninan/anaconda3/") ;; edit the location for anaconda installation directory if selecetd.
  )


(require-package 'pip-requirements)

(setq default-venv-location (concat default-env-directory "/" default-venv-name "/"))

;; Settings for python 2
(when (eq dotemacs-python/select 'python)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (infer-indentation-style)
              (setq python-shell-interpreter "python")
              (after 'company
                ;; elpy installation
                (require-package 'pyvenv) 
                (require-package 'elpy)
                (elpy-enable)
                (after 'elpy
                  (setq elpy-rpc-python-command "python")
                  (when dotemacs-python/venv
                    (setenv "WORKON_HOME" default-env-directory)
                    (pyvenv-mode 1)
                    (pyvenv-tracking-mode 1)
                    (pyvenv-activate default-venv-location))
                  (add-to-list 'company-backends 'elpy-company-backend)                
                  ))))
  )

;; Settings for Python 3
(when (eq dotemacs-python/select 'python3)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (infer-indentation-style)
              (when dotemacs-python/system-check
                ;; something for windows if true
                ;; optional something if not
                (setq python-shell-interpreter "python"))
              (unless dotemacs-python/system-check
                (setq python-shell-interpreter "python3"))
              (after 'company
                ;; elpy installation
                (require-package 'pyvenv) 
                (require-package 'elpy)
                (elpy-enable)
                (after 'elpy
                  (when dotemacs-python/system-check
                    (setq elpy-rpc-python-command "python"))
                  (unless dotemacs-python/system-check
                    (setq elpy-rpc-python-command "python3"))
                  (when dotemacs-python/venv
                    (setenv "WORKON_HOME" default-env-directory)
                    (pyvenv-mode 1)
                    (pyvenv-tracking-mode 1)
                    (pyvenv-activate default-venv-location))
                  (add-to-list 'company-backends 'elpy-company-backend)                
                  ))))
  )

;; Setings for anaconda
(when (eq dotemacs-python/select 'anaconda)  
  
  ;; Adding anaconda to emacs path
  (when dotemacs-python/system-check
    (when (file-directory-p (concat anaconda-directory "/"))
      (setenv "PATH"
              (concat
               ;; Change this with your path to MSYS bin directory
               (concat (replace-regexp-in-string "/" "\\\\" anaconda-directory) ";" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Scripts;" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Library\\mingw-w64\\bin;" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Library\\bin;")
               (getenv "PATH")))))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (infer-indentation-style)
              (require-package 'anaconda-mode)
              (require-package 'pyvenv)              
              'anaconda-mode
              'anaconda-eldoc-mode                  
              (setq-default py-shell-name "ipython")
              (setq python-shell-interpreter "ipython"
                    python-shell-interpreter-args "--simple-prompt -i")
              (when dotemacs-python/venv
                (setenv "WORKON_HOME" default-env-directory)
                (pyvenv-mode 1)
                (pyvenv-tracking-mode 1)
                (pyvenv-activate default-venv-location))
              (setq org-babel-async-ipython t)
              ))
  (after 'company
    (require-package 'company-anaconda)
    (after 'org
      (add-to-list 'company-backends 'company-anaconda)
      ;; (add-to-list 'company-backends 'company-ob-ipython)
      )
    )
  
  (after 'org
    ;; Enabling ob-ipython
    (require-package 'ob-ipython)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ipython . t)))
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    
    ;; When Jupiter installed
    ;; (advice-add 'ob-ipython-auto-configure-kernels :around
    ;;             (lambda (orig-fun &rest args)
    ;;               "Configure the kernels when found jupyter."
    ;;               (when (executable-find ob-ipython-command)
    ;;                 (apply orig-fun args))))

    ;; Ob-ipython completion
    (defun run-python-first (&rest args)
      "Start a inferior python if there isn't one."
      (or (comint-check-proc "*Python*") (run-python)))

    (advice-add 'org-babel-execute:ipython :after
                (lambda (body params)
                  "Send body to `inferior-python'."
                  (run-python-first)
                  (python-shell-send-string body)))

    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                            '(pcomplete-completions-at-point python-completion-at-point))))

    ;; Eldoc function for ob-ipython completion
    (defun ob-ipython-eldoc-function ()
      (when (org-babel-where-is-src-block-head)
        (python-eldoc-function)))

    (add-hook 'org-mode-hook
              (lambda ()
                (setq-default eldoc-documentation-function 'ob-ipython-eldoc-function)))

    ;; Src block help ob-ipython
    (defun ob-ipython-help (symbol)
      (interactive (list (read-string "Symbol: " (python-eldoc--get-symbol-at-point))))
      (unless (org-babel-where-is-src-block-head)
        (error "Symbol is not in src block."))
      (unless (ob-ipython--get-kernel-processes)
        (error "There is no ob-ipython-kernal running."))
      (when-let* ((processes  (ob-ipython--get-kernel-processes))
                  (session (caar processes))
                  (ret (ob-ipython--eval
                        (ob-ipython--execute-request (format "help(%s)" symbol) session))))
        (let ((result (cdr (assoc :result ret)))
              (output (cdr (assoc :output ret))))
          (let ((buf (get-buffer-create "*ob-ipython-doc*")))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert output)
                (goto-char (point-min))
                (read-only-mode t)
                (pop-to-buffer buf)))))))
    )
  

  
  ;; Emacs ipython notebook.
  (require-package 'ein)
  (require 'ein)
  ;; (require ob-ein)
  (require 'ein-notebook)
  ;; (require 'ein-subpackages)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ein . t)))
  )



;; Setings for scimax
(when (eq dotemacs-python/select 'scimax)  
  
  ;; Adding anaconda to emacs path
  (when dotemacs-python/system-check
    (when (file-directory-p (concat anaconda-directory "/"))
      (setenv "PATH"
              (concat
               ;; Change this with your path to MSYS bin directory
               (concat (replace-regexp-in-string "/" "\\\\" anaconda-directory) ";" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Scripts;" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Library\\mingw-w64\\bin;" (replace-regexp-in-string "/" "\\\\" anaconda-directory) "\\Library\\bin;")
               (getenv "PATH")))))

  
  ;; (setq-default py-shell-name "ipython")
  ;; (setq python-shell-interpreter "ipython"
  ;;       python-shell-interpreter-args "--simple-prompt -i")
  (require-package 'anaconda-mode)
  (require-package 'pyvenv)
  (anaconda-mode)
  (anaconda-eldoc-mode)
  ;; Scimax
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (infer-indentation-style)
              ))
  (when dotemacs-python/venv
    (setenv "WORKON_HOME" default-env-directory)
    (pyvenv-mode 1)
    (pyvenv-tracking-mode 1)
    (pyvenv-activate default-venv-location)
    (after 'flycheck
      (setq flycheck-python-pycompile-executable (concat default-venv-location "bin/python"))
      (setq flycheck-python-flake8-executable (concat default-venv-location "bin/flake8"))
      (setq flycheck-python-pylint-executable (concat default-venv-location "bin/pylint"))
      (flycheck-mode -1)
      (flycheck-mode t))
    )
  

  (setq scimax-dir "~/.emacs.d/extra/scimax/")
  (add-to-list 'load-path "~/.emacs.d/extra/scimax/ob-ipython-upstream/")
  (add-to-list 'load-path "~/.emacs.d/extra/scimax/")
  (require 'scimax-org-babel-python)
  (require 'ob-ipython)
  (require-package 'lispy)
  (lispy-mode 1)

  
  (require 'scimax-org-babel-ipython-upstream)
  
  (setq ob-ipython-exception-results nil)
  (after 'org
    ;; Ob-ipython completion
    (defun run-python-first (&rest args)
      "Start a inferior python if there isn't one."
      (or (comint-check-proc "*Python*") (run-python)))

    (advice-add 'org-babel-execute:ipython :after
                (lambda (body params)
                  "Send body to `inferior-python'."
                  (run-python-first)
                  (python-shell-send-string body)))

    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                            '(pcomplete-completions-at-point python-completion-at-point))))

    ;; Eldoc function for ob-ipython completion
    (defun ob-ipython-eldoc-function ()
      (when (org-babel-where-is-src-block-head)
        (python-eldoc-function)))

    (add-hook 'org-mode-hook
              (lambda ()
                (setq-default eldoc-documentation-function 'ob-ipython-eldoc-function)))

    ;; Src block help ob-ipython
    (defun ob-ipython-help (symbol)
      (interactive (list (read-string "Symbol: " (python-eldoc--get-symbol-at-point))))
      (unless (org-babel-where-is-src-block-head)
        (error "Symbol is not in src block."))
      (unless (ob-ipython--get-kernel-processes)
        (error "There is no ob-ipython-kernal running."))
      (when-let* ((processes  (ob-ipython--get-kernel-processes))
                  (session (caar processes))
                  (ret (ob-ipython--eval
                        (ob-ipython--execute-request (format "help(%s)" symbol) session))))
        (let ((result (cdr (assoc :result ret)))
              (output (cdr (assoc :output ret))))
          (let ((buf (get-buffer-create "*ob-ipython-doc*")))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert output)
                (goto-char (point-min))
                (read-only-mode t)
                (pop-to-buffer buf)))))))
    )
  ;; (scimax-ob-ipython-turn-on-eldoc)
  ;; Scimax end

  )
(provide 'config-python)
