(after 'org  
  (defhydra hydra-org-template (:color blue :hint nil)
    "
   _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:      p_y_thon output
   _l_atex   _E_xample   _p_erl          _i_ndex:      p_Y_thon tangled
   _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:    ipy_t_hon output
   _s_rc     _n_ote      plant_u_ml      _H_TML:       ipy_T_hon file
   _h_tml    ^ ^         cm_d_prompt     _A_SCII:      ipython _r_aw
  "
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("n" (hot-expand "<not"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "perl"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("d" (hot-expand "<s" "sh :shcmd \"cmdproxy.exe\""))
    ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
    ("Y" (hot-expand "<s" "python :results output :exports both :shebang \"#!/usr/bin/env python\"\n"))
    ("y" (hot-expand "<s" "python :results output"))
    ("t" (hot-expand "<s" "ipython :results raw drawer output :exports both :session"))
    ("r" (hot-expand "<s" "ipython :results raw drawer :exports both :session"))
    ("T" (hot-expand "<s" "ipython :results raw drawer output :exports both :session :ipyfile ./image.png"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("o" nil "quit"))

  (require 'org-tempo) ; Required from org 9 onwards for old template expansion
  ;; Reset the org-template expnsion system, this is need after upgrading to org 9 for some reason
  (setq org-structure-template-alist (eval (car (get 'org-structure-template-alist 'standard-value))))
  (defun hot-expand (str &optional mod header)
    "Expand org template.
  STR is a structure template string recognised by org like <s. MOD is a
  string with additional parameters to add the begin line of the
  structure element. HEADER string includes more parameters that are
  prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-tempo-complete-tag)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1))))

  (eval-after-load "org"
    '(cl-pushnew
      '("not" . "note")
      org-structure-template-alist))

  ;; Startup ORG setups used by default
  (define-skeleton org-skeleton
    "Header info for an Org file"
    "Title: "
    "#+TITLE:" str "\n"
    "#+AUTHOR: Alen Alex Ninan\n"
    "#+email: alen.alexninan@exxonmobil.com\n"
    "Time-stamp: <>\n"
    "#+STARTUP: showall\n"
    "#+STARTUP: indent\n"
    "#+STARTUP: align\n"
    "#+STARTUP: inlineimages\n"
    "#+ARCHIVE: %s_done::\n"
    "#+OPTIONS: num:nil toc:nil\n"  
    )

  (define-skeleton org-kanban-insert
    "Kanban insert in org file"
    "Kanban"
    "#+BEGIN: kanban\n"
    "#+END:\n"
    )
  
  (defhydra /hydras/org (:hint nil :exit t)
    "
   org:   Sorting & states                Org functions                    Tracking                 Editing
          ---------------------------------------------------------------------------------------------------------------------
          _s_ → sort all                   _e_ → babel evaluate buffe r     _c_ → clock-in           _n_ → new footnote
          _u_ → update all cookies         _b_ → babel execute src          _o_ → clock-out          _v_ → convert DF to table
          _h_ → export to html             _p_ → org properties             _q_ → clock cancel       _z_ → startup config
          _x_ → presentation export        _f_ → reload image               _m_ → clock-in modify    _k_ → add kanban
          _a_ → archive all done tree      _i_ → toggle image               _g_ → clock goto
          _d_ → archive all done file      _t_ → Convert table        
    "
    
    ("s" /org/org-sort-entries)
    ("u" /org/update-all-statistical-cookies)
    ("h" /org/export-to-html)
    ("x" org-reveal-export-to-html-and-browse)    
    ("a" /org/archive-done-tasks-tree)
    ("d" /org/archive-done-tasks-file)
    ("n" org-footnote-new)
    ("e" org-babel-execute-buffer)    
    ("b" org-babel-execute-src-block)
    ("p" org-set-property)
    ("i" org-toggle-inline-images)
    ("t" org-table-create-or-convert-from-region)
    ("f" org-redisplay-inline-images)
    ("c" org-clock-in)
    ("o" org-clock-out)
    ("q" org-clock-cancel)
    ("m" org-clock-modify-effort-estimate)
    ("g" org-clock-goto)
    ("v" org-table-create-or-convert-from-region)
    ("z" org-skeleton)
    ("k" org-kanban-insert)
    )
  )

(provide 'config-bindings-hydras-aan)
