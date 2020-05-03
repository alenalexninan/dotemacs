(after 'config-ivy
;; Remove some unwanted files from findfile list
(setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
;; Remove virtual buffers from switch buffer
(setq ivy-use-virtual-buffers nil)
)
(provide 'config-ivy-aan)
