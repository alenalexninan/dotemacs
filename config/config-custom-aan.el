(defgroup dotemacs-custom-aan nil
  "Configuration options for miscellaneous."
  :group 'dotemacs
  :prefix 'dotemacs-misc)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (remove-if-not 'buffer-file-name (buffer-list)))))
