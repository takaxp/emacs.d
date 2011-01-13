;;; Configuration for Mac
;;;                                       Takaaki ISHIKAWA  <takaxp@ieee.org>

;; Spotlight search with anything.el
(defun anything-spotlight ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-mac-spotlight)
   " *anything-spotlight*"))

;; QuickLook can show a file saved by Emacs 
;(setq encoding-for-xattr "SHIFT_JIS;2561")
;(setq encoding-for-xattr "UTF-8;134217984")
;(setq encoding-for-xattr "EUC-JP;2361")
;(setq encoding-for-xattr "MACINTOSH;0")
;(setq encoding-for-xattr "ISO-2022-JP;2080")
(defun set-xattr-mac ()
  (interactive)
  (progn
    (setq mime-for-xattr
	  (coding-system-get buffer-file-coding-system 'mime-charset))
    (setq encoding-for-xattr nil)
    (cond
     ((eq mime-for-xattr 'utf-8) (setq encoding-for-xattr "UTF-8;134217984"))
     ((eq mime-for-xattr 'shift_jis) (setq encoding-for-xattr "SHIFT_JIS;2561"))
     ((eq mime-for-xattr 'euc-jp) (setq encoding-for-xattr "EUC-JP;2361")))
    (if (and (not (eq encoding-for-xattr nil))
	     ;; Exception for a buffer saved on external devices
	     (not (string-match "^/Volumes" (buffer-file-name))))
	(shell-command
	 (format "xattr -w com.apple.TextEncoding \"%s\" %s"
		 encoding-for-xattr (buffer-name (current-buffer)))))))
; Exception for spcific hosts
(if (or
     (string= "mbp.local" (system-name)) (string= "mini.local" (system-name)))
    (add-hook 'after-save-hook 'set-xattr-mac)); add metadata after save


;; Flag for inline-patch
(setq default-input-method "MacOSX")
;; To input two-byte character with Shift key under inline-patch
(mac-add-key-passed-to-system 'shift)
;; Set Kawasemi as a input method
;(mac-set-input-method-parameters "jp.monokakido.inputmethod.Kawasemi" 'cursor-type "red")
;(mac-set-input-method-parameter "com.google.inputmethod.Japanese.base" 'title "具")

;; Use dictionary in Emacs (require EB Library, eblook, and lookup.el)
(autoload 'lookup "lookup" nil t)
(autoload 'lookup-region "lookup" nil t)
(autoload 'lookup-pattern "lookup" nil t)
; (autoload 'lookup-wood "lookup" nil t) ; for ispell
(define-key ctl-x-map "l" 'lookup)
(define-key ctl-x-map "y" 'lookup-region)
(define-key ctl-x-map "\C-y" 'lookup-pattern)
(setq lookup-search-agents
      '(
        (ndeb "~/Storage/Dic/COBUILD5")
        (ndeb "~/Storage/Dic/LDOCE4")
        (ndeb "~/Storage/Dic/eijiro")
;        (ndeb "~/Storage/Dic/COBUILD5/COBUILD/DATA/HONMON")
        ))
(setq lookup-default-dictionary-options
      '((:stemmer .  stem-english)))
(setq lookup-use-kakasi nil)
;        (ndeb "~/Storage/Dic/COBUILD5")


;;; MobileOrg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://orgmode.org/manual/Setting-up-the-staging-area.html

;(setq org-mobile-files "next.org")
(setq org-mobile-files (quote ("~/Dropbox/org/next.org" "~/Dropbox/org/support.org" "~/Dropbox/org/note.org")))

;; Set a file to capture data from iOS devices
(setq org-mobile-inbox-for-pull (concat org-directory "captured.org"))
; Upload location stored org files (index.org will be created)
(setq org-mobile-directory "~/Dropbox/MobileOrg/")
;(setq org-mobile-directory "/scpc:hoge@hoge.com:/path/to/the/mobileorg/")
;(setq org-mobile-directory "/Volumes/webdav/mobileorg/")
;(setq org-mobile-directory "~/Desktop/mobileorg/")


;;; Menu to push or pull org files using MobileOrg
(defun org-mobile-sync ()
  (interactive)
  (setq org-mobile-sync-type
	(read-from-minibuffer "How do you sync the org files? (pull or push) "))
  (message "%s" org-mobile-sync-type)
  (cond
   ((string= "pull" org-mobile-sync-type)(org-mobile-pull))
   ((string= "push" org-mobile-sync-type)(org-mobile-push))))       
;  (if (yes-or-no-p "How do you sync the org files? ")


; Whenever this file is read, pull files from the server
(message "%s" "MobileOrg sync ... [pull]")
(sleep-for 1.0)
(org-mobile-pull)


;;; Automatic called functions when Emacs enters idle time ;;;;;;;;;;;;;;;;;;;;
(defun sleep-after-reload ()
  (interactive)
  (message "%s" "reloading...")
  (sleep-for 0.5)

  ; Set alarms of org-agenda
  (message "%s" "set alarms")
  (sleep-for 0.5)
  (org-agenda-to-appt)
  (sleep-for 0.5)

  ; Export an iCal file
  (message "%s" "iCal export")
  (sleep-for 0.5)
  (reload-ical-export)
  (sleep-for 0.5)

  ; Send org files to the server
  (message "%s" "MobileOrg sync ... [push]")
  (sleep-for 0.5)
  (org-mobile-push)

; add new functions here
;
  (message "%s" "done")
  (sleep-for 0.5)
  (message "%s" "")
)

(provide 'mac)
