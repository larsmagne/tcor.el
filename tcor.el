;;; tcor.el --- ocr.space OCR -*- lexical-binding: t -*-
;; Copyright (C) 2016 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: tools

;; tcor.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; tcor.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

(require 'json)
(require 'mm-url)
(require 'vtable)

(defvar tcor-api-key nil)
(defvar tcor-inhibit-split nil)

(defun tcor-upload (file &optional no-process _language)
  (let* ((url-request-method "POST")
	 (boundary "--14--")
	 (url-request-extra-headers
	  `(("Content-Type" . ,(format "multipart/form-data; boundary=%s"
				       boundary))))
	 (url-request-data
	  (mm-url-encode-multipart-form-data
	   `(("apikey" . ,tcor-api-key)
	     ("isOverlayRequired" . "true")
	     ("scale" . "true")
	     ;;("language" . ,(or language "eng"))
	     ("OCREngine" . "2")
	     ("file" . (("filedata" . ,(with-temp-buffer
					 (set-buffer-multibyte nil)
					 (insert-file-contents file)
					 (unless no-process
					   (call-process-region
					    (point-min) (point-max)
					    "convert" t (current-buffer) nil
					    "-morphology" "close" "diamond"
					    "-" "png:-"))
					 ;; ocr.space has a 5MB image limit.
					 (when (> (buffer-size) 4500000)
					   (call-process-region
					    (point-min) (point-max)
					    "convert" t (current-buffer) nil
					    "-" "jpg:-"))
					 (buffer-string)))
			("name" . "file")
			("filename" . ,(file-name-nondirectory file)))))
	   boundary)))
    (with-current-buffer (or (ignore-errors
			       (url-retrieve-synchronously
				"https://apipro1.ocr.space/parse/image" t nil
				600))
			     (generate-new-buffer "*error*"))
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\n\n" nil t)
	    (buffer-substring (point) (point-max)))
	(kill-buffer (current-buffer))))))

(defun tcor-language (file)
  (let* ((mag (car (last (file-name-split file) 3)))
	 (data (assq (intern mag) (tcor-magazines))))
    (cdr (assq 'lang (cdr data)))))

(defun tcor-ocr (file &optional no-process)
  ;;(setq debug-on-error t)
  (message "OCR-ing %s" file)
  (let ((data (tcor-upload file no-process (tcor-language file)))
	(coding-system-for-write 'utf-8)
	(got-error nil)
	json)
    (with-temp-buffer
      (insert (or data ""))
      (goto-char (point-min))
      (setq json (ignore-errors
		   (json-read)))
      (when (eq (cdr (assq 'IsErroredOnProcessing json))
		t)
	(if (stringp (assq 'ErrorMessage json))
	    (message "%s" (assq 'ErrorMessage json))
	  (message "%s: %s"  (ignore-errors (cdr (assq 'ErrorMessage json)))
		   (ignore-errors
		     (cdr (assq 'ErrorMessage
				(elt (cdr (assq 'ParsedResults json)) 0))))))
	(setq got-error t)
	(sleep-for 2))
      (when (and json
		 (eq (cdr (assq 'IsErroredOnProcessing json))
		     :json-false))
	(write-region (point-min) (point-max)
		      (replace-regexp-in-string "[.][^.]+\\'" ".json" file))))
    (let ((ocr (cdr (assq 'ParsedResults json))))
      (when (and ocr
		 (plusp (length ocr)))
	(with-temp-buffer
	  (let ((text (cdr (assq 'ParsedText (aref ocr 0)))))
	    (insert text)
	    (goto-char (point-min))
	    (while (re-search-forward "\r\n" nil t)
	      (replace-match " " t t))
	    (goto-char (point-min))
	    (while (re-search-forward " +" nil t)
	      (replace-match " " t t))
	    (goto-char (point-min))
	    (while (re-search-forward "- " nil t)
	      (replace-match "" t t))
	    (write-region (point-min) (point-max)
			  (replace-regexp-in-string
			   "[.][^.]+\\'" ".txt" file))))))
    got-error))

(defun tcor--identify (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (call-process "identify" nil t nil "-format" "%w %h"
		  (expand-file-name file))
    (buffer-string)))

(defun tcor-image-size (file)
  (let ((size (mapcar #'string-to-number (split-string (tcor--identify file)))))
    (cons (car size) (cadr size))))

(defun tcor-resize-images ()
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (let (size)
      (when (and (not (file-exists-p (file-name-with-extension file "json")))
		 (or (> (car (setq size (tcor-image-size file))) 3000)
		     (> (cdr size) 5000)))
	(message "Resizing %s" file)
	(call-process "mogrify" nil nil nil "-resize" "3000x5000>"
		      (expand-file-name file))))))

(defun tcor-fix-big-images ()
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (when (and (not (file-exists-p (file-name-with-extension file "json")))
	       (> (file-attribute-size (file-attributes file))
		  (* 4500 1024)))
      (message "Resizing for kb %s" file)
      (when (file-exists-p "/tmp/tcor.jpg")
	(delete-file "/tmp/tcor.jpg"))
      (cl-loop for size from 3000 downto 1000 by 100
	       do (call-process "convert" nil nil nil
				"-resize" (format "%dx" size)
				(expand-file-name file) "/tmp/tcor.jpg")
	       while (> (file-attribute-size (file-attributes "/tmp/tcor.jpg"))
			(* 5000 1024)))
      (when (file-exists-p "/tmp/tcor.jpg")
	(rename-file "/tmp/tcor.jpg" file t)))))

(defun tcor-resize ()
  (tcor-resize-images)
  (tcor-fix-big-images))

(defun tcor-all ()
  (let ((errors nil))
    (dolist (file (directory-files-recursively
		   "~/src/kwakk/magscan/" "page.*jpg$"))
      (unless (file-exists-p
	       (replace-regexp-in-string "[.][^.]+\\'" ".json" file))
	(when (tcor-ocr file t)
	  (setq errors t))))
    errors))

(defun tcor-sharded (instance instances)
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/" "page.*jpg$"))
    (when (and (not (file-exists-p (file-name-with-extension file "json")))
	       (= (mod (string-to-number (sha1 file) 16) instances) instance))
      (tcor-ocr file t)))
  (kill-emacs))

(defun tcor-sort-by-size (files)
  (nreverse
   (sort files (lambda (f1 f2)
		 (< (file-attribute-size (file-attributes f1))
		    (file-attribute-size (file-attributes f2)))))))

(defun tcor-import-files (mag &optional prefix)
  (tcor-import-file mag prefix
		    (directory-files "." t "jp2.*zip\\|[.]cb[rz7]$\\|pdf$\\|rar$")))

(defun tcor-import-file (mag &optional prefix files)
  (delete-other-windows)
  (dolist (file (tcor-sort-by-size (or files (dired-get-marked-files nil current-prefix-arg))))
    (dolist (dir
	     (cond
	      ((string-match "pdf$" file)
	       (tcor-unpack-pdf (list file) mag prefix))
	      ((string-match "jp2.*zip" file)
	       (tcor-unpack-jp2 (list file) mag prefix))
	      ((string-match "[.]cb[rz7]$\\|[.]rar$\\|[.]zip$" file)
	       (tcor-unpack-cbr (list file) mag prefix))
	      ((file-directory-p file)
	       (tcor-unpack-directories (list file) mag prefix))))
      (when (file-exists-p dir)
	(let ((first (car (directory-files dir t "page.*jpg"))))
	  (if (not first)
	      (message "Error: No pages in %s" dir)
	    (find-file first))))))
  (tcor-find-empty-directories))

(defun tcor-unpack (mag &optional prefix)
  (dolist (file (tcor-sort-by-size
		 (directory-files default-directory
				  t
				  "[.]cb[zr7]$\\|[.]pdf$\\|jp2.*[.]zip$")))
    (tcor-import-file mag prefix (list file))))

(defun tcor-issue-match (mag)
  (if (member mag '("CBG" "CSN"))
      "\\b[0-9][0-9][0-9][0-9]\\b"
    "\\b[0-9][0-9][0-9]\\b"))

(defvar tcor-override-issue nil)

(defun tcor-issue (mag file)
  (or tcor-override-issue
      (and (string-match (tcor-issue-match mag) file)
	   (match-string 0 file))))

(defun tcor-file-exists-p (dir)
  (let ((exists (file-exists-p dir)))
    (unless exists
      (make-directory dir t))
    (when (directory-empty-p dir)
      (setq exists nil))
    exists))

(defun tcor-unpack-cbr (dir mag &optional prefix)
  (let ((subs nil))
    (dolist (cbr (tcor-sort-by-size (if (consp dir)
					dir
				      (directory-files dir t "[.]cb[zr7]$"))))
      (when-let ((issue (tcor-issue mag cbr)))
	(message "%s" cbr)
	(let* ((sub (expand-file-name (concat (or prefix "") issue)
				      (format "~/src/kwakk/magscan/%s/" mag))))
	  (unless (tcor-file-exists-p sub)
	    (let ((default-directory sub))
	      (if (string-match "[.]cbr$\\|[.]rar$" cbr)
		  (with-environment-variables (("LANG" "C.UTF-8"))
		    (call-process "unrar" nil (get-buffer-create "*rar*") nil "e" cbr))
		(unless (zerop (call-process "unzip" nil nil nil "-j" cbr))
		  (call-process "7z" nil nil nil "e" cbr)))
	      ;; Delete Macos resource files.
	      (dolist (file (directory-files sub t "\\`[.]_"))
		(delete-file file))
	      (cl-loop
	       with i = 0
	       for file in (let ((case-fold-search t))
			     ;; 
			     (directory-files sub t "[.]\\([jJ][pP][eE]?[gG]\\|[wW][eE][bB][pP]\\|[pP][nN][gG]\\)\\'"))
	       for size = (tcor-image-size file)
	       do
	       (call-process "chmod" nil nil nil "u+rw" file)
	       (if (or tcor-inhibit-split
		       (< (car size) (cdr size)))
		   ;; Vertical
		   (if (let ((case-fold-search t))
			 (string-match "[.]jpe?g\\'" file))
		       (rename-file file
				    (expand-file-name (format "page-%03d.jpg"
							      (cl-incf i))
						      sub))
		     (call-process "convert" nil nil nil
				   (expand-file-name file)
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub)))
		 ;; Horizontal.
		 (call-process
		  "convert" nil nil nil
		  "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		  (file-truename file)
		  (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				    sub))
		 (call-process
		  "convert" nil nil nil
		  "-crop" (format "%sx%s+%s-0"
				  (/ (car size) 2)
				  (cdr size)
				  (/ (car size) 2))
		  ;; "1949x3064+1949-0"
		  (file-truename file)
		  (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				    sub))
		 (delete-file file))))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-cbr-unnumered (dir)
  (cl-loop for cbr in (directory-files dir t "[.]cb[zr7]$")
	   for issue from 1
	   do
	   (progn
	     (message "%s" cbr)
	     (let* ((sub (expand-file-name (format "WS%03d" issue)
					   "~/src/kwakk/magscan/W/")))
	       (unless (tcor-file-exists-p sub)
		 (let ((default-directory sub))
		   (if (string-match "[.]cbr$" cbr)
		       (call-process "unrar" nil nil nil "e" cbr)
		     (call-process "unzip" nil nil nil "-j" cbr))
		   (cl-loop for page from 1
			    for file in (directory-files
					 sub t "[.][jJ][pP][eE]?[gG]$")
			    do (rename-file file
					    (expand-file-name
					     (format "page-%03d.jpg" page)
					     sub)))))))))

(defun tcor-find-archive-org-urls (search-url)
  (interactive "sURL: ")
  (pop-to-buffer "*archive*")
  (erase-buffer)
  (let ((data
	 (with-current-buffer (url-retrieve-synchronously search-url)
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (json-parse-buffer)
	     (kill-buffer (current-buffer))))))
    (cl-loop for hit across
	     (gethash "hits"
		      (gethash "hits"
			       (gethash "body"
					(gethash "response" data))))
	     collect
	     (cons (gethash "identifier" (gethash "fields" hit))
		   (gethash "title" (gethash "fields" hit))))))

(defun tcor-unpack-jp2 (dir mag &optional prefix)
  (let ((subs nil))
    (dolist (zip (if (consp dir)
		     dir
		   (directory-files dir t "[.]zip$")))
      (when-let ((issue (tcor-issue mag zip)))
	(message "%s" zip)
	(let ((sub (expand-file-name (concat (or prefix "") issue)
				     (format "~/src/kwakk/magscan/%s/" mag))))
	  (unless (tcor-file-exists-p sub)
	    (let ((default-directory sub))
	      (call-process "unzip" nil nil nil "-j" zip)
	      ;; Delete Macos resource files.
	      (dolist (file (directory-files sub t "\\`[.]_"))
		(delete-file file))
	      (cl-loop
	       with i = 0
	       for jp-file in (directory-files sub t "[.][jJ][pP]2$")
	       for file = (file-name-with-extension jp-file ".jpg")
	       do (call-process "convert" nil nil nil jp-file file)
	       for size = (tcor-image-size file)
	       do (when (> (car size) 3000)
		    (call-process "gm" nil nil nil
				  "mogrify" "-resize" "3000x"
				  file)
		    (setq size (tcor-image-size file)))
	       do (if (or tcor-inhibit-split
			  (< (car size) (cdr size)))
		      ;; Vertical
		      (rename-file file
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub))
		    ;; Horizontal.
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+%s-0"
				     (/ (car size) 2)
				     (cdr size)
				     (/ (car size) 2))
		     ;; "1949x3064+1949-0"
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (delete-file file))
	       (delete-file jp-file)))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-pdf (dir mag &optional prefix inhibit-split)
  (let ((subs nil))
    (dolist (pdf (if (consp dir)
		     dir
		   (directory-files dir t "[.]pdf$")))
      (when-let ((issue (tcor-issue mag pdf)))
	(let ((sub (expand-file-name (concat (or prefix "") issue)
				      (format "~/src/kwakk/magscan/%s/" mag))))
	  (message "%s" pdf)
	  (unless (tcor-file-exists-p sub)
	    (let ((default-directory sub))
	      (call-process "pdftoppm" nil nil nil
			    "-jpeg"
			    "-rx" "250"
			    "-ry" "250"
			    pdf
			    "PDF")
	      (cl-loop
	       with i = 0
	       for file in (directory-files sub t "[.]jpg$")
	       for size = (progn
			    (when nil
			      (call-process "mogrify" nil nil nil
					    "-rotate" "270"
					    file))
			    (tcor-image-size file))
	       do (if (or tcor-inhibit-split
			  inhibit-split
			  (< (car size) (cdr size)))
		      ;; Vertical
		      (rename-file file
				   (expand-file-name (format "page-%03d.jpg"
							     (cl-incf i))
						     sub))
		    ;; Horizontal.
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+0+0" (/ (car size) 2) (cdr size))
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (call-process
		     "convert" nil nil nil
		     "-crop" (format "%sx%s+%s-0"
				     (/ (car size) 2)
				     (cdr size)
				     (/ (car size) 2))
		     ;; "1949x3064+1949-0"
		     (file-truename file)
		     (expand-file-name (format "page-%03d.jpg" (cl-incf i))
				       sub))
		    (delete-file file))))
	    (push sub subs)))))
    subs))

(defun tcor-unpack-directories (dir mag &optional prefix)
  (dolist (dir (if (consp dir)
		   dir
		 (directory-files dir t "[0-9]")))
    (when-let ((issue (tcor-issue mag dir)))
      (let* ((sub (expand-file-name (concat (or prefix "") issue)
				    (format "~/src/kwakk/magscan/%s/" mag)))
	     (i 0))
	(message "%s" dir)
	(unless (tcor-file-exists-p sub)
	  (dolist (file (directory-files dir t (tcor-case-insensate "webp\\|jpeg\\|png\\|jpg\\'")))
	    (call-process
	     "convert" nil nil nil
	     (file-truename file)
	     (expand-file-name (format "page-%03d.jpg" (cl-incf i)) sub))))))))

(defun tcor-case-insensate (string)
  (replace-regexp-in-string
   "[a-z]"
   (lambda (char)
     (format "[%s%s]" char (upcase char)))
   string))

(defun tcor-magazines ()
  (with-temp-buffer
    (insert-file-contents "~/src/kwakk/magazines.json")
    (json-read)))

(defun tcor-query-mags (&optional start)
  (let ((data (tcor-magazines)))
    (when start
      (setq data (nthcdr start data)))
    (cl-loop for mag in data
	     unless (assq 'complete (cdr mag))
	     do (tcor-browse-url-firefox
		 (concat "https://www.scribd.com/search?query=%22"
			 (string-replace " " "+" (cdr (assq 'name (cdr mag))))
			 "%22"))
	     (sleep-for 1))))

(defun tcor-list-missing-issues (mag &optional browse first archive max name)
  (unless max
    (setq max most-positive-fixnum))
  (let* ((data (tcor-magazines))
	 (dfile (format "~/src/kwakk/magscan/%s/double-issues.txt" mag))
	 (doubles (and (file-exists-p dfile)
		       (with-temp-buffer
			 (insert-file-contents dfile)
			 (split-string (buffer-string)))))
	 (elem (cdr (assq (intern mag) data))))
    (unless elem
      (user-error "No such mag: %s" mag))
    (let ((name (or name (cdr (assq 'name elem)))))
      (pop-to-buffer "*missing*")
      (erase-buffer)
      (let ((issues (directory-files (format "~/src/kwakk/magscan/%s/" mag) nil "[0-9]$")))
	(when-let ((last (cdr (assq 'last elem))))
	  (setq issues (nconc issues (list (format "%03d" (1+ last))))))
	(let ((groups (seq-group-by (lambda (issue)
				      (car (tcor-segment-issue issue mag)))
				    issues)))
	  (open-webs
	   (cl-loop for (group . issues) in groups
		    while (> max 0)
		    append
		    (cl-loop with start = 1
			     for num in (mapcar (lambda (issue)
						  (cdr (tcor-segment-issue issue mag)))
						issues)
			     while (> max 0)
			     append
			     (cl-loop for i from start upto (1- num)
				      for string = (string-clean-whitespace
						    (format "%S %s %03d" name group i))
				      while (> max 0)
				      when (and (or (not first)
						    (> i first))
						(not (member (format "%03d" i) doubles)))
				      append
				      (progn
					(cl-decf max)
					(insert string "\n")
					(let ((urls nil))
					  (when browse
					    (when archive
					      (push (format "https://archive.org/search?query=%s" string) urls)
					      (when (string-match " 0" string)
						(push (format "https://archive.org/search?query=%s"
							      (replace-regexp-in-string " 0+" " " string))
						      urls)))
					    (push (format "https://annas-archive.se/search?q=%s" string) urls)
					    (when (string-match " 00" string)
					      (push (format "https://annas-archive.se/search?q=%s"
							    (replace-regexp-in-string " 00" " 0" string))
						    urls))
					    (when (string-match " 0" string)
					      (push (format "https://annas-archive.se/search?q=%s"
							    (replace-regexp-in-string " 0+" " " string))
						    urls)))
					  (nreverse urls))))
			     do (setq start (1+ num))))))))))

(defun tcor-browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Firefox.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Firefox window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "firefox " url) nil
           "/usr/local/bin/firefox/firefox"
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))

(defun tcor-segment-issue (issue mag)
  (let* ((len (if (member mag '(CBG CSN)) 4 3))
	 (num (substring issue (- (length issue) len))))
    (if (string-match "^[0-9]+$" num)
	(cons (substring issue 0 (- (length issue) len))
	      (string-to-number num))
      (and (string-match "[A-Z]+" issue)
	   (cons (match-string 0 issue)
		 (string-to-number (substring issue (match-end 0))))))))

(defun tcor-delete-extra-pages (mag)
  (switch-to-buffer "*extra*")
  (dolist (dir (directory-files (format "~/src/kwakk/magscan/%s/" mag) t "[0-9]$"))
    (dolist (file (seq-take (nreverse (directory-files dir t "page.*jpg")) 2))
      (erase-buffer)
      (insert-image (create-image file nil nil :max-height (frame-pixel-height)))
      (when (y-or-n-p (format "Delete %s? " file))
	(delete-file file)
	(when (file-exists-p (file-name-with-extension file "txt"))
	  (delete-file (file-name-with-extension file "txt")))
	(when (file-exists-p (file-name-with-extension file "json"))
	  (delete-file (file-name-with-extension file "json")))))))

(defun tcor-split-into-issues (start pages)
  (let ((files (directory-files "." nil "page.*jpg")))
    (cl-loop for num from start
	     for dir = (format "../SPL-%03d/" num)
	     while files
	     do
	     (make-directory dir)
	     (cl-loop for i from 1 upto pages
		      do (rename-file (pop files)
				      (format "%spage-%03d.jpg" dir i))))))

(defun tcor-list-dirs-without-pages ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (unless (directory-files issue nil "page-001.jpg")
	(message "Missing %s" issue)))))

(defun tcor-index (&optional mags force)
  (dolist (mag (or mags (directory-files "~/src/kwakk/magscan/" nil "[A-Z]")))
    (let* ((dir (concat "~/src/kwakk/magscan/" mag))
	   (newest (car
		    (sort (directory-files-recursively dir "[.]txt\\'\\|issues.json")
			  #'file-newer-than-file-p)))
	   (omega (expand-file-name (format "~/src/kwakk/omega.db/%s/docdata.glass" mag))))
      (when (or force
		(not (file-exists-p omega))
		(file-newer-than-file-p newest omega))
	(message "Indexing %s" mag)
	(let ((default-directory (expand-file-name "~/src/kwakk/magscan/")))
	  (call-process "rsync" nil nil nil
			"-a" "--delete"
			"--include=*/" "--include=page*.txt" "--exclude=*"
			(expand-file-name dir)
			(concat "/tmp/" mag))
	  (call-process "rm" nil nil nil
			"-rf" (expand-file-name (concat "~/src/kwakk/omega.db/" mag)))
	  (call-process "omindex" nil nil nil
			"-S" "--db" (expand-file-name (concat "~/src/kwakk/omega.db/" mag))
			"--url" "/"
			"-E" "8000"
			(concat "/tmp/" mag)))))))

(defun tcor-count-pages ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" nil "[A-Z]"))
    (let* ((dir (concat "~/src/kwakk/magscan/" mag))
	   (newest (car
		    (sort (append (directory-files-recursively dir "[.]txt\\'")
				  (and (file-exists-p (expand-file-name "suppress-covers.txt" dir))
				       (list (expand-file-name "suppress-covers.txt" dir)))
				  (and (file-exists-p (expand-file-name "double-issues.txt" dir))
				       (list (expand-file-name "double-issues.txt" dir))))
			  #'file-newer-than-file-p))))
      (when (or (not (file-exists-p (expand-file-name "issues.json" dir)))
		(file-newer-than-file-p newest (expand-file-name "issues.json" dir)))
	(message "Counting %s" mag)
	(magscan-count-pages dir)))))

(defun tcor-gather-data ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (mag (directory-files "~/src/kwakk/magscan/" nil "[A-Z]"))
      (let* ((dir (concat "~/src/kwakk/magscan/" mag "/"))
	     (pages (length (directory-files-recursively dir "page.*jpg")))
	     (issues (length (directory-files dir nil "[0-9]$"))))
	(setf (gethash mag table)
	      (list (cons 'issues issues)
		    (cons 'pages pages)))))
    (with-temp-buffer
      (insert (json-encode table))
      (write-region (point-min) (point-max) "~/src/kwakk/data.json"))))  

(defun tcor-pre-ocr ()
  (tcor-resize))

(defun tcor-post-ocr ()
  (magscan-covers-and-count)
  (tcor-count-pages)
  (tcor-gather-data)
  (tcor-index))

(defun tcor-find-credits-pages (&optional hours mag start)
  (let ((files nil))
    (dolist (mag (if mag
		     (list (expand-file-name mag "~/src/kwakk/magscan/"))
		   (directory-files "~/src/kwakk/magscan/" t "[A-Z]")))
      (dolist (issue (directory-files mag t "[0-9]$"))
	(dolist (file (nreverse (seq-take (nreverse (directory-files issue t "page.*jpg")) 4)))
	  (when (and (< (file-attribute-size (file-attributes file)) (* 500 1024))
		     (or (not hours)
			 (> (float-time (file-attribute-modification-time (file-attributes issue)))
			    (- (float-time) (* hours 60 60)))))
	    (push file files)))))
    (setq files (nreverse files))
    (catch 'stop
      (let ((i (or start 0)))
	(while (< i (length files))
	  (setq file (elt files i))
	  (switch-to-buffer "*pages*")
	  (erase-buffer)
	  (insert-image (create-image file nil nil :max-height (truncate (* (frame-pixel-height) 0.9))))
	  (let ((answer (car (read-multiple-choice (format "Delete %s?" file)
						   '((?y "Yes")
						     (?n "No")
						     (?p "Prev")
						     (?s "Stop"))))))
	    (cond
	     ((= answer ?y)
	      (delete-file file)
	      (when (file-exists-p (file-name-with-extension file "txt"))
		(delete-file (file-name-with-extension file "txt")))
	      (when (file-exists-p (file-name-with-extension file "json"))
		(delete-file (file-name-with-extension file "json")))
	      (when (file-exists-p (expand-file-name "issues.json" mag))
		(delete-file (expand-file-name "issues.json" mag)))
	      (cl-incf i))
	     ((= answer ?n)
	      (cl-incf i))
	     ((= answer ?s)
	      (throw 'stop i))
	     (t
	      (cl-decf i)))))))))

(defun tcor-list-all-mags (&optional category)
  (switch-to-buffer "*list*")
  (erase-buffer)
  (insert "List 30 English language magazines and fanzines focused on discussing comic books, and also list approximately how many issues the magazine published.  Order by number of issues, descending.  Exclude magazines that are predominantly comics magazines -- list only magazines that are about comics instead.  Prioritize magazines with long runs.  Exclude magazines from this list: ")
  (let ((titles (tcor-known-titles)))
    (dolist (mag (with-temp-buffer
		   (insert-file-contents "~/src/kwakk/magazines.json")
		   (json-read)))
      (when (or (not category)
		(equal category (cdr (assq 'cat (cdr mag)))))
	(if (assq 'misc (cdr mag))
	    (dolist (name (cdr (assq 'prefix (cdr mag))))
	      (unless (member (cdr name) titles)
		(insert (cdr name) ", ")))
	  (unless (member (cdr (assq 'name (cdr mag))) titles)
	    (insert (cdr (assq 'name (cdr mag))) ", ")))))
    (dolist (title titles)
      (insert title ", "))))

(defun tcor-link-all-mags ()
  (switch-to-buffer "*list*")
  (erase-buffer)
  (dolist (mag (with-temp-buffer
		 (insert-file-contents "~/src/kwakk/magazines.json")
		 (json-read)))
    (unless (cdr (assq 'hidden (cdr mag)))
      (if (memq (car mag) '(MIF MIM MII MIP))
	  (dolist (name (cdr (assq 'prefix (cdr mag))))
	    (insert (format "<a href='https://kwakk.info/%s/'>%s</a>, "
			    (downcase (symbol-name (car mag)))
			    (cdr name))))
	(insert (format "<a href='https://kwakk.info/%s/'>%s</a>, "
			(downcase (symbol-name (car mag)))
			(cdr (assq 'name (cdr mag)))))))))


(defun tcor-do ()
  ;; Check that the mags file is valid.
  (tcor-magazines)
  (tcor-pre-ocr)
  (call-process "~/src/tcor.el/orc-shard")
  ;; Loop while there's errors.
  (cl-loop for i from 0 upto 10
	   while (tcor-all)
	   do (sleep-for 1))
  (tcor-post-ocr))

;; https://www.comicsfanzines.co.uk/s-z/speakeasy-81-120
(defun tcor-fetch-views (url)
  (let ((dom
	 (with-current-buffer (url-retrieve-synchronously url)
	   (goto-char (point-min))
	   (search-forward "\n\n")
	   (prog1
	       (libxml-parse-html-region (point) (point-max))
	     (kill-buffer (current-buffer))))))
    dom))

(defun tcor-parse-views (dom)
  (cl-loop for link in (dom-by-tag dom 'a)
	   for url = (dom-attr link 'href)
	   for issue = (string-trim (dom-texts (dom-parent dom (dom-parent dom link))))
	   when (and url
		     (string-match "drive.google.*/view" url)
		     (not (zerop (length issue))))
	   collect (list url (replace-regexp-in-string
			      "[^a-zA-Z0-9]" "-"
			      (replace-regexp-in-string
			       "\\([0-9]+\\) +\\([()/0-9]+\\)\\'"
			       "\\1\\2" issue)))))

(defun tcor-view-urls (url)
  (let ((list (tcor-parse-views (tcor-fetch-views url)))
	(table (make-hash-table :test #'equal)))
    (with-temp-buffer
      (dolist (elem list)
	(unless (gethash (car elem) table)
	  (insert (cadr elem) " " (car elem) "\n")
	  (setf (gethash (car elem) table) t)))
      (write-region (point-min) (point-max) "/tmp/urls.txt" t))))

(defun tcor-weed-urls (mag)
  (let ((files (directory-files (format "~/src/kwakk/magscan/%s/" mag))))
    (while (not (eobp))
      (if (and (re-search-forward "\\b\\([0-9][0-9][0-9]\\)\\b" (pos-eol) t)
	       (member (match-string 1) files))
	  (delete-line)
	(forward-line 1)))))

(defun tcor-find-empty-directories ()
  (interactive)
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (unless (file-exists-p (expand-file-name "page-001.jpg" issue))
	(find-file issue)))))

(defun tcor-view (file)
  (interactive (list (car (dired-get-marked-files nil current-prefix-arg))))
  (cond
   ((string-match "[.]pdf\\'" file)
    (call-process "xxpdf" nil nil nil file))
   ((string-match "z$\\|zip$" file)
    (with-temp-buffer
      (call-process "unzip" nil t nil "-l" file)
      (goto-char (point-min))
      (forward-line 6)
      (message "%s" (buffer-substring (point) (pos-eol 5)))))
   ((string-match "r\\'" file)
    (with-temp-buffer
      (call-process "unrar" nil t nil "l" file)
      (goto-char (point-min))
      (forward-line 8)
      (message "%s" (buffer-substring (point) (pos-eol 6)))))))

(keymap-global-set "H-i" #'tcor-view)

(defun tcor-find-discontinuities ()
  (dolist (mag (directory-files "~/src/kwakk/magscan/" t "[A-Z]"))
    (dolist (issue (directory-files mag t "[0-9]$"))
      (let* ((pages (sort (directory-files issue nil "page.*jpg\\'")
			  #'string-version-lessp))
	     (last (car (last pages))))
	(unless (= (length pages)
		   (and (string-match "[0-9]+" last)
			(string-to-number (match-string 0 last))))
	  (find-file issue)
	  (error "Num %s; last %s" (length pages) last))))))


;; (dolist (id '(FIGMAR ZONE PAPER KABOOM CISO BCM PX FDC TB MIMF VECU SPLITTER LD LAH FIGMAN EKL CSG BA)) (tcor-insert-blog id))
(defun tcor-insert-blog (mag)
  (let* ((mags (with-temp-buffer
		 (insert-file-contents "~/src/kwakk/magazines.json")
		 (json-read)))
	 (data (assq mag mags))
	 (url (format "https://kwakk.info/%s/"
		      (downcase (symbol-name mag)))))
    (let ((browse-url-browser-function browse-url-secondary-browser-function))
      (browse-url url))
    (ewp-import-screenshot 4)
    (insert (format "<a href=\"%s\">%s</a>\n\n\n"
		    url
		    (cdr (assq 'name data))))))

(defun tcor-crop ()
  (ignore-errors (make-directory "c"))
  (dolist (page (directory-files "." nil "page.*jpg"))
    (let ((size (tcor-image-size page)))
      (ignore size)
      (call-process "convert" nil (get-buffer-create "*crop*") nil
		    "+repage" "-crop"
		    (format "%dx%d+%d+%d"
			    3000 3900
			    0 400
			    ;;(car size)
			    ;;4880
			    ;;4900
			    ;;(- 2830 96)
			    ;;1660 2550
			    
			    ;;512
			    ;;1026
			    ;;200 200
			    )
		    page (concat "c/" page)))))

(defun tcor-query-libgen ()
  (while (re-search-forward "^ +\\(.*\\) - \\(Approx\\|Over\\)" nil t)
    (tcor-browse-url-firefox
     (concat "https://annas-archive.org/search?index=&page=1&q=" (match-string 1) "&display=&content=book_comic&sort=")
     ;; (concat "https://libgen.li/index.php?req=" (match-string 1) "&columns%5B%5D=s&objects%5B%5D=f&objects%5B%5D=e&objects%5B%5D=s&topics%5B%5D=l&topics%5B%5D=c&topics%5B%5D=f&topics%5B%5D=m&res=100&showch=on&gmode=on&filesuns=all")
     )
    (sleep-for 2)))

(defvar tcor-misc-counter 75)

(defun tcor-add-misc (name)
  (interactive (list (buffer-substring-no-properties (mark) (point))))
  (let ((prefix (format "C%d-" (cl-incf tcor-misc-counter))))
    (save-window-excursion
      (find-file "~/src/kwakk/magazines.json")
      (goto-char (point-min))
      (search-forward "\"MICS\":")
      (search-forward "}")
      (beginning-of-line)
      (insert (format "      %S: %S,\n" prefix name)))
    (let* ((tcor-override-issue "001")
	   (files (dired-get-marked-files nil current-prefix-arg))
	   (tcor-inhibit-split (or nil (string-match "pdf\\'" (car files)))))
      (tcor-import-file "MICS" prefix files)
      (rename-file (car files) "/var/cx/mics/"))))

(defun tcor-find-mags-with-few-issues ()
  (pop-to-buffer "*mags*")
  (erase-buffer)
  (dolist (mag (tcor-magazines))
    (when (< (length (directory-files (expand-file-name (symbol-name (car mag)) "~/src/kwakk/magscan/")))
	     10)
      (insert (symbol-name (car mag)) " " (cdr (assq 'name (cdr mag))) "\n"))))

(defun tcor-html (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (search-forward "\n\n")
    (prog1
	(libxml-parse-html-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun tcor-download (url file)
  (call-process "curl" nil nil nil
		"--output" file url))

(defun tcor-sane ()
  (cl-loop for link in (dom-by-tag (tcor-html "https://digitalcommons.unl.edu/sane/all_issues.html") 'a)
	   for url = (dom-attr link 'href)
	   when (and url (string-match "/iss[0-9]+/" url))
	   do
	   (let ((dir (and (string-match "/vol.*" url)
			   (concat "/tmp/sane/" (string-replace "/" "-" (match-string 0 url)))))
		 html)
	     (unless (file-exists-p dir)
	       (make-directory dir t)
	       (setq html (tcor-html url))
	       (tcor-download (cl-loop for img in (nreverse (dom-by-tag html 'img))
				       for src = (dom-attr img 'src)
				       when (and src (string-match "assets/md5images" src))
				       return (shr-expand-url src url))
			      (expand-file-name "cover.gif" dir))
	       (cl-loop with part = 0
			for page in (dom-by-tag html 'a)
			for pdf = (dom-attr page 'href)
			;; https://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=1055&context=sane
			when (and pdf (string-match "/cgi/viewcontent.cgi" pdf))
			do (tcor-download pdf (expand-file-name (format "%02d.pdf" (cl-incf part)) dir)))))))

(defun tcor-known-titles ()
  (let ((titles nil))
    (with-temp-buffer
      (insert-file-contents "~/src/kwakk/known-magazines.txt")
      (while (re-search-forward "^[0-9]+) \\(.*\\)" nil t)
	(push (string-trim (match-string 1)) titles)))
    titles))

(defvar-keymap tcor-list-mode-map
  "s" #'tcor-list-search
  "w" #'tcor-list-search-with-name)

(define-derived-mode tcor-list-mode special-mode "TCOR")

(defun tcor-list ()
  (interactive)
  (switch-to-buffer "*list*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (tcor-list-mode)
    (make-vtable
     :columns '((:name "ID")
		(:name "Complete")
		(:name "Type")
		(:name "Language")
		(:name "Name"))
     :objects (tcor-magazines)
     :getter
     (lambda (mag column vtable)
       (pcase (vtable-column vtable column)
	 ("ID" (symbol-name (car mag)))
	 ("Complete" (if (assq 'complete (cdr mag)) "*" ""))
	 ("Type" (cdr (assq 'cat (cdr mag))))
	 ("Language" (or (cdr (assq 'lang (cdr mag))) ""))
	 ("Name" (cdr (assq 'name (cdr mag)))))))))

(defun tcor-list-search-with-name (name)
  (interactive "sSearch with name: ")
  (tcor-list-search name))

(defun tcor-list-search (&optional name)
  (interactive)
  (let* ((mag (vtable-current-object))
	 (name (or name (cdr (assq 'name (cdr mag)))))
	 (url (format "https://annas-archive.org/search?q=%s"
		      (browse-url-encode-url (concat "\"" name "\""))))
	 (results nil)
	 (first t)
	 func dom urls)
    (setq urls (list url))
    
    (setq func
	  (lambda (url)
	    (url-retrieve
	     url
	     (lambda (&rest _)
	       (goto-char (point-min))
	       (search-forward "\n\n")
	       (let ((start (point)))
		 (when (search-forward " partial matches<" nil t)
		   (delete-region (point) (point-max)))
		 (setq dom (libxml-parse-html-region start (point-max))))
	       (setq d dom)
	       (kill-buffer (current-buffer))
	       (cl-loop for comment in (dom-by-tag dom 'comment)
			for text = (dom-text comment)
			when (and text (string-match "<h3 " text))
			do (dom-append-child dom
					     (with-temp-buffer
					       (insert text)
					       (libxml-parse-html-region (point-min) (point-max)))))
	       (setq results
		     (append results
			     (cl-loop for entry in (dom-by-tag dom 'h3)
				      for siblings = (dom-non-text-children (dom-parent dom entry))
				      collect (list :name (dom-text entry)
						    :spec (dom-text (cadr (memq entry (reverse siblings))))
						    :publisher (dom-text (cadr (memq entry siblings)))
						    :link (dom-attr (dom-parent dom (dom-parent dom entry)) 'href)))))

	       (if first
		   (setq urls
			 (seq-uniq
			  (cl-loop for link in (dom-by-tag dom 'a)
				   for href = (dom-attr link 'href)
				   when (and href
					     (string-match "&page=\\([0-9]+\\)" href)
					     (not (equal (match-string 1 href) "1")))
				   collect (shr-expand-url href "https://annas-archive.org/")))
			 first nil)
		 (pop urls))
	       (if (not urls)
		   (tcor-list-results results (car mag))
		 (run-at-time 2 nil func (car urls)))))))
    (funcall func (car urls))))

(defun tcor-list-results (results mag)
  (switch-to-buffer "*results*")
  (let ((inhibit-read-only t)
	(objs (tcor-filter-results results mag)))
    (erase-buffer)
    (setq truncate-lines t)
    (tcor-list-results-mode)
    (when objs
      (make-vtable
       :columns '((:name "Match")
		  (:name "Name" :width 40)
		  (:name "Publisher" :width 20)
		  (:name "Spec"))
       :objects objs
       :getter
       (lambda (res column vtable)
	 (pcase (vtable-column vtable column)
	   ("Match" (plist-get res :match))
	   ("Name" (plist-get res :name))
	   ("Publisher" (plist-get res :publisher))
	   ("Spec" (replace-regexp-in-string "\\`[^,]*,[^,]*,[^,]*,[^,]*," ""
					     (plist-get res :spec))))))
      nil)))

(defvar-keymap tcor-list-results-mode-map
  "RET" #'tcor-anna-archive)

(defun tcor-anna-archive (&optional pop)
  (interactive "P")
  (let ((result (vtable-current-object)))
    (funcall (if pop #'open-web-new-window #'open-web-same-window)
	     (shr-expand-url (plist-get result :link) "https://annas-archive.org/"))))

(define-derived-mode tcor-list-results-mode special-mode "TCOR")

(defun tcor-filter-results (results mag)
  (let ((missing (tcor-missing-issues mag)))
    (tcor-sort-results
     (cl-loop for elem in results
	      for match = (tcor-match-p (plist-get elem :name) missing)
	      when match
	      collect (append elem (list :match match)))
     missing)))

(defun tcor-sort-results (results missing)
  (sort results
	(lambda (r1 r2)
	  (< (tcor-result-rank r1 missing)
	     (tcor-result-rank r2 missing)))))

(defun tcor-result-rank (result missing)
  (or
   (cl-loop for i from 0
	    for number in (tcor-numberify-name (plist-get result :name))
	    when (memq number missing)
	    return i)
   most-positive-fixnum))

(defun tcor-match-p (name missing)
  ;; Filter out common false matches.
  (setq name (replace-regexp-in-string "\\bc2c\\b" "" name))
  (let ((numbers (tcor-numberify-name name)))
    (cl-loop for (g . m) in (reverse missing)
	     when (member m numbers)
	     return (format "%s%s%s" g (if (equal g "") "" " ") m))))

(defun tcor-numberify-name (name)
  (cl-loop with start = 0
	   while (string-match "[0-9]+" name start)
	   collect (string-to-number (match-string 0 name))
	   do (setq start (match-end 0))))

(defun tcor-missing-issues (mag)
  (let* ((data (tcor-magazines))
	 (dfile (format "~/src/kwakk/magscan/%s/double-issues.txt" mag))
	 (doubles (and (file-exists-p dfile)
		       (with-temp-buffer
			 (insert-file-contents dfile)
			 (split-string (buffer-string)))))
	 (elem (cdr (assq mag data)))
	 (digits (if (member mag '(CBG CSN))
		     4
		   3))
	 (issues (directory-files (format "~/src/kwakk/magscan/%s/" mag) nil "[0-9]$"))
	 (highest (string-to-number
		   (or
		    (car (last (seq-filter (lambda (e)
					     (string-match-p "\\`[0-9]+\\'" e))
					   issues)))
		    "0"))))
    (when-let ((last (cdr (assq 'last elem))))
      (setq issues (nconc issues (cl-loop for i from (1+ highest) upto last
					  collect (format (format "%%0%dd" digits) i)))))
    (let ((groups (seq-group-by (lambda (issue)
				  (car (tcor-segment-issue issue mag)))
				issues)))
      (cl-loop for (group . gissues) in groups
	       append
	       (cl-loop for i from 1 upto (cdr (tcor-segment-issue (car (last gissues)) mag))
			for em = (format (format "%%s%%0%dd" digits) group i)
			unless (or (member em doubles)
				   (member em issues))
			collect (cons group i))))))

(provide 'tcor)

;; List the 60 magazines and fanzines discussing comic books (but exclude comic book magazines as well as online magazines) that have the most issues, and also list approximately how many issues the magazine published.  Order by number of issues.  Do not include magazines in English, Spanish, French, Italian, German, Dutch, Japanese or Korean.

;; List 30 English language magazines and fanzines focused on discussing comic books, and also list approximately how many issues the magazine published.  Order by number of issues.  Exclude magazines that are predominantly comics magazines -- list only magazines that are about comics instead.  Exclude magazines from this list: 

;; List magazines and fanzines about comic books in any language, and also list approximately how many issues the magazine published.  Order by number of issues.  Exclude magazines that are predominantly comics magazines -- list only magazines that are about comics instead.  Exclude all magazines from the following list: 

;; List 30 Argentinian magazines and fanzines focused on discussing comic books, and also list approximately how many issues the magazine published.  Order by number of issues.  Exclude magazines that are predominantly comics magazines -- list only magazines that are about comics instead. 

;;; tcor.el ends here
