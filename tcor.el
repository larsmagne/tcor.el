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

(defvar tcor-api-key nil)

(defun tcor-upload (file &optional no-process)
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
	     ;;("OCREngine" . "2")
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
				"https://apipro2.ocr.space/parse/image" t nil
				600))
			     (generate-new-buffer "*error*"))
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\n\n" nil t)
	    (buffer-substring (point) (point-max)))
	(kill-buffer (current-buffer))))))

(defun tcor-ocr (file &optional no-process)
  (message "OCR-ing %s" file)
  (let ((data (tcor-upload file no-process))
	(coding-system-for-write 'utf-8)
	json)
    (with-temp-buffer
      (insert (or data ""))
      (goto-char (point-min))
      (setq json (ignore-errors
		   (json-read)))
      (when (eq (cdr (assq 'IsErroredOnProcessing json))
		t)
	(message "%s" (cdr (assq 'ErrorMessage json)))
	(sit-for 2))
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
			   "[.][^.]+\\'" ".txt" file))))))))

(defun tcor-all ()
  (dolist (file (directory-files-recursively
		 "~/src/kwakk/magscan/"
		 "page.*jpg$"))
    (unless (file-exists-p
	     (replace-regexp-in-string "[.][^.]+\\'" ".json" file))
      (tcor-ocr file t)))
  (magscan-covers-and-count))

(defun tcor-unpack-cbr (dir mag)
  (dolist (cbr (directory-files dir t "[.]cb[zr]$"))
    (when (string-match "\\b[0-9][0-9][0-9]\\b" cbr)
      (message "%s" cbr)
      (let* ((issue (match-string 0 cbr))
	     (sub (expand-file-name issue
				    (format "~/src/kwakk/magscan/%s/" mag))))
	(unless (file-exists-p sub)
	  (make-directory sub t)
	  (let ((default-directory sub))
	    (if (string-match "[.]cbr$" cbr)
		(call-process "unrar" nil nil nil "e" cbr)
	      (unless (zerop (call-process "unzip" nil nil nil "-j" cbr))
		(call-process "7z" nil nil nil "e" cbr)))
	    ;; Delete Macos resource files.
	    (dolist (file (directory-files sub t "\\`[.]_"))
	      (delete-file file))
	    (cl-loop
	     with i = 0
	     for file in (directory-files sub t "[.][jJ][pP][eE]?[gG]$")
	     for size = (magscan-image-size file)
	     do (if (< (car size) (cdr size))
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
		  (delete-file file)))))))))

(defun tcor-unpack-cbr-unnumered (dir)
  (cl-loop for cbr in (directory-files dir t "[.]cb[zr]$")
	   for issue from 1
	   do
	   (progn
	     (message "%s" cbr)
	     (let* ((sub (expand-file-name (format "WS%03d" issue)
					   "~/src/kwakk/magscan/W/")))
	       (unless (file-exists-p sub)
		 (make-directory sub)
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
    (setq urls
	  (cl-loop for hit across
		   (gethash "hits"
			    (gethash "hits"
				     (gethash "body"
					      (gethash "response" data))))
		   collect
		   (cons (gethash "identifier" (gethash "fields" hit))
			 (gethash "title" (gethash "fields" hit)))))))

(defun tcor-unpack-jp2 (dir mag)
  (dolist (zip (directory-files dir t "[.]zip$"))
    (when (string-match "\\b[0-9][0-9][0-9][0-9]\\b" zip)
      (message "%s" zip)
      (let* ((issue (match-string 0 zip))
	     (sub (expand-file-name issue
				    (format "~/src/kwakk/magscan/%s/" mag))))
	(unless (file-exists-p sub)
	  (make-directory sub t)
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
	     for size = (magscan-image-size file)
	     do (when (> (car size) 3000)
		  (call-process "gm" nil nil nil
				"mogrify" "-resize" "3000x"
				file)
		  (setq size (magscan-image-size file)))
	     do (if (< (car size) (cdr size))
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
	     (delete-file jp-file))))))))

(defun tcor-unpack-pdf (dir mag &optional inhibit-split)
  (dolist (pdf (directory-files dir t "[.]pdf$"))
    (when (string-match "\\b[0-9][0-9][0-9]\\b" pdf)
      (let* ((issue (match-string 0 pdf))
	     (sub (expand-file-name issue
				    (format "~/src/kwakk/magscan/%s/" mag))))
	(message "%s" pdf)
	(unless (file-exists-p sub)
	  (make-directory sub t)
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
			  (magscan-image-size file))
	     do (if (or inhibit-split
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
		  (delete-file file)))))))))

(provide 'tcor)

;;; tcor.el ends here
