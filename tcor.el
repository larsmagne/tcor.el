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
      (when json
	(write-region (point-min) (point-max)
		      (replace-regexp-in-string "[.][^.]+\\'" ".json" file))))
    (let ((ocr (cdr (assq 'ParsedResults json))))
      (when ocr
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
  (dolist (file (directory-files-recursively "~/src/kwakk/magscan/AH"
					     "page.*jpg$"))
    (unless (file-exists-p
	     (replace-regexp-in-string "[.][^.]+\\'" ".json" file))
      (tcor-ocr file (string-match-p "AH" file)))))

(provide 'tcor)

;;; tcor.el ends here
