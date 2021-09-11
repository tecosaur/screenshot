;;; screenshot.el --- Swiftly grab images of your code -*- lexical-binding: t -*-

;; Copyright (C) 2020 TEC

;; Author: TEC <http://github/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Homepage: https://github.com/tecosaur/screenshot
;; Version: 0.1.0
;; Keywords: convenience, screenshot
;; Package-Requires: ((emacs "27") (transient "0.2.0") (posframe "0.8.3"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Convenience package for creating images of the current region or buffer.
;; Requires `imagemagick' for some visual post-processing, and `xclip' for
;; copying images to the clipboard.

;;; Code:

(require 'transient)
(require 'posframe)

(defgroup screenshot ()
  "Customise group for Screenshot."
  :group 'convenience)

(defvar screenshot--buffer nil
  "The buffer last used to create a screenshot.")

(defcustom screenshot-buffer-creation-hook nil
  "Hook run after creating a buffer for screenshots.
Run after hardcoded setup, but before the screenshot is captured."
  :type 'hook
  :group 'screenshot)

(defvar screenshot--region-beginning nil
  "Start of the region forming the screenshot.")
(defvar screenshot--region-end nil
  "End of the region forming the screenshot.")

(defvar screenshot--tmp-file nil
  "An intermediate target file for the screenshot.")

(defvar screenshot--first-line-number nil
  "The first line contained in the screenshot.")

(defvar screenshot--total-lines nil
  "The total number of lines contained in the screenshot.")

;;; Generated variables

;;; Screenshot parameters

(eval-when-compile
  (defmacro screenshot--define-infix (key name description type default
                                          &rest reader)
    "Define infix with KEY, NAME, DESCRIPTION, TYPE, DEFAULT and READER as arguments."
    `(progn
       (defcustom ,(intern (concat "screenshot-" name)) ,default
         ,description
         :type ,type
         :group 'screenshot)
       (transient-define-infix ,(intern (concat "screenshot--set-" name)) ()
         "Set `screenshot--theme' from a popup buffer."
         :class 'transient-lisp-variable
         :variable ',(intern (concat "screenshot-" name))
         :key ,key
         :description ,description
         :argument ,(concat "--" name)
         :reader (lambda (&rest _) ,@reader))))

  (screenshot--define-infix
   "-l" "line-numbers-p" "Show line numbers"
   'boolean nil
   (not screenshot-line-numbers-p))

  (screenshot--define-infix
   "-L" "relative-line-numbers-p" "Relative line numbers within the screenshot"
   'boolean nil
   (not screenshot-relative-line-numbers-p))

  (screenshot--define-infix
   "-t" "text-only-p" "Use a text-only version of the buffer"
   'boolean nil
   (not screenshot-text-only-p))

  (screenshot--define-infix
   "-T" "truncate-lines-p" "Truncate lines beyond the screenshot width"
   'boolean nil
   (not screenshot-truncate-lines-p))

  (declare-function counsel-fonts "ext:counsel-fonts")

  (declare-function ivy-read "ext:ivy-read")

  (screenshot--define-infix
   "-ff" "font-family" "Font family to use"
   'string (let ((font (face-attribute 'default :font)))
             (if (eq font 'unspecified) "monospace"
               (symbol-name (font-get font :family))))
   (if (fboundp #'counsel-fonts)
       (ivy-read "Font: " (delete-dups (font-family-list))
                 :preselect screenshot-font-family
                 :require-match t
                 :history 'counsel-fonts-history
                 :caller 'counsel-fonts)
     (completing-read "Font: " (delete-dups (font-family-list)))))

  (screenshot--define-infix
   "-fs" "font-size" "Font size (pt)"
   'number 14
   (read-number "Font size in pt: " screenshot-font-size))

;;;; Frame

  (screenshot--define-infix
   "-b" "border-width" "Border width in pixels"
   'integer 20
   (read-number "Border width in px: " screenshot-border-width))

  (screenshot--define-infix
   "-r" "radius" "Rounded corner radius"
   'integer 10
   (read-number "Border radius in px: " screenshot-radius))

  (screenshot--define-infix
   "-w" "min-width" "Minimum width, in columns"
   'integer 40
   (read-number "Minimum width (columns): " screenshot-min-width))

  (screenshot--define-infix
   "-W" "max-width" "Maximum width, in columns"
   'integer 120
   (read-number "Maximum width (columns): " screenshot-max-width))

;;;; Shadow

  (screenshot--define-infix
   "-s" "shadow-radius" "Radius of the shadow in pixels"
   'integer 12
   (read-number "Shadow width in px: " screenshot-shadow-radius))

  (screenshot--define-infix
   "-i" "shadow-intensity" "Intensity of the shadow"
   'integer 80
   (read-number "Shadow intensity: " screenshot-shadow-intensity))

  (screenshot--define-infix
   "-c" "shadow-color" "Color of the shadow"
   'color "#333"
   (read-string "Shadow color: " screenshot-shadow-color))

  (screenshot--define-infix
   "-x" "shadow-offset-horizontal" "Shadow horizontal offset"
   'integer -8
   (read-number "Shadow horizontal offset in px: " screenshot-shadow-offset-horizontal))

  (screenshot--define-infix
   "-y" "shadow-offset-vertical" "Shadow vertical offset"
   'integer 5
   (read-number "Shadow vertical offset in px: " screenshot-shadow-offset-vertical)))

;;; Main function

;;;###autoload
(defun screenshot (beg end &optional upload-text)
  "Take a screenshot of the current region or buffer.

Region included in screenshot is the active selection, interactively,
or given by BEG and END.  Buffer is used if region spans 0-1 characters.

When a universal argument is given, UPLOAD-TEXT is non-nil.
Then the text of the region/buffer is uploaded, and the URL is copied to clipboard."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end) (when current-prefix-arg t))
                 (list (point-min) (point-max) (when current-prefix-arg t))))

  (if upload-text
      (screenshot-text-upload beg end)
    (deactivate-mark)
    (screenshot--set-screenshot-region beg end)
    (setq screenshot--tmp-file
          (make-temp-file "screenshot-" nil ".png"))
    (call-interactively #'screenshot-transient)))

(defvar screenshot-text-upload-function #'screenshot-ixio-upload
  "Function to use to upload text.

Must take a start and end position for the current buffer, and
return a URL.")

(defun screenshot-text-upload (beg end)
  "Upload the region from BEG to END, and copy the upload URL to the clipboard."
  (message "Uploading text...")
  (let ((url
         (funcall screenshot-text-upload-function beg end)))
    (gui-select-text url)
    (message "Screenshot uploaded, link copied to clipboard (%s)" url)))

(defun screenshot-ixio-upload (beg end)
  "Upload the region from BEG to END to ix.io, and return the URL."
  (let ((output (generate-new-buffer "ixio")) url)
    (shell-command-on-region beg end
                             (format "curl -F 'ext:1=%s' -F 'f:1=<-' ix.io 2>/dev/null"
                                     (file-name-extension (or (buffer-file-name) " .txt")))
                             output)
    (setq url (string-trim-right (with-current-buffer output (buffer-string))))
    (kill-buffer output)
    url))

;;; Screenshot capturing

(defun screenshot--set-screenshot-region (beg end)
  "Use the region from BEG to END to determine the relevant region to capture.
Also records useful information like the total number of lines contained,
and the line number of the first line of the region."
  (when (or (= beg end) (= (1+ beg) end))
    (setq beg (point-min)
          end (point-max)))
  (save-excursion
    (goto-char beg)
    (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
      (forward-line 1)
      (setq beg (line-beginning-position)))
    (back-to-indentation)
    (when (= beg (point))
      (setq beg (line-beginning-position)))
    (goto-char end)
    (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
      (forward-line -1)
      (setq end (line-end-position))))
  (setq screenshot--region-beginning beg
        screenshot--region-end end
        screenshot--first-line-number (line-number-at-pos beg)
        screenshot--total-lines (- (line-number-at-pos end) (line-number-at-pos beg) -1)))

(defun screenshot--setup-buffer ()
  "Modify the current buffer to make it appropriate for screenshotting."
  (setq-local face-remapping-alist '((line-number-current-line line-number)
                                     (show-paren-match nil)
                                     (region nil))
              line-spacing 0.1)
  (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
  (when (bound-and-true-p solaire-mode) (solaire-mode -1))
  (run-hooks 'screenshot-buffer-creation-hook))

(defvar screenshot--text-only-buffer
  (with-current-buffer (generate-new-buffer " *screenshot")
    (screenshot--setup-buffer)
    (when prettify-symbols-mode
      (prettify-symbols-mode -1))
    (current-buffer))
  "A text-only buffer for use in creating screenshots.")

(defun screenshot--format-text-only-buffer (beg end)
  "Insert text from BEG to END in the current buffer, into the screenshot text-only buffer."
  ;; include indentation if `beg' is where indentation starts
  (let ((s (string-trim-right (buffer-substring beg end))))
    (with-current-buffer (setq screenshot--buffer screenshot--text-only-buffer)
      (buffer-face-set :family screenshot-font-family
                       :height (* 10 screenshot-font-size))
      (erase-buffer)
      (insert s)
      (indent-rigidly (point-min) (point-max)
                      (- (indent-rigidly--current-indentation
                          (point-min) (point-max))))
      (current-buffer))))

(defun screenshot--narrowed-clone-buffer (beg end)
  "Create a clone of the current buffer, narrowed to the region from BEG to END.
This buffer then then set up to be used for a screenshot."
  (with-current-buffer (clone-indirect-buffer " *screenshot-clone" nil t)
    (narrow-to-region beg end)
    (screenshot--setup-buffer)
    (buffer-face-set :family screenshot-font-family
                     :height (* 10 screenshot-font-size))
    (current-buffer)))

;;; Screenshot processing

(defun screenshot--process ()
  "Perform the screenshot process.

Create a buffer for the screenshot, use `x-export-frames' to create the image,
and process it."
  (setq screenshot--buffer
        (if screenshot-text-only-p
            (screenshot--format-text-only-buffer screenshot--region-beginning screenshot--region-end)
          (screenshot--narrowed-clone-buffer screenshot--region-beginning screenshot--region-end)))

  (let ((frame (posframe-show
                screenshot--buffer
                :position (point-min)
                :internal-border-width screenshot-border-width
                :min-width screenshot-min-width
                :width screenshot-max-width
                :min-height screenshot--total-lines
                :lines-truncate screenshot-truncate-lines-p
                :poshandler #'posframe-poshandler-point-bottom-left-corner
                :hidehandler #'posframe-hide)))
    (with-current-buffer screenshot--buffer
      (setq-local display-line-numbers screenshot-line-numbers-p)
      (when screenshot-text-only-p
        (setq-local display-line-numbers-offset
                    (if screenshot-relative-line-numbers-p
                        0 (1- screenshot--first-line-number))))
      (font-lock-ensure (point-min) (point-max))
      (redraw-frame frame)
      (with-temp-file screenshot--tmp-file
        (insert (x-export-frames frame 'png))))
    (posframe-hide screenshot--buffer))
  (unless screenshot-text-only-p
    (kill-buffer screenshot--buffer))
  (screenshot--post-process screenshot--tmp-file))

(defcustom screenshot-post-process-hook
  (when (executable-find "pngquant")
    (list (defun screenshot--compress-file (file)
            (call-process "pngquant" nil nil nil "-f" "-o" file file))))
  "Functions to be called on the output file after processing.
Must take a single argument, the file name, and operate in-place."
  :type 'function
  :group 'screenshot)

(defun screenshot--post-process (file)
  "Apply any image post-processing to FILE."
  (when (or (> screenshot-radius 0)
            (> screenshot-shadow-radius 0))
    (let ((result
           (shell-command-to-string
            (format "convert '%1$s' \\( +clone -alpha extract \\
\\( -size %2$dx%2$d xc:black -draw 'fill white circle %2$d,%2$d %2$d,0' -write mpr:arc +delete \\) \\
\\( mpr:arc \\) -gravity northwest -composite \\
\\( mpr:arc -flip \\) -gravity southwest -composite \\
\\( mpr:arc -flop \\) -gravity northeast -composite \\
\\( mpr:arc -rotate 180 \\) -gravity southeast -composite \\) \\
-alpha off -compose CopyOpacity -composite -compose over \\
\\( +clone -background '%3$s' -shadow %4$dx%5$d+%6$d+%7$d \\) \\
+swap -background none -layers merge '%1$s'"
                    file
                    screenshot-radius
                    screenshot-shadow-color
                    screenshot-shadow-intensity
                    screenshot-shadow-radius
                    screenshot-shadow-offset-horizontal
                    screenshot-shadow-offset-vertical))))
      (unless (string= result "")
        (error "Could not apply imagemagick commands to image:\n%s" result))))
  (run-hook-with-args 'screenshot-post-process-hook file))

;;; Screenshot actions

(eval-when-compile
  (defmacro screenshot--def-action (name &rest body)
    "Define action NAME to be performed from the transient interface.
BODY is executed after `screenshot-process' is called."
    `(defun ,(intern (concat "screenshot-" name)) (&optional _args)
       "Screenshot action to be performed from the transient interface."
       (interactive
        (list (transient-args 'screenshot-transient)))
       (screenshot--process)
       ,@body))

  (screenshot--def-action
   "save"
   (rename-file
    screenshot--tmp-file
    (concat (file-name-sans-extension
             (or (buffer-file-name)
                 (expand-file-name "screenshot")))
            ".png")
    t)
   (message "Screenshot saved"))

  (screenshot--def-action
   "save-as"
   (rename-file
    screenshot--tmp-file
    (read-file-name "Save as: " (file-name-directory (or (buffer-file-name) default-directory)))
    1)
   (message "Screenshot saved"))

  (screenshot--def-action
   "copy"
   (call-process "xclip" nil nil nil
                 "-selection" "clipboard"
                 "-target" "image/png"
                 "-in" screenshot--tmp-file)
   (delete-file screenshot--tmp-file)
   (message "Screenshot copied"))

  (defcustom screenshot-upload-fn nil
    "Function or string which provides a method to upload a file.
If a function, it must take a filename and returns a URL to it.
If a string, it is formatted with the file name, and run as a shell command.

Note: you have to define this yourself, there is no default."
    :type '(choice function string)
    :group 'screenshot)

  (screenshot--def-action
   "upload"
   (if (not screenshot-upload-fn)
       (error "No upload function defined")
     (message "Uploading...")
     (let ((url
            (pcase screenshot-upload-fn
              ((pred functionp) (funcall screenshot-upload-fn screenshot--tmp-file))
              ((pred stringp) (string-trim-right (shell-command-to-string (format screenshot-upload-fn screenshot--tmp-file))))
              (_ (error "Upload function is not a function or string!")))))
       (gui-select-text url)
       (message "Screenshot uploaded, link copied to clipboard (%s)" url)))
   (delete-file screenshot--tmp-file)))

;;; Screenshot transient

(transient-define-prefix screenshot-transient ()
  ["Code"
   (screenshot--set-line-numbers-p)
   (screenshot--set-relative-line-numbers-p)
   (screenshot--set-text-only-p)
   (screenshot--set-truncate-lines-p)
   (screenshot--set-font-family)
   (screenshot--set-font-size)]
  ["Frame"
   (screenshot--set-border-width)
   (screenshot--set-radius)
   (screenshot--set-min-width)
   (screenshot--set-max-width)]
  ["Shadow"
   (screenshot--set-shadow-radius)
   (screenshot--set-shadow-intensity)
   (screenshot--set-shadow-color)
   (screenshot--set-shadow-offset-horizontal)
   (screenshot--set-shadow-offset-vertical)]
  ["Action"
   ("s" "Save" screenshot-save)
   ("S" "Save as" screenshot-save-as)
   ("c" "Copy" screenshot-copy)
   ("u" "Upload" screenshot-upload)])

(provide 'screenshot)
;;; screenshot.el ends here
