;;; screenshot.el --- Swiftly grab images of your code -*- lexical-binding: t -*-

;; Copyright (C) 2020 TEC

;; Author: TEC <http://github/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Homepage: https://github.com/tecosaur/screenshot
;; Version: 0.2.0
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
;; Requires `imagemagick' to set metadata and for some visual post-processing,
;; and `xclip' for copying images to the clipboard.

;;; Code:

(require 'transient)
(require 'posframe)
(require 'lisp-mnt)

(defgroup screenshot ()
  "Customise group for Screenshot."
  :group 'convenience)

(defcustom screenshot-buffer-creation-hook nil
  "Hook run after creating a buffer for screenshots.
Run after hardcoded setup, but before the screenshot is captured."
  :type 'hook
  :group 'screenshot)

;;; Generated variables

(defvar screenshot--region-beginning nil
  "Start of the region forming the screenshot.")
(defvar screenshot--region-end nil
  "End of the region forming the screenshot.")

(defvar screenshot--tmp-file nil
  "An intermediate target file for the screenshot.")

(defvar screenshot--first-line-number nil
  "The first line contained in the screenshot.")

;;; Screenshot parameters

(eval-when-compile
  (defmacro screenshot--define-infix (key name description type default
                                          &rest reader)
    "Define a defcustom screenshot-NAME and an associated transient infix setter.

The new variable screenshot-NAME takes the default value DEFAULT,
and is given the docstring DESCRIPTION, and declared to be of
TYPE.

The infix uses KEY and DESCRIPTION, modifies the variable
screenshot-NAME, and is set by a reader function with body
READER."
    (declare (indent 5) (doc-string 3))
    (let* ((infix-var (intern (format "screenshot-%s" name)))
           (reader
            (cond
             ((and (not reader)
                   (eq (cadr type) 'boolean))
              `((not ,infix-var)))
             ((and (memq (cadr type) '(string color number integer float))
                   (not reader))
              `((,(pcase (cadr type)
                    ((or 'string 'color) #'read-string)
                    ((or 'number 'integer 'float) #'read-number))
                 ,(concat description ": ") ,infix-var)))
             (t reader))))
      `(progn
         (defcustom ,infix-var ,default
           ,(concat description ".")
           :type ,type
           :group 'screenshot)
         (transient-define-infix ,(intern (format "screenshot--set-%s" name)) ()
           ,(format "Set `screenshot--%s' from a popup buffer." name)
           :class 'transient-lisp-variable
           :variable ',infix-var
           :key ,key
           :description ,description
           :argument ,(format "--%s" name)
           :reader (lambda (&rest _)
                     (let ((new-value (progn ,@reader)))
                       (if (equal new-value ,infix-var)
                           (message "%s unchanged" ',infix-var)
                         (prog1
                             (customize-save-variable ',infix-var new-value)
                           (message "New %s value saved" ',infix-var)))
                       new-value)))))))

(screenshot--define-infix
    "-n" line-numbers-p
    "Show line numbers"
    'boolean nil)

(screenshot--define-infix
    "-r" relative-line-numbers-p
    "Relative line numbers within the screenshot"
    'boolean nil)

(screenshot--define-infix
    "-t" text-only-p
    "Use a text-only version of the buffer"
    'boolean nil)

(screenshot--define-infix
    "-x" truncate-lines-p
    "Truncate lines beyond the screenshot width"
    'boolean nil)

(screenshot--define-infix
    "-i" remove-indent-p
    "Remove indent in selection"
    'boolean t)

(screenshot--define-infix
    "-md" code-as-image-description-p
    "Set \"description\" metadata to text in region"
    'boolean t)

(screenshot--define-infix
    "-mt" buffer-name-as-image-title-p
    "Set image \"title\" metadata to `buffer-name'"
    'boolean t)

(screenshot--define-infix
    "-ma" user-full-name-as-image-author-p
    "Set image \"author\" metadata to `user-full-name'"
    'boolean nil)

(declare-function counsel-fonts "ext:counsel-fonts")

(declare-function ivy-read "ext:ivy-read")

(screenshot--define-infix
    "-ff" font-family
    "Font family to use"
    'string (let ((font (face-attribute 'default :font)))
              (if (eq font 'unspecified) "monospace"
                (symbol-name (font-get font :family))))
  (completing-read
   "Font: "
   (mapcar
    (lambda (f) (propertize f 'face (list :family f)))
    ;; TODO strip non-ascii fonts
    (delete-dups (font-family-list)))
   nil t nil nil screenshot-font-family))

(screenshot--define-infix
    "-fs" font-size
    "Font size in pt"
    'number 14)

;;;; Frame

(screenshot--define-infix
    "-bw" border-width
    "Border width in px"
    'integer 20)

(screenshot--define-infix
    "-br" radius
    "Rounded corner radius in px"
    'integer 10)

(screenshot--define-infix
    "-w" min-width
    "Minimum width, in columns"
    'integer 40)

(screenshot--define-infix
    "-W" max-width
    "Maximum width, in columns"
    'integer 120)

;;;; Shadow

(screenshot--define-infix
    "-sr" shadow-radius
    "Radius of the shadow in px"
    'integer 12)

(screenshot--define-infix
    "-si" shadow-intensity
    "Intensity of the shadow (0-100)"
    'integer 80)

(screenshot--define-infix
    "-sc" shadow-color
    "Color of the shadow (hex string)"
    'color "#333")

(screenshot--define-infix
    "-sx" shadow-offset-horizontal
    "Shadow horizontal offset in px"
    'integer -8)

(screenshot--define-infix
    "-sy" shadow-offset-vertical
    "Shadow vertical offset in px"
    'integer 5)

;;; Main function

;;;###autoload
(defun screenshot (beg end &optional upload-text)
  "Take a screenshot of the current region or buffer.

Region included in screenshot is the active selection, interactively,
or given by BEG and END.  Buffer is used if region spans 0-1 characters.

When a universal argument is given, UPLOAD-TEXT is non-nil.
Then the text of the region/buffer is uploaded, and the URL is copied
to the clipboard."
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
    (setq end (1+ (re-search-backward "[^[:space:]\n]"))))
  (setq screenshot--region-beginning beg
        screenshot--region-end end
        screenshot--first-line-number (line-number-at-pos beg)))

(declare-function solaire-mode "ext:solaire-mode")

(declare-function hl-line-mode "ext:hl-line")

(defun screenshot--setup-buffer ()
  "Modify the current buffer to make it appropriate for screenshotting."
  (setq-local face-remapping-alist '((line-number-current-line line-number)
                                     (show-paren-match nil)
                                     (region nil))
              line-spacing 0.1)
  (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))
  (when (bound-and-true-p solaire-mode) (solaire-mode -1))
  (when (bound-and-true-p winner-mode) (winner-mode -1))
  (run-hooks 'screenshot-buffer-creation-hook))

(defvar screenshot--text-only-buffer
  (with-current-buffer (generate-new-buffer " *screenshot")
    (screenshot--setup-buffer)
    (when prettify-symbols-mode
      (prettify-symbols-mode -1))
    (current-buffer))
  "A text-only buffer for use in creating screenshots.")

(defun screenshot--format-text-only-buffer (beg end)
  "Insert text from BEG to END in the current buffer, into the screenshot buffer."
  ;; include indentation if `beg' is where indentation starts
  (let ((s (string-trim-right (buffer-substring beg end))))
    (with-current-buffer screenshot--text-only-buffer
      (buffer-face-set :family screenshot-font-family
                       :height (* 10 screenshot-font-size))
      (erase-buffer)
      (insert s)
      (current-buffer))))

(defun screenshot--narrowed-clone-buffer (beg end)
  "Create a clone of the current buffer, narrowed to the region from BEG to END.
This buffer then then set up to be used for a screenshot."
  (let ((hl (bound-and-true-p hl-line-mode)))
    (when hl (hl-line-mode -1))
    (prog1
        (with-current-buffer (make-indirect-buffer (current-buffer) " *screenshot-clone" t t)
          (narrow-to-region beg end)
          (screenshot--setup-buffer)
          (buffer-face-set :family screenshot-font-family
                           :height (* 10 screenshot-font-size))
          (current-buffer))
      (when hl (hl-line-mode 1)))))

(defun screenshot--max-line-length (&optional buffer)
  "Find the maximum line length in BUFFER."
  (let ((max-line 0))
    (with-current-buffer (or buffer (current-buffer))
      (save-excursion
        (goto-char (point-min))
        (dotimes (line-1 (count-lines (point-min) (point-max)))
          (setq max-line (max max-line
                              (- (line-end-position (1+ line-1))
                                 (line-beginning-position (1+ line-1))))))))
    max-line))

(defun screenshot--displayed-lines (&optional buffer)
  "Count the number of (perhaps wrapped) lines displyed in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if screenshot-truncate-lines-p
        (count-lines (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (let ((displayed-lines 0))
          (dotimes (line-1 (count-lines (point-min) (point-max)))
            (setq displayed-lines
                  (+ displayed-lines
                     (max 1 (ceiling (- (line-end-position (1+ line-1))
                                        (line-beginning-position (1+ line-1)))
                                     screenshot-max-width)))))
          displayed-lines)))))

;;; Screenshot processing

(defun screenshot--process (beg end)
  "Perform the screenshot process on the region BEG to END.

More specifically, this function will:
- Create a buffer for the screenshot
- Save it as an image
- Process the image"
  (let ((ss-buf
         (if screenshot-text-only-p
             (screenshot--format-text-only-buffer beg end)
           (screenshot--narrowed-clone-buffer beg end)))
        (indent-level 0))
    (when screenshot-remove-indent-p
      (with-current-buffer ss-buf
        (setq indent-level (indent-rigidly--current-indentation
                            (point-min) (point-max)))
        (when (> indent-level 0)
          (indent-rigidly (point-min) (point-max)
                          (- indent-level)))))
    (unwind-protect
        (progn
          (screenshot--process-buffer ss-buf)
          (screenshot--post-process screenshot--tmp-file))
      (when (and screenshot-remove-indent-p
                 (not screenshot-text-only-p)
                 (> indent-level 0))
        (with-current-buffer ss-buf
          (indent-rigidly (point-min) (point-max)
                          indent-level)))
      (unless (eq ss-buf screenshot--text-only-buffer)
        (kill-buffer ss-buf)))))

(defun screenshot--process-buffer (ss-buf)
  "Save a screenshot of SS-BUF to `screenshot--tmp-file' via `x-export-frames'."
  (let* (before-make-frame-hook
         delete-frame-functions
         (width (max screenshot-min-width
                     (min screenshot-max-width
                          (screenshot--max-line-length
                           ss-buf))))
         (height (screenshot--displayed-lines ss-buf))
         (frame (posframe-show
                 ss-buf
                 :position (point-min)
                 :internal-border-width screenshot-border-width
                 :min-width width
                 :width width
                 :max-width width
                 :min-height height
                 :height height
                 :max-height height
                 :lines-truncate screenshot-truncate-lines-p
                 :poshandler #'posframe-poshandler-point-bottom-left-corner
                 :hidehandler #'posframe-hide)))
    (with-current-buffer ss-buf
      (setq-local display-line-numbers screenshot-line-numbers-p)
      (when screenshot-text-only-p
        (setq-local display-line-numbers-offset
                    (if screenshot-relative-line-numbers-p
                        0 (1- screenshot--first-line-number))))
      (font-lock-ensure (point-min) (point-max))
      (redraw-frame frame)
      (with-temp-file screenshot--tmp-file
        (insert (x-export-frames frame 'png))))
    (posframe-hide ss-buf)))

(defcustom screenshot-post-process-hook
  (append (and (executable-find "pngquant") (list #'screenshot--compress-file))
          (list #'screenshot--set-metadata))
  "Functions to be called on the output file after processing.
Must take a single argument, the file name, and operate in-place."
  :type 'function
  :group 'screenshot)

(defun screenshot--compress-file (file)
  "Compress FILE with pngquant."
  (call-process "pngquant" nil nil nil "--force" "--skip-if-larger" "--output" file file))

(defun screenshot--set-metadata (file)
  "Set requested metadata on FILE."
  (let ((result (apply #'call-process "convert" nil nil nil
                       (append (list file)
                               (and screenshot-code-as-image-description-p
                                    (list "-set" "description" (buffer-substring
                                                                screenshot--region-beginning
                                                                screenshot--region-end)))
                               (and screenshot-buffer-name-as-image-title-p
                                    (list "-set" "title" (buffer-name)))
                               (and screenshot-user-full-name-as-image-author-p
                                    (list "-set" "author" user-full-name))
                               (list
                                "-set" "software" (format "Emacs %s; screenshot.el %s"
                                                          emacs-version
                                                          (lm-version (symbol-file 'screenshot)))
                                file)))))
    (unless (zerop result)
      (error "Could not apply imagemagick commands to image (exit code %d)" result))))

(defun screenshot--post-process (file)
  "Apply any image post-processing to FILE."
  (when (or (> screenshot-radius 0)
            (> screenshot-shadow-radius 0))
    (let ((result
           (apply #'call-process
                  "convert"
                  nil nil nil
                  (delq
                   nil
                   (append
                    (list file)
                    (and (> screenshot-radius 0)
                         (list "(" "+clone" "-alpha" "extract"
                               "(" "-size" (format "%1$dx%1$d" screenshot-radius)
                               "xc:black"
                               "-draw" (format "fill white circle %1$d,%1$d %1$d,0"
                                               screenshot-radius)
                               "-write" "mpr:arc" "+delete" ")"
                               "(" "mpr:arc" ")" "-gravity" "northwest" "-composite"
                               "(" "mpr:arc" "-flip" ")" "-gravity" "southwest" "-composite"
                               "(" "mpr:arc" "-flop" ")" "-gravity" "northeast" "-composite"
                               "(" "mpr:arc" "-rotate" "180" ")" "-gravity" "southeast" "-composite" ")"
                               "-alpha" "off"
                               "-compose" "CopyOpacity"
                               "-composite" "-compose" "over"))
                    (and (> screenshot-shadow-radius 0)
                         (list "(" "+clone" "-background" screenshot-shadow-color
                               "-shadow" (format "%dx%d+%d+%d"
                                                 screenshot-shadow-intensity
                                                 screenshot-shadow-radius
                                                 screenshot-shadow-offset-horizontal
                                                 screenshot-shadow-offset-vertical)
                               ")" "+swap"))
                    (list
                     "-background" "none"
                     "-layers" "merge"
                     file))))))
      (unless (eq result 0)
        (error "Could not apply imagemagick commands to image (exit code %d)" result))))
  (run-hook-with-args 'screenshot-post-process-hook file))

;;; Screenshot actions

(eval-when-compile
  (defmacro screenshot--def-action (name &optional docstring &rest body)
    "Define action NAME to be performed from the transient interface.
This defines a function screenshot-NAME with DOCSTRING which executes BODY after
determining (and binding) the region beg/end and calling `screenshot--process'.
If BODY starts with :no-img then `screenshot--process' is not called."
    (declare (doc-string 2) (indent defun))
    `(defun ,(intern (format "screenshot-%s" name)) (beg end)
       ,(concat
         (if (stringp docstring)
             (concat docstring "\n\n")
           (push docstring body)
           "")
         "Screenshot action to be performed from the transient interface.")
       (interactive
        (progn
          (unless (eq transient-current-command 'screenshot-transient)
            (if (region-active-p)
                (screenshot--set-screenshot-region (region-beginning) (region-end))
              (screenshot--set-screenshot-region
               (line-beginning-position) (line-end-position)))
            ,@(and (not (eq (car body) :no-img))
                   '((setq screenshot--tmp-file
                           (make-temp-file "screenshot-" nil ".png")))))
          (list screenshot--region-beginning screenshot--region-end)))
       ,@(if (eq (car body) :no-img)
             (progn (pop body) nil)
           '((screenshot--process beg end)))
       ,@body)))

(screenshot--def-action save
  "Save the current selection (BEG-END) as an image."
  (rename-file
   screenshot--tmp-file
   (concat (file-name-sans-extension
            (or (buffer-file-name)
                (expand-file-name "screenshot")))
           ".png")
   t)
  (message "Screenshot saved"))

(screenshot--def-action save-as
  "Save the current selection (BEG-END) as an image in the specified location."
  (rename-file
   screenshot--tmp-file
   (read-file-name "Save as: " (file-name-directory (or (buffer-file-name) default-directory)))
   1)
  (message "Screenshot saved"))

(screenshot--def-action copy
  "Copy the current selection (BEG-END) as an image to the clipboard."
  (let ((wayland-p (getenv "WAYLAND_DISPLAY")))
    (cond
     ((and wayland-p (executable-find "wl-copy"))
      (call-process "wl-copy" screenshot--tmp-file nil nil
                    "--type" "image/png"))
     ((and (not wayland-p) (executable-find "xclip"))
      (call-process "xclip" nil nil nil
                    "-selection" "clipboard"
                    "-target" "image/png"
                    "-in" screenshot--tmp-file))
     (t
      (user-error "Missing `%s' executable, needed to copy images to the clipboard"
                  (if wayland-p "wl-copy" "xclip")))))
  (delete-file screenshot--tmp-file)
  (message "Screenshot copied"))

(screenshot--def-action text-copy
  "Copy the current selection (BEG-END) as text to the clipboard."
  :no-img
  (let ((content (string-trim-right (buffer-substring beg end))))
    (with-temp-buffer
      (insert content)
      (when screenshot-remove-indent-p
        (indent-rigidly (point-min) (point-max)
                        (- (indent-rigidly--current-indentation
                            (point-min) (point-max)))))
      (kill-new (buffer-substring-no-properties
                 (point-min) (point-max))))))

(defcustom screenshot-upload-fn nil
  "Function or string which provides a method to upload a file.
If a function, it must take a filename and returns a URL to it.
If a string, it is formatted with the file name, and run as a shell command.

Note: you have to define this yourself, there is no default."
  :type '(choice function string)
  :group 'screenshot)

(screenshot--def-action upload
  "Upload an image of the current selection (BEG-END) via `screenshot-upload-fn'."
  (if (not screenshot-upload-fn)
      (error "No upload function defined")
    (message "Uploading...")
    (let ((url
           (pcase screenshot-upload-fn
             ((pred functionp) (funcall screenshot-upload-fn screenshot--tmp-file))
             ((pred stringp) (string-trim-right (shell-command-to-string (format screenshot-upload-fn screenshot--tmp-file))))
             (_ (error "Upload function is not a function or string!")))))
      (kill-new url)
      (message "Screenshot uploaded, link copied to clipboard (%s)"
               (propertize url 'face 'link))))
  (delete-file screenshot--tmp-file))

(defcustom screenshot-text-upload-fn #'screenshot-ixio-upload
  "Function to use to upload text.

Must take a start and end position for the current buffer, and
return a URL."
  :type 'function
  :group 'screenshot)

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

(screenshot--def-action text-upload
  "Upload the current selection (BEG-END) as text via `screenshot-text-upload-fn'."
  (message "Uploading text...")
  (let ((content (string-trim-right (buffer-substring beg end)))
        url)
    (with-temp-buffer
      (insert content)
      (when screenshot-remove-indent-p
        (indent-rigidly (point-min) (point-max)
                        (- (indent-rigidly--current-indentation
                            (point-min) (point-max)))))
      (setq url (funcall screenshot-text-upload-fn (point-min) (point-max)))
      (kill-new url)
      (message "Screenshot uploaded, link copied to clipboard (%s)"
               (propertize url 'face 'link)))))

;;; Screenshot transient

(transient-define-prefix screenshot-transient ()
  "Transient that should only ever be invoked by `screenshot'."
  ["Code"
   (screenshot--set-line-numbers-p)
   (screenshot--set-relative-line-numbers-p)
   (screenshot--set-text-only-p)
   (screenshot--set-truncate-lines-p)
   (screenshot--set-remove-indent-p)
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
  ["Metadata"
   (screenshot--set-code-as-image-description-p)
   (screenshot--set-buffer-name-as-image-title-p)
   (screenshot--set-user-full-name-as-image-author-p)]
  ["Action"
   ["Save"
    ("s" "Save image" screenshot-save)
    ("S" "Save image as" screenshot-save-as)]
   ["Upload"
    ("u" "Image" screenshot-upload)
    ("U" "Text" screenshot-text-upload)]
   ["Copy"
    ("c" "Image" screenshot-copy)
    ("C" "Text" screenshot-text-copy)]])

(provide 'screenshot)
;;; screenshot.el ends here
