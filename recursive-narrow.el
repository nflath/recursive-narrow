;;; recursive-narrow.el --- narrow-to-region that operates recursively

;; Copyright (C) 2010 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/recursive-narrow
;; Version: 1.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package defines two functions, recursive-narrow-to-region and
;; recursive-widen that replace the builtin functions narrow-to-region and
;; widen.  These functions operate the same way, except in the case of multiple
;; calls to recursive-narrow-to-region.  In this case, recursive-widen will go
;; to the previous buffer visibility, not make the entire buffer visible.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'recursive-narrow)
;; (global-set-key (kbd "C-x n n") 'recursive-narrow-or-widen-dwim)
;; (global-set-key (kbd "C-x n w") 'recursive-widen-dwim)
;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(setq recursive-narrow-buffers nil)
(setq recursive-narrow-to nil)
(setq recursive-narrow-in nil)
(require 'cl)

(defun recursive-narrow-or-widen-dwim ()
  "If the region is active, narrow to that regoin.
Otherwise, narrow to the current function
or org subtree.  If this has no effect, widen the buffer."
  (interactive)
  (let ((old-recursive-narrow-buffers (list-length recursive-narrow-buffers))
        (recursive-state-changed t))
    (cond ((region-active-p) (recursive-narrow-to-region (region-beginning) (region-end)))
          ((derived-mode-p 'prog-mode) (narrow-to-defun))
          ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
          (t (setq recursive-state-changed nil)))
    (if (or (not recursive-state-changed)
            (= (list-length recursive-narrow-buffers) old-recursive-narrow-buffers))
        (progn
          (print (list-length recursive-narrow-buffers))
          (recursive-widen)))))

(defun recursive-narrow-to-region (start end)
  "Replacement of `narrow-to-region'.
Performs the exact same
function but also allows `recursive-widen' to remove just one
  call to `recursive-narrow-to-region'.
Argument START beginning of region.
Argument END end of region."
  (interactive "r")
  (let
      ((recursive-narrow-in t)
       (current-narrow-settings (assoc (current-buffer) recursive-narrow-to)))
    (unless (and current-narrow-settings
             (= (car (cdr current-narrow-settings)) start)
             (= (cdr (cdr current-narrow-settings)) end))
      (setq recursive-narrow-buffers
            (append (list (cons (current-buffer) (cons (point-min) (point-max))))
                    recursive-narrow-buffers))
      (setq recursive-narrow-to
            (append (list (cons (current-buffer) (cons start end))
                    recursive-narrow-to)))
      (narrow-to-region start end))))

(defun recursive-widen ()
  "Replacement of widen that will only pop one level of visibility."
  (interactive)
  (let ((recursive-narrow-in t))
    (if (assoc (current-buffer) recursive-narrow-buffers)
        (progn
          (setq widen-to (cdr (assoc (current-buffer) recursive-narrow-buffers)))
          (setq recursive-narrow-buffers (remove
                                          (assoc (current-buffer) recursive-narrow-buffers)
                                          recursive-narrow-buffers))
          (setq recursive-narrow-to (remove
                                     (assoc (current-buffer) recursive-narrow-to)
                                          recursive-narrow-to))
          (narrow-to-region (car widen-to) (cdr widen-to))
          (recenter))
      (widen))))

(defadvice narrow-to-region (around potentially-recursive activate)
  (if recursive-narrow-in
      ad-do-it
    (recursive-narrow-to-region (ad-get-arg 0) (ad-get-arg 1))))

(defadvice widen (around potentially-recursive activate)
  (if recursive-narrow-in
      ad-do-it
    (recursive-widen)))

(global-set-key (kbd "C-x n w") 'recursive-widen)
(global-set-key (kbd "C-x n n") 'recursive-narrow-or-widen-dwim)

(provide 'recursive-narrow)

;;; recursive-narrow.el ends here
