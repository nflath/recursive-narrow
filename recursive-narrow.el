;;; recursive-narrow.el --- narrow-to-region that operates recursively

;; Copyright (C) 2010 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/recursive-narrow
;; Version: 1.0

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
;; (global-set-key (kbd "C-x n n") 'recursive-narrow-to-region)
;; (global-set-key (kbd "C-x n w") 'recursive-widen)
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

(defun recursive-narrow-to-region (start end)
  """Replacement of narrow-to-region.  Performs the exact same
  function but also allows recursive-widen to remove just one
  call to recursive-narrow-to-region."""
  (interactive "r")
  (setq recursive-narrow-buffers
        (append (list (cons (current-buffer) (cons (point-min) (point-max))))
                recursive-narrow-buffers ))
  (narrow-to-region start end))

(defun recursive-widen ()
  """Replacement of widen that will only pop one level of visibility."""
  (interactive)
  (if (assoc (current-buffer) recursive-narrow-buffers)
      (progn
        (setq widen-to (cdr (assoc (current-buffer) recursive-narrow-buffers)))
        (setq recursive-narrow-buffers (remove
                                        (assoc (current-buffer) recursive-narrow-buffers)
                                        recursive-narrow-buffers))
        (narrow-to-region (car widen-to) (cdr widen-to))
        (recenter))
    (widen)))

(provide 'recursive-narrow)

;;; recursive-narrow.el ends here