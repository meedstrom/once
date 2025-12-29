;;; once.el --- Add-hook and eval-after-load, but only once  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/once
;; Created:  2025-12-12
;; Keywords: lisp
;; Package-Requires: ((emacs "29.1"))


;;; Commentary:

;; This library provides two main functions:

;; - `once-hook', substitute for `add-hook'
;; - `once-load', substitute for `eval-after-load'

;; They are like `add-hook' and `eval-after-load' respectively,
;; except that they only result in calling the provided function once: at the
;; next time the hook is run or the file is loaded, respectively.

;; The variants

;; - `once-hook*'
;; - `once-load*'

;; can be explained if you think of the main functions as being called
;; "pushnew-self-deleting-hook", and these just "push-self-deleting-hook".

;; The macros

;; - `once-hook!'
;; - `once-load!'

;; are meant to help with writing Emacs init-files.


;;;; Examples:

;; Unset some default keys in geiser-repl, when that file first loads:

;;     (once-load! geiser-repl
;;       (keymap-unset geiser-repl-mode-map "M-," t)
;;       (keymap-unset geiser-repl-mode-map "M-." t)
;;       (keymap-unset geiser-repl-mode-map "M-`" t))

;; Configure font after the daemon makes its first emacsclient frame:

;;     (once-hook! server-after-make-frame-hook
;;       (set-face-font 'default (font-spec :family "Iosevka Nerd Font" :size 29)))

;; By the way, setting hooks in init-files is a natural fit for the ## macro
;; from Llama.  No problems combining that with this library:

;;     (once-load 'org (##setopt org-todo-keywords '((sequence "IDEA" "DONE"))))

;; Setting up a `my-first-frame-hook':

;;     (if (daemonp)
;;         (once-hook 'server-after-make-frame-hook (##run-hooks 'my-first-frame-hook))
;;       (add-hook 'emacs-startup-hook (##run-hooks 'my-first-frame-hook)))

;; The advantage of `once-hook' plus Llama, compared to `once-hook!', is that
;; the former preserves the exact calling convention of `add-hook'.
;; That makes it trivial to rewrite from `add-hook' to `once-hook' and back.
;; Note the identical arguments:

;;     (add-hook  'enable-theme-functions (##message "Loaded theme %s" %) -50)
;;     (once-hook 'enable-theme-functions (##message "Loaded theme %s" %) -50)


;;; Code:

(require 'cl-lib)

(defvar once--counter 0)
(defvar once-functions nil
  "List of function symbols defined by `once-load', `once-hook' etc.

These functions are thin wrappers that delete themselves from the
corresponding hook after it runs, or that `fset' themselves to `ignore'
after the corresponding feature has loaded.")

(defun once--make-deterministic-name (&rest args)
  "Return a string that is unique for Lisp objects ARGS.

As with `sxhash', it is deterministic only for the current session.
If any of ARGS is a symbol, its symbol name is used, which may not
be unique enough for your purposes if `obarray' is overridden."
  (concat "once---"
          (mapconcat (lambda (arg)
                       (if (symbolp arg)
                           (symbol-name arg)
                         (number-to-string (sxhash arg))))
                     args
                     ".")))


;;;; Substitutes for add-hook

(defun once-hook (hook function &optional depth local)
  "Like `add-hook' but call FUNCTION on next run of HOOK only.
DEPTH and LOCAL as in `add-hook'.

As expected from `add-hook', this does nothing if FUNCTION is already a
member of HOOK, even if DEPTH would differ.

For alternative behavior, use `once-hook*'.  That may be a good idea
when writing Lisp for distribution, as it is simpler to reason about."
  ;; Check manually b/c input `function' is not the `wrapper' we make
  (unless (if local
              (and (boundp hook) (member function (symbol-value hook)))
            (and (default-boundp hook) (member function (default-value hook))))
    (let ((wrapper (intern (once--make-deterministic-name hook function local))))
      (unless (fboundp wrapper)
        (defalias wrapper
          (lambda (&rest args)
            (remove-hook hook wrapper local)
            (apply function args))))
      (add-hook hook wrapper depth local)
      (cl-pushnew wrapper once-functions)
      wrapper)))

(defun once-hook* (hook function &optional depth local)
  "Non-idempotent version of `once-hook'.
Think of it as `push', if `once-hook' is like `cl-pushnew'.

Repeated invocations will stack up multiple calls of FUNCTION on the
next run of HOOK.  This is because FUNCTION is wrapped in a new lambda
each time."
  (let ((wrapper (intern (format "once-%S%d" hook (cl-incf once--counter)))))
    (fset wrapper
          (lambda (&rest args)
            (remove-hook hook wrapper local)
            (setq once-functions (delq wrapper once-functions))
            (fmakunbound wrapper)
            (apply function args)))
    (add-hook hook wrapper depth local)
    (push wrapper once-functions)
    wrapper))


;;;; Substitutes for eval-after-load

(defun once-load (feature function)
  "Call FUNCTION on next load of FEATURE, or now if already loaded.

Like `eval-after-load', the effect is idempotent in that loading FEATURE
once will not call a given FUNCTION more than once, even if `once-load'
was invoked multiple times before load with the same arguments.

\(For alternative behavior there, see `once-load*'.\)

The difference from `eval-after-load' is that loading FEATURE again will
not cause FUNCTION to be called again.
In other words, loading FEATURE ten times calls FUNCTION once.

If FEATURE is already loaded, this simply calls FUNCTION and, unlike
`eval-after-load', does not also add it to `after-load-alist'.

In other words, invoking `once-load' once can only result in at most one
call of FUNCTION.

This may be sensible as a standard substitute for `eval-after-load',
since there is no easy way to undo `eval-after-load' similar to how
`remove-hook' can undo `add-hook'."
  (if (featurep feature)
      (funcall function)
    (let ((wrapper (intern (once--make-deterministic-name feature function))))
      (defalias wrapper
        (lambda ()
          (fset wrapper #'ignore)
          (funcall function)))
      (eval-after-load feature wrapper)
      (cl-pushnew wrapper once-functions)
      wrapper)))

(defun once-load* (feature function)
  "Non-idempotent version of `once-load'.
Think of it as `push', if `once-load' is like `cl-pushnew'.

Repeated invocations will stack up multiple calls of FUNCTION on the
next load of FEATURE.  This is because FUNCTION is wrapped in a new
lambda each time."
  (if (featurep feature)
      (funcall function)
    (let ((wrapper (intern (format "once-%S%d" feature (cl-incf once--counter)))))
      (fset wrapper
            (lambda ()
              (fset wrapper #'ignore)
              (funcall function)))
      (eval-after-load feature wrapper)
      (push wrapper once-functions)
      wrapper)))


;;;; Macros

(defmacro once-hook! (hook &rest body)
  "Eval BODY on next run of HOOK."
  (declare (indent 1) (debug t))
  `(once-hook ',hook (lambda () ,@body)))

(defmacro once-load! (feature &rest body)
  "Like `with-eval-after-load' but do not re-eval on re-load.
Eval BODY on next load of FEATURE, or eval now if already loaded."
  (declare (indent 1) (debug t))
  `(once-load ',feature (lambda () ,@body)))

(provide 'once)

;;; once.el ends here
