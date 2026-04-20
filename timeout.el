;;; timeout.el --- Throttle or debounce Elisp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, extensions
;; Version: 2.1.6
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/karthink/timeout

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; timeout is a small Elisp library that provides higher order functions to
;; throttle or debounce Elisp functions.  This is useful for corralling
;; over-eager code that:
;; (i) is slow and blocks Emacs, and
;; (ii) does not provide customization options to limit how often it runs,
;;
;; To throttle a function FUNC to run no more than once every 2 seconds, run
;; (timeout-throttle 'func 2.0)
;;
;; To debounce a function FUNC to run after a delay of 0.3 seconds, run
;; (timeout-debounce 'func 0.3)
;;
;; To create a new throttled or debounced version of FUNC instead, run
;;
;; (timeout-throttled-func 'func 2.0)
;; (timeout-debounced-func 'func 0.3)
;;
;; You can bind this via fset or defalias:
;;
;; (defalias 'throttled-func (timeout-throttled-func 'func 2.0))
;; (fset     'throttled-func (timeout-throttled-func 'func 2.0))
;;
;; Dynamic duration is supported by passing a symbol or function instead of
;; a number:
;;
;; (defvar my-timeout 1.5)
;; (timeout-throttle 'func 'my-timeout)  ; uses value of my-timeout
;; (timeout-throttle 'func (lambda () (if busy-p 0.1 2.0)))  ; conditional
;;
;; The interactive spec and documentation of FUNC is carried over to the new
;; function.

;;; Code:
(require 'nadvice)
(require 'cl-generic)

(define-obsolete-function-alias 'timeout-throttle! 'timeout-throttle "v2.0")
(define-obsolete-function-alias 'timeout-debounce! 'timeout-debounce "v2.0")

(oclosure-define timeout
  "Timeout closure."
  type func delay
  (timer :mutable t)
  (default :mutable t)
  (args :mutable t)
  doc)

(cl-defmethod function-documentation ((fun timeout))
  (concat (timeout--doc fun)
          "\n\n"
          (if (eq (timeout--type fun) 'debounce)
              "Debounce"
            "Throttle")
          " calls to this function"))

(defsubst timeout--eval-value (value)
  "Eval a VALUE.
If value is a function (either lambda or a callable symbol), eval the
function (with no argument) and return the result.  Else if value is a
symbol, return its value.  Else return itself."
  (cond ((numberp value) value)
        ((functionp value) (funcall value))
        ((and (symbolp value) (boundp value)) (symbol-value value))
        (t (error "Invalid value %s" value))))


(defmacro timeout--lambda
    (type advice-p func delay &optional default interactive)
  "Create a timeout lambda for FUNC with DELAY.
TYPE, ADVICE-P and INTERACTIVE should be compile-time constants.
TYPE is \\='throttle or \\='debounce. If INTERACTIVE is non-nil, create
a command. If ADVICE-P create an around advice.
DEFAULT is the default value to initialize."
  (declare (indent 2))
  (let* ((arglist (if advice-p
                      '(old-fn &rest new-args)
                    '(&rest new-args)))
         (call-fun (if advice-p 'old-fn 'func))
         (interactive-form
          (when interactive
            '(interactive
              (advice-eval-interactive-spec
               (cadr (interactive-form func))))))
         (body
          (pcase type
            ('throttle
             `(progn
                (unless (memq timer timer-list)
                  (setq default (apply ,call-fun new-args))
                  (timer-set-time
                   timer
                   (timer-relative-time nil
                                        (timeout--eval-value delay)))
                  (timer-set-function timer #'ignore)
                  (timer-activate timer))
                default))

            ('debounce
             `(prog1 default
                (setq args new-args)
                (when (memq timer timer-list)
                  (cancel-timer timer))
                (timer-set-time
                 timer
                 (timer-relative-time nil
                                      (timeout--eval-value delay)))
                (timer-set-function
                 timer
                 (lambda (buf)
                   (setq default
                         (if (buffer-live-p buf)
                             (with-current-buffer buf
                               (apply ,call-fun args))
                           (apply ,call-fun args))))
                 (list (current-buffer)))
                (timer-activate timer)))
            (_ (error "Invalid timeout type: %S" type)))))
    `(oclosure-lambda
         (timeout
          (type ',type)
          (func ,func)
          (delay ,delay)
          (timer (timer-create))
          (default ,default)
          (args nil)
          (doc (documentation ,func)))
         ,arglist
       ,interactive-form
       ,body)))

;;;###autoload
(defun timeout-debounce (func &optional delay default)
  "Debounce FUNC by making it run DELAY seconds after it is called.

This advises FUNC, when called (interactively or from code), to
run after DELAY seconds.   If FUNC is called again within this time,
the timer is reset.

DELAY defaults to 0.5 seconds.  DELAY can be a number, a symbol (whose
value is a number), or a function (that evaluates to a number).  When
passed a symbol or function, it is evaluated at runtime for dynamic
duration.  Using a delay of 0 removes any debounce advice.

The function returns immediately with value DEFAULT when called the
first time.  On future invocations, the result from the previous call is
returned."
  (if (and delay (eq delay 0))
      (progn
        (when-let* ((ad  (advice-member-p 'debounce func))
                    (_   (eq (oclosure-type (advice--car ad)) 'timeout))
                    (tmr (timeout--timer (advice--car ad))))
          (cancel-timer tmr))
        (advice-remove func 'debounce))
    (let ((ad (timeout--lambda debounce t func (or delay 0.50) default)))
      (advice-add func :around ad
                  `((name . debounce)
                    (depth . -99)))
      ad)))

;;;###autoload
(defun timeout-throttle (func &optional throttle)
  "Make FUNC run no more frequently than once every THROTTLE seconds.

THROTTLE defaults to 1 second.  THROTTLE can be a number, a
symbol (whose value is a number), or a function (that evaluates to a
number).  When passed a symbol or function, it is evaluated at runtime
for dynamic duration.  Using a throttle of 0 removes any throttle
advice.

When FUNC does not run because of the throttle, the result from the
previous successful call is returned."
  (if (and throttle (eq throttle 0))
      (advice-remove func 'throttle)
    (let ((ad (timeout--lambda throttle t func (or throttle 1.0))))
      (advice-add func :around ad
                  `((name . throttle)
                    (depth . -98)))
      ad)))

(defun timeout-debounced-func (func &optional delay default)
  "Return a debounced version of function FUNC.

The debounced function runs DELAY seconds after it is called.  DELAY
defaults to 0.5 seconds.  DELAY can be a number, a symbol (whose value
is a number), or a function (that evaluates to a number).  When passed
a symbol or function, it is evaluated at runtime for dynamic duration.

The function returns immediately with value DEFAULT when called the
first time.  On future invocations, the result from the previous call is
returned."
  (if (commandp func)
      (timeout--lambda debounce nil func (or delay 0.50) default t)
    (timeout--lambda debounce nil func (or delay 0.50) default nil)))

(defun timeout-throttled-func (func &optional throttle)
  "Return a throttled version of function FUNC.

The throttled function runs no more frequently than once every THROTTLE
seconds.  THROTTLE defaults to 1 second.  THROTTLE can be a number, a
symbol (whose value is a number), or a function (that evaluates to a
number).  When passed a symbol or function, it is evaluated at runtime
for dynamic duration.

When FUNC does not run because of the throttle, the result from the
previous successful call is returned."
  (if (commandp func)
      (timeout--lambda throttle nil func (or throttle 1.0) nil t)
    (timeout--lambda throttle nil func (or throttle 1.0) nil nil)))

(provide 'timeout)
;;; timeout.el ends here
