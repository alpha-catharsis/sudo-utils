;;; alpha-sudo.el --- Sudo utilities for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Alpha Catharsis

;; Author: Alpha Catharsis <alpha.catharsis@gmail.com>
;; Maintainer: Alpha Catharsis <alpha.catharsis@gmail.com>
;; Version: 0.0.1
;; Keywords: linux
;; URL: https://github.com/alpha-catharsis/alpha-sudo.el
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; alpha-sudo provides some useful interactive and non-interactive functions
;; for working with sudo. The package documentation is available here:
;;
;; https://github.com/alpha-catharsis/alpha-sudo
;;
;; Please report bugs, requests and suggetions to
;; Alpha Catharsis <alpha.catharsis@gmail.com> or on Github.

;;; Code:

(require 'subr-x)

;; Internal generic functions

(defun alpha--user-shell ()
  "Return the user shell program."
  (let ((shell (getenv "SHELL")))
    (or shell "/bin/bash")))

(defun alpha--call-process (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun alpha--shell-command-async (name command sentinel)
  "Run shell command COMMAND asyncronously and call function FUNC.
The function has as parameters the name NAME, the command exit code and
the command output."
  (make-process :name name
                :buffer (generate-new-buffer-name (concat "*sudo-" name "*"))
                :command (list (alpha--user-shell) "-c" command)
                :stderr nil
                :sentinel (lambda (process _event)
                            (let ((exitcode (process-exit-status process))
                                  (buffer (process-buffer process)))
                              (with-current-buffer buffer
                                (funcall sentinel name
                                         exitcode (buffer-string)))
                              (kill-buffer buffer)))))

(defun alpha--shell-program-path (program)
  "Return the path of shell program PROGRAM. If it cannot be found return nil."
  (let ((result (alpha--call-process "which" program)))
    (if (= 0 (car result))
        (string-trim-right (cadr result))
        nil)))

(defun alpha--shell-program-full-path (program)
  "Return the full path of the shell program PROGRAM.
Return nil if the program does not exist."
  (let ((real-cmd (or (alpha--shell-program-path program)
                     program)))
       (if (file-exists-p real-cmd)
           real-cmd
         nil)))

;; Internal alpha-sudo functions

(defun alpha-sudo--interactive-sentinel (name exitcode output)
  "Handle the result of an interactive sudo command."
  (with-current-buffer (generate-new-buffer (concat "*sudo-" name "*"))
    (when (/= 0 exitcode)
      (insert (format "Processe exited with error code: %d\n" exitcode)))
    (insert output)
    (if (> (count-lines (point-min) (point-max)) 5)
        (progn
          (read-only-mode)
          (switch-to-buffer-other-window (current-buffer)))
      (progn
        (message (buffer-string))
        (kill-buffer)))))

;; Public functions

(defun alpha-sudo-path ()
  "Return the path to sudo."
  (alpha--shell-program-path "sudo"))

(defun alpha-can-sudo-p (command)
  "Check if user can run COMMAND with sudo."
  (let ((sudo-path (alpha-sudo-path)))
    (if sudo-path
        (let ((result (alpha--call-process sudo-path "-l" command)))
          (= 0 (car result)))
      nil)))

(defun alpha-require-sudo-password-p (command)
  "Check if password is needed for running COMMAND with sudo."
  (when (not (alpha-can-sudo-p command))
    (error "User cannot execute command `%s' via sudo" command))
  (let ((cmd-with-passwd (alist-get 'nopasswd (alpha-sudo-allowed-commands))))
    (not (or (string= (car cmd-with-passwd) "ALL")
             (member command cmd-with-passwd)))))

(defun alpha-sudo-allowed-commands ()
    "Return the commands that the user can execute with sudo.
The results is an alist containing with key 'nopasswd the commands that can be
executed withouth password and with key 'passwd the commands that requiring
password. \"ALL\" indicates that the user can run all commands."
    (let ((sudo-path (alpha-sudo-path)))
      (if sudo-path
          (let ((sudo-result (alpha--call-process sudo-path "-nl"))
                (result ()))
            (if (= 0 (car sudo-result))
                (let ((lines (cdr (split-string (cadr sudo-result) "\n" t))))
                  (dolist (line lines result)
                    (let ((fields (split-string line)))
                      (when (or (string= (car fields) "(ALL)")
                                (string= (car fields) "(root)"))
                        (let ((commands
                               (mapcar (lambda (x)
                                         (replace-regexp-in-string "," "" x))
                                       (cdr fields))))
                          (if (string= "NOPASSWD:" (cadr fields))
                              (setq result
                                    (append result
                                            (list
                                             (cons 'nopasswd (cdr commands)))))
                            (setq result
                                  (append result
                                          (list (cons 'passwd commands))))))))))
              nil))
        nil)))

(defun alpha-sudo-exec (command sentinel &optional password)
  "Execute shell command COMMAND as root via SUDO.
At the end of the execution the SENTINEL function is called.
Password PASSWORD can be passed as optional argument."
  (let ((progname (car (split-string command))))
    (if password
        (alpha--shell-command-async progname
                                   (concat "/bin/echo "
                                           (shell-quote-argument password)
                                           " | "
                                           (alpha-sudo-path)
                                           " -S "
                                           command)
                                   sentinel)
      (alpha--shell-command-async progname
                                 (concat (alpha-sudo-path) " -n " command)
                                 sentinel))))

(defun alpha-sudo-shell-command (command)
  "Execute shell command COMMAND as root via sudo.
The password is requested only if needed."
  (interactive "MShell command (root): ")
  (let ((real-cmd (alpha--shell-program-full-path
                   (car (split-string command)))))
    (if (not (and real-cmd
                  (alpha-can-sudo-p real-cmd)))
        (error "Cannot exectute command `%s' via sudo" command)
      (if (alpha-require-sudo-password-p real-cmd)
          (alpha-sudo-exec command #'alpha-sudo--interactive-sentinel
                      (read-passwd "Enter password: "))
        (alpha-sudo-exec command #'alpha-sudo--interactive-sentinel)))))

;; Package provision

(provide 'alpha-sudo)

;;; alpha-sudo.el ends here
