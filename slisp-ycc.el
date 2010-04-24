;;; slisp.el --- Lisp interpreter

;; Copyright (C) 2010  Takeshi Sodeyama

;; Author: Takeshi Sodeyama <ta.sode@gmail.com>
;; Keywords: Lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl)

(setq max-specpdl-size 4096)
(setq max-lisp-eval-depth 4096)

(setq slisp-filename "sample.sl")

(setq slisp-type-list '("(" ")"))
(setq slisp-start-paren "(")
(setq slisp-end-paren ")")
(setq slisp-double-quote "\"")
(setq slisp-ignore-list '("\t" "\r" "\n" " "))

(setq slisp-primitive-list
      '("list" "setq" "let" "defun"
        "if" "cond" "lambda" "apply"
        "top-progn" "progn" "quote" "print"
        "+" "-" "*" "/" "=" ">" "<"
        "not" "or" "and"
        "call/cc" "throw"))
(setq slisp-sequence-top-symbol "top-progn")
(setq slisp-callcc-symbol "call/cc")

(setq slisp-environment (make-hash-table :test #'equal))


(defun gethash-keys (hash)
  (loop for k being the hash-keys in hash collect k))

(defun slisp-copy-hash (srcenv destenv)
  (dolist (key (gethash-keys srcenv))
    (if (eq (gethash key destenv) nil)
        (puthash key (gethash key srcenv) destenv))))

(defun slisp-clear-messages ()
  (let ((buffer (get-buffer "*Messages*"))
        (cur (current-buffer)))
    (progn
      (set-buffer buffer)
      (erase-buffer)
      (switch-to-buffer cur))))

(defun slisp-contains (x list)
  (loop for y in list
        thereis (equal x y)))

(defmacro with-gensyms (syms &rest body)
  (declare (indent 1))
  `(let ,(mapcar
          (lambda (sym)
            `(,sym (gensym)))
          syms)
     ,@body))

(defun slisp-get-src-string (filename)
  (let ((pre-buffer (current-buffer))
        (ret ""))
    (find-file filename)
    (setq str (buffer-substring-no-properties (point-min) (point-max)))
    (let ((lines (split-string str "\n")))
      (dolist (line lines)
        (if (not (string-match "^;" line))
            (setq ret (concat ret line)))))
    (switch-to-buffer pre-buffer)
    ret))

(defmacro slisp-set-tokenbuf-and-clear (buf tokens)
  `(if (not (equal ,buf ""))
       (progn
         (push ,buf ,tokens)
         (setq ,buf ""))))

(defun slisp-get-tokens (in_str)
  (let ((chars (split-string in_str ""))
        (token_buf "")
        (tokens nil)
        (in_double_quote nil))
    (dolist (c chars)
      (if (equal c slisp-double-quote)
          (if in_double_quote
              (setq in_double_quote nil)
            (setq in_double_quote t)))
      (cond ((slisp-contains c slisp-type-list)
             (progn
               (slisp-set-tokenbuf-and-clear token_buf tokens)
               (push c tokens)))
            ((and (not in_double_quote) (slisp-contains c slisp-ignore-list))
             (slisp-set-tokenbuf-and-clear token_buf tokens))
            (t
             (setq token_buf (concat token_buf c)))))
    (nreverse tokens)))

(defun slisp-check-primitive (token)
  (slisp-contains token slisp-primitive-list))

(defun slisp-check-type (token type)
  (cond ((equal type "string") (string-match "^\"\\(.+\\)\"$" token))
        ((equal type "number") (string-match "^\\([0-9]+\\)$" token))
        ((equal type "symbol") (string-match "\\\w+" token))
        ((equal type "boolean") (string-match "^\\(t\\|nil\\)$" token))
        ((equal type "quote") (string-match "\'.+" token))))

(defun slisp-parse (tokens)
  (let ((stack '()))
    (dolist (token tokens)
      (cond ((equal token slisp-start-paren)
             (push '() stack))
            ((equal token slisp-end-paren)
             (let* ((cur_stack (pop stack))
                    (up_stack (pop stack)))
               (push
                (cons (nreverse cur_stack) up_stack)
                stack)))
            ((slisp-check-primitive token)
             (push (cons token (pop stack)) stack))
            ((slisp-check-type token "boolean")
             (let ((bool))
               (cond ((equal token "t")
                      (setq bool t))
                     ((equal token "nil")
                      (setq bool nil)))
               (push (cons bool (pop stack)) stack)))
            ((slisp-check-type token "string")
             (push (cons (match-string 1 token) (pop stack)) stack))
            ((slisp-check-type token "number")
             (push (cons (string-to-number (match-string 1 token)) (pop stack)) stack))
            ((slisp-check-type token "symbol")
             (push (cons (make-symbol token) (pop stack)) stack))
            (t
             (push (cons token (pop stack)) stack))))
    (let ((cur (pop stack)))
      (cons slisp-sequence-top-symbol (nreverse cur)))))

(defun slisp-callcc-parse (exp env)
  (with-gensyms (variable)
    (catch 'exit
      (dolist (seed (cdr exp))
        (let* ((ret (slisp-callcc-getcc seed variable env))
               (find (slisp-get-findcallcc ret))
               (cc (slisp-get-callcc ret)))
          (if find
              (progn
                (let ((sexp (list "lambda" (list variable) (list "throw" cc))))
                  (puthash "callcc" (slisp-eval sexp env) env))
                (throw 'exit t))))))))

(defun slisp-callcc-getcc (exp variable env)
  (catch 'exit
    (let ((cc '())
          (find_callcc nil))
      (dolist (i exp)
        (if (listp i)
            (let ((find (slisp-get-findcallcc (slisp-callcc-getcc i variable env)))
                  (ret (slisp-get-callcc (slisp-callcc-getcc i variable env))))
              (setq find_callcc find)
              (setq cc (slisp-callcc-setlist ret cc))
              (if (and (symbolp ret)
                       (equal (symbol-name ret) (symbol-name variable)))
                  (setq find_callcc t)))
          (if (and (stringp i)
                   (equal i slisp-callcc-symbol))
              (throw 'exit (list t variable))
            (setq cc (slisp-callcc-setlist i cc)))))
      (if (listp cc)
          (list find_callcc (nreverse cc))
        (list find_callcc cc)))))

(defun slisp-get-findcallcc (exp)
  (car exp))

(defun slisp-get-callcc (exp)
  (cadr exp))

(defun slisp-callcc-setlist (exp l)
  (if (eq l nil)
      (list exp)
    (cons exp l)))

(defmacro slisp-make-primitive? (exp operator)
  `(cond ((listp ,exp)
          (let ((op (car ,exp)))
            (and (string? op)
                 (equal op ,operator))))
         (t
          nil)))

(defun slisp-eval (exp env)
  (defun neq (exp obj)
    (not (eq exp obj)))
  (defun pair? (exp)
    (if (listp exp)
        (let ((first (car exp))
              (second (cadr exp)))
          (and (neq first nil)
               (neq second nil)))
      nil))

  (defun number? (exp)
    (numberp exp))
  (defun string? (exp)
    (stringp exp))
  (defun symbol? (exp)
    (symbolp exp))
  (defun boolean? (exp)
    (and (string? exp)
         (or (equal exp "t")
             (equal exp "nil"))))
  (defun self? (exp)
    (cond ((booleanp exp) t)
          ((number? exp) t)
          ((string? exp) t)))
  (defun variable? (exp)
    (symbolp exp))

  ;; special forms check
  (defun list? (exp)
    (slisp-make-primitive? exp "list"))
  (defun let? (exp)
    (slisp-make-primitive? exp "let"))
  (defun set? (exp)
    (slisp-make-primitive? exp "setq"))
  (defun defun? (exp)
    (slisp-make-primitive? exp "defun"))
  (defun if? (exp)
    (slisp-make-primitive? exp "if"))
  (defun cond? (exp)
    (slisp-make-primitive? exp "cond"))
  (defun lambda? (exp)
    (slisp-make-primitive? exp "lambda"))
  (defun apply? (exp)
    (slisp-make-primitive? exp "apply"))
  (defun call/cc? (exp)
    (slisp-make-primitive? exp "call/cc"))
  (defun throw? (exp)
    (slisp-make-primitive? exp "throw"))
  (defun top-progn? (exp)
    (slisp-make-primitive? exp "top-progn"))
  (defun progn? (exp)
    (slisp-make-primitive? exp "progn"))
  (defun print? (exp)
    (slisp-make-primitive? exp "print"))
  (defun arithmetic? (exp)
    (let ((op (car exp)))
      (cond ((equal op "+") t)
            ((equal op "-") t)
            ((equal op "*") t)
            ((equal op "/") t)
            ((equal op "=") t)
            ((equal op ">") t)
            ((equal op "<") t)
            ((equal op "not") t)
            ((equal op "or") t)
            ((equal op "and") t)
            (t nil))))

  (defun eval-lambda? (exp)
    (let ((first (car exp))
          (rest (cdr exp)))
      (and (listp first)
           (equal "lambda" (car first))
           (not (eq rest nil)))))

  (defun eval-define? (exp)
    (let ((var (car exp)))
      (variable? var)))

  (defun eval-callcc? (exp)
    (let ((var (car exp)))
      (and (stringp var)
           (equal var "call/cc"))))

  (defun throw? (exp)
    (let ((var (car exp)))
      (and (stringp var)
           (equal var "throw"))))

  (defun eval-self? (exp)
    (self? (car exp)))

  (defun eval-rec-lambda? (exp)
    (if (listp exp)
        (let ((first (car exp))
              (rest (cdr exp)))
          (and (equal first "lambda")
               (neq rest nil)))
      nil))

  (defun application? (exp)
    (pair? exp))

  (defun eval-define (exp env)
    (let* ((func-name (symbol-name (car exp)))
           (func-hash (gethash func-name env))
           (func-args-var (car func-hash))
           (func-body (cadr func-hash))
           (args-length (length func-args-var))
           (func-args-real (cdr exp))
           (new_env (copy-hash-table env)))
      (cond ((eq args-length 0)
             (let ((new_exp (list func-body (list (car func-args-real)))))
               new_exp))
            (t
             (dotimes (i args-length)
               (let ((variable (nth i func-args-var))
                     (value (nth i func-args-real)))
                 (setq exp (beta-reduction exp value (symbol-name variable) env))))
             (slisp-eval exp new_env)))))

 (defun eval-lambda (exp env)
    (let* ((first (car exp))
           (func-args-var (cadr first))
           (func-body (caddr first))
           (func-args-real (cdr exp))
           (args-length (length func-args-var))
           (ret func-body))
      (cond ((self? ret)
             ret)
            ((eq func-args-var nil)
             (slisp-eval func-body env))
            (t
             (progn
               (if (variable? ret)
                   (setq ret (lookup-variable-value ret env)))
               (dotimes (i args-length)
                 (let ((variable (nth i func-args-var))
                       (value (nth i func-args-real)))
                   (setq ret (beta-reduction ret value (symbol-name variable) env))))
               (slisp-eval ret env))))))
  
  (defun eval-self-lambda (exp env)
    (let ((func-args-var (cadr exp))
          (func-body (caddr exp))
          (do_eval nil))
      (dolist (item func-args-var)
        (let ((name (symbol-name item)))
        (if (neq (gethash name env) nil)
            (setq do_eval t))))
      (if do_eval
          (slisp-eval func-body env)
        exp)))

  (defun eval-callcc (exp env)
    (let* ((body (cadr exp))
           (lambda-arg (car (cadr body)))
           (lambda-body (caddr body)))
      (puthash (symbol-name lambda-arg) (gethash "callcc" env) env)
      (slisp-eval lambda-body env)))

  (defun eval-if (exp env)
    (let ((condition (nth 1 exp))
          (true-body (nth 2 exp))
          (else-body (nth 3 exp)))
      (if (slisp-eval condition (copy-hash-table env))
          (slisp-eval true-body (copy-hash-table env))
        (if (not (eq else-body nil))
            (slisp-eval else-body (copy-hash-table env))))))

  (defun eval-cond (exp env)
    (let ((condlist (cdr exp)))
      (loop for conditem in condlist
            when (slisp-eval (car conditem) (copy-hash-table env))
            return (slisp-eval (cadr conditem) (copy-hash-table env)))))

  (defun make-lambda (exp env)
    (let ((func-name "lambda")
          (func-args (nth 1 exp))
          (func-impl (nth 2 exp)))
      (puthash func-name (list func-args func-impl) env)))

  (defun convert-list (exp)
    (cdr exp))

  (defun set-let (exp env)
    (let ((var-list (cadr exp))
          (exp-list (caddr exp)))
      (dolist (var-item var-list)
        (let ((variable (car var-item))
              (value 
               (cond ((eq (cadr var-item) nil) nil)
                     (t (cadr var-item)))))
          (puthash (symbol-name variable) value env)))
      (slisp-eval exp-list env)))

  (defun set-definition (exp env)
    (let* ((func-name (nth 1 exp))
           (func-args (nth 2 exp))
           (func-impl (nth 3 exp))
           (new_exp (list "lambda" func-args func-impl)))
      (puthash (symbol-name func-name) new_exp env)))

  (defun progn-actions (exp)
    (cdr exp))

  (defun eval-top-sequence (actions env)
    (dolist (action actions)
      (catch 'callcc-exit
        (setq ret (slisp-eval action env)))))

  (defun eval-sequence (actions env)
    (dolist (action actions)
      (slisp-eval action env)))

  (defun calc (exp env)
    (let ((op (car exp))
          (first (slisp-eval (cadr exp) (copy-hash-table env)))
          (second (slisp-eval (caddr exp) (copy-hash-table env))))
      (cond ((equal op "+") (+ first second))
            ((equal op "-") (- first second))
            ((equal op "*") (* first second))
            ((equal op "/") (/ first second))
            ((equal op "=") (= first second))
            ((equal op "<") (< first second))
            ((equal op ">") (> first second))
            ((equal op "not") (not first))
            ((equal op "or") (or first second))
            ((equal op "and") (and first second)))))

  (defun print (exp env)
    (let ((mes (slisp-eval (cadr exp) (copy-hash-table env))))
      (cond ((number? mes)
             (message "%d" mes))
            ((booleanp mes)
             (if mes (message "t")
               (message "nil")))
            (t
             (message "%s" mes)))))

  (defun eval-boolean (exp)
    (cond ((equal exp "t") t)
          ((equal exp "nil" nil))
          (t 
           (error "eval-boolean not match:%s" exp))))

  (defun lookup-variable-value (exp env)
    (let* ((symbolname (symbol-name exp)))
      (gethash symbolname env)))

  (defun have-variable-value? (exp env)
    (and (not (listp exp))
         (symbolp exp)
         (let ((v (lookup-variable-value exp env)))
           (neq v nil))))

  (defun set-assignment (exp env)
    (let ((variable (cadr exp))  
          (value (slisp-eval (caddr exp) (copy-hash-table env))))
      (puthash (symbol-name variable) value slisp-environment)))

  (defun list-of-values (exp env)
    exp)

  (defun beta-reduction (exp trans_exp key env)
    (let ((ret '())
          (check_exp exp)
          (have_key nil))
      (cond ((lambda? check_exp)
             (progn
               (setq check_exp (cddr exp))
               (let ((func-args (cadr exp)))
                 (dolist (arg func-args)
                   (if (and (not (listp arg))
                            (equal (symbol-name arg) key))
                       (setq have_key t))))))
            ((let? check_exp)
             (progn
               (setq check_exp (cddr exp))
               (let ((func-args (cadr exp)))
                 (dolist (arg func-args)
                   (if (and (not (listp arg))
                            (equal (symbol-name arg) key))
                       (setq have_key t)))))))
      (if (eq have_key t)
          exp
        (if (listp check_exp)
            (let ((len (length check_exp)))
              (dolist (i check_exp)
                (if (listp i)
                    (let ((in_exp (beta-reduction i trans_exp key env)))
                      (if (= len 1)
                          (setq ret in_exp)
                        (setq ret (cons in_exp (if (eq ret nil) '() ret)))))
                  (let ((ii (if (symbolp i) (symbol-name i) i)))
                    (if (= len 1)
                        (setq ret (if (equal ii key) trans_exp i))
                      (setq ret (cons
                                 (if (equal ii key)
                                     trans_exp
                                   i)
                                 (if (eq ret nil) '() ret))))))))
          (setq ret (cons
                     (if (eq (symbol-name check_exp) key)
                         trans_exp
                       check_exp)
                     ret)))
        (if (> (length check_exp) 1)
            (setq ret (nreverse ret)))
        (cond ((lambda? exp)
               (progn
                 (setq ret (list "lambda" (cadr exp) ret))))
              ((let? exp)
               (progn
                 (setq ret (list "let" (cadr exp) ret)))))
        ret)))

  (defun eval-apply (exp env)
    (let* ((first (car exp))
           (second (cdr exp))
           (procedure (slisp-eval first env)))
      (let ((new_exp (cons procedure second)))
        (cond ((eval-lambda? new_exp)
               (eval-lambda new_exp env))
              ((eval-define? new_exp)
               (eval-define new_exp env))
              ((eval-callcc? new_exp)
               (eval-callcc new_exp env))
              ((eval-self? new_exp)
               (car new_exp))
              (t
               (error "eval-apply not match:%s" new_exp))))))

  (defun eval-throw (exp env)
    (let ((body (cadr exp)))
      (slisp-eval body env)
      (throw 'callcc-exit t)))

  (cond ((self? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((list? exp) (convert-list exp))
        ((let? exp) (set-let exp env))
        ((set? exp) (set-assignment exp env))
        ((defun? exp) (set-definition exp env))
        ((if? exp) (eval-if exp env))
        ((cond? exp) (eval-cond exp env))
        ((throw? exp) (eval-throw exp env))
        ((lambda? exp)
         (eval-self-lambda exp env))
        ((top-progn? exp) (eval-top-sequence (progn-actions exp) env))
        ((progn? exp) (eval-sequence (progn-actions exp) env))
        ((arithmetic? exp) (calc exp env))
        ((print? exp) (print exp env))
        ((application? exp)
         (eval-apply exp env))
        (t (error "Unknown expression type. " exp))))

(defun slisp-e ()
  (interactive)
  (slisp-clear-messages)
  (let ((tree (slisp-parse (slisp-get-tokens (slisp-get-src-string slisp-filename)))))
    (slisp-callcc-parse tree slisp-environment)
    (slisp-eval tree slisp-environment)))

(defun slisp-mode ()
  (interactive)
  (setq major-mode 'slisp-mode
    mode-name "slisp mode")
  (setq beta-local-map (make-keymap))
  (define-key beta-local-map "\C-cn" 'slisp-e)
  (use-local-map beta-local-map))