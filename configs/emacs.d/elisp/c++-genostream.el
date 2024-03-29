;;; c++-genostream.el --- Generator functions for output ostream operators -*-  lexical-binding: t -*-

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Keywords: convenience, tools, c++, lsp
;; Created: 2022-09-28
;; Version: 0.0.1

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


;;; Commentary:

;; This package provides a number of functions to simplify the generation of
;; output stream operators for C++ types.  Requires lsp-mode and a lsp-server
;; with the `textDocument/symbol' and `textDocument/ast' capability, such as
;; `clangd'.

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)


(defun c++-genostream--sort-candidates (symbols)
  "Sort the given hash-table of SYMBOLS into a sorted list of candidates."
  (let (result)
    (dolist (map symbols result)
      (let ((namespace (gethash "containerName" map))
            (name (gethash "name" map)))
        (if (not (string= namespace ""))
            (push (concat namespace "::" name) result)
          (push name result))))
    (nreverse result)))


(defun c++-genostream--query-symbols ()
  "Ask the user for a symbol and identify candidates the user can choose from.

Additionally, if prefix is given, ask the user to specify a
preference for the generated constructs.  This is primarily used
to force generation of a bitflag enumeration operator where
automatic detection is not possible."
  (let* ((token (read-string "Symbol: "))
         (kinds '("Record" "Enum" "Bitflag"))
         (kind  (when current-prefix-arg
                  (completing-read "Prefer: " kinds nil t nil nil kinds)))
         (symbols (c++-genostream--get-symbols token))
         (candidates (c++-genostream--sort-candidates symbols))
         (choice (if (> (length candidates) 1)
                     (completing-read "Candidates: " candidates nil t nil nil candidates)
                   (and candidates (car candidates))))
         (position (cl-position choice candidates :test #'string=)))
    (if choice
        (list (nth position symbols) kind)
      (error "No symbols found"))))


(defun c++-genostream--get-symbols (symbol)
  "Query the lsp-server for possible locations of SYMBOL.

Returns a hash-map of potential results."
  (lsp-request "workspace/symbol" (list :query symbol)))


(defun c++-genostream--document-symbols (uri)
  "Query the lsp-server for symbols in the given document URI.

TODO: This can be used similarly to the AST nodes, but enums
become harder to specialize."
  (lsp-request "textDocument/documentSymbol" (list :textDocument (list :uri uri))))


(defun c++-genostream--get-ast (uri range)
  "Retrieve the Abstract Syntax Tree for the given document URI and RANGE."
  (lsp-request "textDocument/ast" (list :textDocument (list :uri uri)
                                        :range range)))


(defun c++-genostream--is-scoped-enum (ast)
  "Check if the AST node is ascoped enum node."
  (let* ((kind   (gethash "kind" ast))
         (arcana (gethash "arcana" ast)))
    (and (string= kind "Enum")
         (or (string-match-p (regexp-quote " class ") arcana)
             (string-match-p (regexp-quote " struct ") arcana)))))


(defun c++-genostream--is-pow2 (v)
  "Check if the number V is a power of 2.

Note that 0 is considered a power of two in this case."
  (and v (= 0 (logand v (- v 1)))))


(defun c++-genostream--is-bitflag-enum (ast)
  "Check if the AST node is a bitflag-like enum node."
  (let ((kind  (gethash "kind" ast))
        (enums (c++-genostream--enum-values ast)))
    (and (string= kind "Enum")
         enums                          ; Should not be empty.
         (cl-every (lambda (v) (c++-genostream--is-pow2 (cdr v))) enums))))


(defun c++-genostream--ast-walk (ast)
  "Recursively walk the AST node."
  (let ((role     (gethash "role" ast))
        (kind     (gethash "kind" ast))
        (detail   (gethash "detail" ast))
        (arcana   (gethash "arcana" ast))
        (children (gethash "children" ast)))
    (mapc (lambda (ch) (c++-genostream--ast-walk ch)) children)))


(defun c++-genostream--ast-find-symbol (ast symbol)
  "Search through the toplevel AST node to find SYMBOL."
  (let ((detail   (gethash "detail" ast))
        (children (gethash "children" ast)))
    (if (string= detail symbol)
        ast
      (cl-some (lambda (ch) (c++-genostream--ast-find-symbol ch symbol)) children))))


(defun c++-genostream--enum-value (ast)
  "Descend the AST to find the enumeration value of the given node."
  (let ((kind     (gethash "kind" ast))
        (arcana   (gethash "arcana" ast))
        (children (gethash "children" ast)))
    (if (string= kind "Constant")
        (let* ((arcana (split-string arcana))
               (len    (length arcana))
               (value  (car (last arcana))))
          (string-to-number value))
      (cl-some #'c++-genostream--enum-value children))))


(defun c++-genostream--enum-values (ast)
  "Create an alist of all immediate enumeration values for the given AST node."
  (let* ((children (gethash "children" ast))
         (enums nil))
    (mapc (lambda (ch)
            (let ((kind   (gethash "kind" ch))
                  (detail (gethash "detail" ch)))
              (when (string= kind "EnumConstant")
                (push `(,detail . ,(c++-genostream--enum-value ch)) enums))))
          children)
    (nreverse enums)))


(defun c++-genostream--enum-bitflag-loop (token ast)
  "Generate an output operator for bitflag enum using the AST node and TOKEN."
  (let* ((indent "    ")
         (name   (gethash "name" token))
         (nsp    (gethash "containerName" token))
         (scoped (c++-genostream--is-scoped-enum ast))
         (enums  (seq-filter (lambda (v) (c++-genostream--is-pow2 (cdr v)))
                             (c++-genostream--enum-values ast)))
         (len    (length enums))
         (fqn    (if (string= nsp "") name (concat nsp "::" name)))
         (sep    (concat ",\n" indent indent))
         (enames (mapconcat (lambda (v) (format "\"%s\"" (car v))) enums sep))
         (econt  (if scoped (concat fqn "::") ""))
         (evalues (mapconcat
                   (lambda (v)
                     (format "static_cast<UInt>(%s)" (concat econt (car v))))
                   enums sep)))
    (format "std::ostream& operator<<(std::ostream &os, %s bf)
{
    bool is_first = true;
    using UnderlyingT = typename std::underlying_type_t<%s>;
    using UInt = typename std::make_unsigned_t<UnderlyingT>;
    UInt u = static_cast<UInt>(bf);
    const size_t nenums = %d;
    const char * enum_names[] = {
        %s
    };
    UInt enum_values[] = {
        %s
    };
    for (size_t i = 0; i < nenums; i++)
    {
        bool is_set = u & enum_values[i];
        if (is_set && !is_first)
            os << \" | \";
        if (is_set)
        {
            is_first = false;
            os << enum_names[i];
        }
    }
    return os;
}" fqn fqn len enames evalues fqn)))


(defun c++-genostream--enum-bitflag (token ast)
  "Generate an output operator for bitflag enum using the AST node and TOKEN."
  (let* ((indent "    ")
         (name   (gethash "name" token))
         (nsp    (gethash "containerName" token))
         (scoped (c++-genostream--is-scoped-enum ast))
         ; Remove non-power of two flags - They are assumed to be unions.
         (enums  (seq-filter (lambda (v) (c++-genostream--is-pow2 (cdr v)))
                             (c++-genostream--enum-values ast)))
         (len    (length enums))
         (fqn    (if (string= nsp "") name (concat nsp "::" name)))
         (sep    (concat ",\n" indent indent))
         (enames (mapconcat (lambda (v) (format "\"%s\"" (car v))) enums sep))
         (econt  (if scoped (concat fqn "::") "")))
    (with-temp-buffer
      (insert (format "std::ostream& operator<<(std::ostream &os, %s v)\n" fqn))
      (insert "{\n")
      (insert (format "%sbool is_first = true;\n" indent))
      (insert (format "%susing UnderlyingT = typename std::underlying_type_t<%s>;\n" indent fqn))
      (insert (format "%susing UInt = typename std::make_unsigned_t<UnderlyingT>;\n" indent))
      (insert (format "%sconst UInt zero = static_cast<UInt>(0);\n" indent))
      (insert (format "%sconst UInt u = static_cast<UInt>(v);\n" indent))
      (dotimes (i len)
        (let* ((enum (car (nth i enums)))
               (fqn  (concat (if (string= nsp "") "" (concat nsp "::")) name "::" enum)))
          (insert (format "%sif ((u & static_cast<UInt>(%s)) != zero)\n" indent fqn))
          (insert (format "%s{\n" indent))
          (insert (format "%s%sos << (!is_first ? \" | \" : \"\") << \"%s\";\n" indent indent enum))
          (insert (format "%s%sis_first = false;\n" indent indent))
          (insert (format "%s}\n" indent))))
      (insert (format "%sreturn os;\n" indent))
      (insert "}\n")
      (buffer-string))))



(defun c++-genostream--enum-scoped (token ast)
  "Generate an output operator for a scoped enum using the AST node and TOKEN."
  (let* ((indent "    ")
         (name (gethash "name" token))
         (nsp  (gethash "containerName" token))
         (fqn  (if (string= nsp "") name (concat nsp "::" name)))
         (enums (c++-genostream--enum-values ast))
         (len   (length enums)))
    (with-temp-buffer
      (insert (format "std::ostream& operator<<(std::ostream &os, %s v)\n" fqn))
      (insert "{\n")
      (insert (format "%sswitch (v)\n" indent))
      (insert (format "%s{\n" indent))
      (dotimes (i len)
        (let* ((enum (car (nth i enums)))
               (fqn  (concat (if (string= nsp "") "" (concat nsp "::")) name "::" enum)))
          (insert (format "%scase %s: os << \"%s\"; break;\n" indent fqn enum))))
      (insert (format "%s}\n" indent))
      (insert (format "%sreturn os;\n" indent))
      (insert "}\n")
      (buffer-string))))


(defun c++-genostream--enum-bare (token ast)
  "Generate an output operator for a bare enum using the AST node and TOKEN."
  (let* ((indent "    ")
         (name (gethash "name" token))
         (nsp  (gethash "containerName" token))
         (fqn  (if (string= nsp "") name (concat nsp "::" name)))
         (enums (c++-genostream--enum-values ast))
         (len   (length enums)))
    (with-temp-buffer
      (insert (format "std::ostream& operator<<(std::ostream &os, %s v)\n" fqn))
      (insert "{\n")
      (insert (format "%sswitch (v)\n" indent))
      (insert (format "%s{\n" indent))
      (dotimes (i len)
        (let* ((enum (car (nth i enums)))
               (fqn (if (string= nsp "") enum (concat nsp "::" enum))))
          (insert (format "%scase %s: os << \"%s\"; break;\n" indent fqn enum))))
      (insert (format "%s}\n" indent))
      (insert (format "%sreturn os;\n" indent))
      (insert "}\n")
      (buffer-string))))


(defun c++-genostream--enum (token ast)
  "Generate an output operator for an enum using the AST node and TOKEN."
  (cond
   ((c++-genostream--is-bitflag-enum ast)
    (c++-genostream--enum-bitflag token ast))
   ((c++-genostream--is-scoped-enum ast)
    (c++-genostream--enum-scoped token ast))
   (t
    (c++-genostream--enum-bare token ast))))


(defun c++-genostream--get-struct-fields (ast)
  "Descend the AST and create a list of all immediate struct fields."
  (let ((children (gethash "children" ast))
        (fields   nil))
    (mapc (lambda (ch)
            (when (string= (gethash "kind" ch) "Field")
              (push (gethash "detail" ch) fields)))
          children)
    (nreverse fields)))


(defun c++-genostream--struct (token ast)
  "Generate an output operator for a struct using the AST node and TOKEN."
  (let* ((indent "    ")
         (name (gethash "name" token))
         (nsp  (gethash "containerName" token))
         (fqn  (if (string= nsp "") name (concat nsp "::" name)))
         (fields (c++-genostream--get-struct-fields ast)))
    (with-temp-buffer
      (insert (format "std::ostream& operator<<(std::ostream &os, const %s &v)\n" fqn))
      (insert "{\n")
      (insert (format "%sos << \"%s{\";\n" indent name))
      (dotimes (i (length fields))
        (if (not (equal i (- (length fields) 1)))
            (insert (format "%sos << \"%s=\" << v.%s << \", \";\n"
                            indent (nth i fields) (nth i fields)))
          (insert (format "%sos << \"%s=\" << v.%s;\n"
                          indent (nth i fields) (nth i fields)))))
      (insert (format "%sos << \"}\";\n" indent))
      (insert (format "%sreturn os;\n" indent))
      (insert "}\n")
      (buffer-string))))


(defun c++-genostream--generate-default (token ast)
  "Generate an output operator for the given TOKEN with corresponding AST node.

This is the default when user have not given any preference."
  (let ((type (gethash "kind" ast)))
    (cond
     ((string= type "Enum")
      (c++-genostream--enum token ast))
     ((string= type "CXXRecord")
      (c++-genostream--struct token ast))
     (t
      (error "Unknown type to generate operator for")))))


(defun c++-genostream--generate-with-preference (token ast kind)
  "Generate an output operator for the given TOKEN with corresponding AST node.

 Prefer KIND when possible."
  (let ((type (gethash "kind" ast)))
    (cond
     ((and (or (string= kind "Enum")
               (string= kind "Bitflag"))
           (string= type "Enum"))
      (cond
       ((or (string= kind "Bitflag")
            (c++-genostream--is-bitflag-enum ast))
        (c++-genostream--enum-bitflag token ast))
       ((c++-genostream--is-scoped-enum ast)
        (c++-genostream--enum-scoped token ast))
       (t
        (c++-genostream--enum-bare token ast))))
     ((and (string= kind "Record")
           (string= type "CXXRecord"))
      (c++-genostream--struct token ast))
     (t
      (error "Mismatched type to generate operator for")))))


(defun c++-genostream--generate (token ast &optional kind)
  "Generate an output operator for the given TOKEN with corresponding AST node.

Optionally, if KIND is non-nil, prefer that kind."
  (if kind
      (c++-genostream--generate-with-preference token ast kind)
    (c++-genostream--generate-default token ast)))


;;;###autoload
(defun c++-genostream (token &optional kind)
  "Generate an output stream operator for the given TOKEN.

Optionally, if prefix is used prefer an operator of the given
KIND."
  (interactive (c++-genostream--query-symbols))
  (let* ((loc (gethash "location" token))
         (nsp (gethash "containerName" token))
         (nam (gethash "name" token))
         (uri (gethash "uri" loc))
         (rng (gethash "range" loc))
         (ast (c++-genostream--get-ast uri rng))
         (sym (c++-genostream--ast-find-symbol ast nam)))
    (if sym
        (insert (c++-genostream--generate token sym kind))
      (error "Unable to find the given symbol"))))


(provide 'c++-genostream)
;;; c++-genostream.el ends here
