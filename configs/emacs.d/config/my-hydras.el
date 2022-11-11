;;; my-hydras.el --- Personal hydra keybindings. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; All personal hydra keybindings are collected in this file.
;;
;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package hydra
  :ensure t
  :pin melpa
  :demand t
  :config
  (require 'hydra-examples)

  (defhydra hydra-help (:exit t)
    "
  Describe        ^^Keys                    ^^Search                    ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_: Function     _k_: Keybinding           _a_: Apropos                _i_: Info
  _p_: Package      _w_: Where-is             _d_: Apropos Docstrings     _n_: WoMan
  _m_: Mode         _b_: Show all bindings    _s_: Info by symbol         _h_: _H_elm-_D_ash
  _v_: Variable
  _S_: Symbol

  "
    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("S" describe-symbol nil)
    ("y" describe-syntax nil)
    ("F" describe-face nil)

    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Documentation
    ("i" info nil)
    ("n" woman nil)
    ("h" helm-dash-at-point nil)
    ("H" helm-dash nil)
    ("D" my-helm-dash-install nil)

    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")

    ("q" help-quit "quit"))
  (global-set-key (kbd "C-c h") #'hydra-help/body)

  (defhydra hydra-kmacro (:hint nil
                          :color pink
                          :pre (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
    "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] Execute    [_n_] Insert    [_b_] Name      [_'_] Previous
     ^^↑^^           [_d_] Delete     [_t_] Set       [_K_] Key       [_,_] Last
 _j_ ←   → _l_       [_o_] Edit       [_a_] Add       [_x_] Register
     ^^↓^^           [_r_] Region     [_f_] Format    [_B_] Defun
     ^_k_^           [_m_] Step
    ^^   ^^          [_s_] Swap
Last Macro: %(key-description last-kbd-macro)
"
    ("j" kmacro-start-macro :color blue)
    ("l" kmacro-end-or-call-macro-repeat)
    ("i" kmacro-cycle-ring-previous)
    ("k" kmacro-cycle-ring-next)
    ("r" apply-macro-to-region-lines)
    ("d" kmacro-delete-ring-head)
    ("e" kmacro-end-or-call-macro-repeat)
    ("o" kmacro-edit-macro-repeat)
    ("m" kmacro-step-edit-macro)
    ("s" kmacro-swap-ring)
    ("n" kmacro-insert-counter)
    ("t" kmacro-set-counter)
    ("a" kmacro-add-counter)
    ("f" kmacro-set-format)
    ("b" kmacro-name-last-macro)
    ("K" kmacro-bind-to-key)
    ("B" insert-kbd-macro)
    ("x" kmacro-to-register)
    ("'" kmacro-edit-macro)
    ("," edit-kbd-macro)
    ("q" nil :color blue))
  (global-set-key (kbd "C-c k") #'hydra-kmacro/body)

  (defhydra hydra-compilation (:color red :hint nil)
    "
Commands          ^^Navigate           ^^Options
--------------------------------------------------------------------------------
_c_: Compile        _n_: Next            _f_: Follow output [%`compilation-scroll-output]
_r_: Recompile      _p_: Previous        _t_: Threshold     [%`compilation-skip-threshold]
_g_: Grep           _N_: Go to Next      _K_: Always Kill   [%`compilation-always-kill]
_k_: Kill           _P_: Go to Previous  ^ ^                [%`compile-command]
_R_: Rename buffer  _F_: Follow sources
_s_: Compile shell
^^^^
"
    ("c" compile :color blue)
    ("s" (let ((current-prefix-arg '(t))) (call-interactively #'compile)) :color blue)
    ("r" recompile :color blue)
    ("g" grep-find :color blue)
    ("k" kill-compilation :color blue)
    ("R" (with-current-buffer "*compilation*" (rename-uniquely)))

    ("n" compilation-next-error)
    ("p" compilation-previous-error)
    ("N" next-error)
    ("P" previous-error)
    ("F" next-error-follow-minor-mode)

    ("f" (setq compilation-scroll-output (not compilation-scroll-output)))
    ("t" (setq compilation-skip-threshold (% (1+ compilation-skip-threshold) 3)))
    ("K" (setq compilation-always-kill (not compilation-always-kill)))
    ("q" nil))
  (global-set-key (kbd "C-c c") #'hydra-compilation/body)

  (defhydra hydra-windowing (:color red :hint nil)
    "
Navigate^^    ^Splitting^      ^Buffers^      ^Windows/Frames^     ^Scroll^
-------------------------------------------------------------------------
  ^_k_^       _x_: Horizontal  _p_: Previous   _c_: Close Window:      ^_a_^
_h_ + _l_     _v_: Vertical    _n_: Next       _o_: Close other:       ^|^
  ^_j_^       ^ ^              _b_: Select     _N_: New Frame:         ^_z_^
^^^^          ^ ^              _f_: Find file  _D_: Delete Frame:      ^^ ^^
^^^^          ^ ^              ^ ^             ^ ^                      ^ ^
Resize^^     ^Addons^                    ^Save/Restore^       ^Miscellaneous^
-------------------------------------------------------------------------
  ^_K_^       _d_: Ace delete window:      _u_: Winner undo     _q_: Quit
_H_ + _L_     _s_: Ace Swap windows:       _U_: Winner redo
  ^_J_^       _i_: Ace maximize window:   _ri_: Register Save
^^^^          _F_: Projectile find file:  _rj_: Register Jump
^^^^          ^ ^                         ^  ^
"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("z" scroll-up-line)
    ("a" scroll-down-line)

    ("x" split-window-below)
    ("v" split-window-right)
    ("u" (progn (winner-undo)
                (setq this-command 'winner-undo)))
    ("U" winner-redo)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" ido-find-file)

    ("c" delete-window)
    ("o" delete-other-windows)
    ("N" my-new-gui-frame)
    ("D" delete-frame)

    ("d" ace-delete-window)
    ("s" ace-swap-window)
    ("i" ace-maximize-window)
    ("F" projectile-find-file)

    ("ri" window-configuration-to-register)
    ("rj" jump-to-register)

    ("q" nil))
  (global-set-key (kbd "C-c w") #'hydra-windowing/body))


(use-package rect
  :defer t
  :config

  (defvar-local my-prev-rect nil "Previous rectangle mark.")

  (defun my-prev-rect ()
    "Restore the previously saved region, if applicable."
    (interactive)
    (push-mark my-prev-rect t t)
    (rectangle-mark-mode 1))

  (defun my-set-rect ()
    "Restore a previous rectangle mark."
    (interactive)
    (if (region-active-p)
        (deactivate-mark)
      (setq-local my-prev-rect (copy-marker (point)))
      (rectangle-mark-mode 1)))

  (unless (fboundp #'rectangle-backward-char)
    (defalias #'rectangle-backward-char #'backward-char))
  (unless (fboundp #'rectangle-forward-char)
    (defalias #'rectangle-forward-char #'forward-char))
  (unless (fboundp #'rectangle-next-line)
    (defalias #'rectangle-next-line #'next-line))
  (unless (fboundp #'rectangle-previous-line)
    (defalias #'rectangle-previous-line #'previous-line))

  (defhydra hydra-rectangle-mark (:body-pre (my-set-rect)
                                  :hint nil
                                  :color pink)
    "
  ^_k_^    _e_: Exchange  _c_: Copy    _o_: Open        _si_: String Insert     _U_: Upcase
_h_   _l_  _r_: Reset     _x_: Kill    _C_: Clear       _sr_: String Replace    _D_: Downcase
  ^_j_^    _p_: Previous  _y_: Yank    _n_: Number      _RR_: Register Read     _%_: Replace
^^^^       _u_: Undo      _d_: Delete  _w_: Whitespace  _RI_: Register Insert
"
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)

    ("e" rectangle-exchange-point-and-mark)
    ("r" my-set-rect)
    ("C-x SPC" my-set-rect)
    ("p" my-prev-rect)
    ("u" undo)

    ("c" copy-rectangle-as-kill)
    ("x" kill-rectangle)
    ("d" delete-rectangle :color blue)
    ("y" yank-rectangle   :color blue)

    ("o" open-rectangle              :color blue)
    ("C" clear-rectangle             :color blue)
    ("n" rectangle-number-lines      :color blue)
    ("w" delete-whitespace-rectangle :color blue)

    ("si" string-insert-rectangle    :color blue)
    ("sr" string-rectangle           :color blue)
    ("RR" copy-rectangle-to-register :color blue)
    ("RI" insert-register            :color blue)

    ("U" upcase-rectangle   :color blue)
    ("D" downcase-rectangle :color blue)
    ("%" query-replace-regexp :color blue)

    ("q" nil "Quit" :exit t))
  (global-set-key (kbd "C-x SPC") #'hydra-rectangle-mark/body))


(use-package dired
  :defer t
  :config
  (defhydra hydra-dired (:color pink)
    "
Opening^^          ^Marking^                    ^Point/Mark Actions^
--------------------------------------------------------------------------------
_e_: Open file     _m_: Mark          _A_: Search  _P_: Print      _M_: Chmod
_f_: Open file     _u_: Unmark        _C_: Copy    _R_: Rename     _O_: Chown
_o_: Display file  _t_: Toggle marks  _D_: Delete  _S_: Symlink    _X_: Shell
_v_: View file     _U_: Unmark all    _G_: Chgrp   _H_: Hardlink   _c_: Compress

New/Delete^^          ^Navigate^            ^Views^            ^Addons^
--------------------------------------------------------------------------------
_+_: New directory    _nu_: Go up directory  _g_: Refresh       _wd_: Wdired on
_d_: Delete mark       _j_: Goto file        _s_: Sort files    _Ti_: Image-dired
_x_: Execute delete    _i_: Insert subdir   _hd_: Hide details  _Tv_: View Image
_._: Clean directory   ^ ^                  _ha_: Hide all

"
    ("k" dired-prev-line nil)
    ("j" dired-next-line nil)

    ("e" dired-find-file nil)
    ("f" dired-find-file nil)
    ("o" dired-display-file nil)
    ("v" dired-view-file nil)

    ("m" dired-mark nil)
    ("u" dired-unmark nil)
    ("t" dired-toggle-marks nil)
    ("U" dired-unmark-all-marks nil)

    ("A" dired-do-find-regexp nil)
    ("C" dired-do-copy nil)
    ("D" dired-do-delete nil)
    ("G" dired-do-chgrp nil)

    ("P" dired-do-print nil)
    ("R" dired-do-rename nil)
    ("S" dired-do-symlink nil)
    ("H" dired-do-hardlink nil)

    ("M" dired-do-chmod nil)
    ("O" dired-do-chown nil)
    ("X" dired-do-shell-command nil)
    ("c" dired-do-compress nil)

    ("+" dired-create-directory nil)
    ("d" dired-flag-file-deletion nil)
    ("x" dired-do-flagged-delete nil)
    ("." dired-clean-directory nil)

    ("nu" dired-up-directory nil)
    ("j" dired-goto-file nil)
    ("i" dired-maybe-insert-subdir nil)

    ("g" revert-buffer nil)
    ("hd" dired-hide-details-mode nil)
    ("ha" dired-hide-all nil)
    ("s" dired-sort-toggle-or-edit nil)

    ("wd" wdired-change-to-wdired-mode nil :head blue :exit t)
    ("Ti" image-dired-dired-toggle-marked-thumbs nil)
    ("Tv" image-dired-dired-display-external nil)

    ("q" nil "Quit"))
  (define-key dired-mode-map [remap dired-summary] 'hydra-dired/body))


;; Org hydra configuration.
(use-package org
  :defer t
  :init
  (defhydra hydra-org (:color blue)
    "Org"
    ("l" org-store-link "Links")
    ("a" org-agenda     "Agenda")
    ("c" org-capture    "Capture")
    ("b" org-switchb    "Switch"))
  (global-set-key (kbd "C-c o") #'hydra-org/body))


;; Gnus hydra configuration.
(use-package message
  :defer t
  :config
  (defhydra hydra-message (:color blue :hint nil)
    "
Generic Mail^^                ^Editing Commands^           ^Actions^
--------------------------------------------------------------------------------
_ca_: Attach file             _ci_: Insert file            _cc_: Send message
_cb_: Attach Buffer           _ce_: Elide region           _cj_: Send delayed
_cu_: Toggle importance       _cr_: Caesar shift region    _cd_: Suspend message
_cn_: Request read receipt    _cm_: Morse region           _ck_: Abort message
_cw_: Change to wide reply    _cq_: Quote region
"
    ("ca" mml-attach-file)
    ("cb" mml-attach-buffer)
    ("cu" message-insert-or-toggle-importance)
    ("cn" message-insert-disposition-notification-to)
    ("cw" message-insert-wide-reply)

    ("ci" message-mark-insert-file)
    ("ce" message-elide-region)
    ("cr" rot13-region)
    ("cm" morse-region)
    ("cq" comment-dwim)

    ("cc" message-send-and-exit)
    ("cj" gnus-delay-article)
    ("cd" message-dont-send)
    ("ck" message-kill-buffer)
    ("C-c ?" describe-mode)
    ("q" nil "Quit" :exit t))
  (define-key message-mode-map (kbd "C-c ?") #'hydra-message/body))


(use-package gnus-group
  :defer t
  :config
  (defhydra hydra-gnus-group (:color pink :hint nil)
    "
Views^^                   ^Marking^                 ^Actions^
-------------------------------------------------------------------
_a_: List active groups   _#_: Mark group/topic     _m_: Write mail
_l_: List unread groups   _u_: Unmark group/topic   _c_: Read all
_L_: List all groups      _k_: Kill group           _S_: Search
_g_: Refresh news         _w_: Kill region          _z_: Suspend
_s_: Server view          _y_: Yank group(s)        _q_: Quit
_t_: Topic view

Modify Topics^^           ^Topic Views^
-------------------------------------------------------------------
 _Tn_: Create topic       _<tab>_: Indent topic
 _Tr_: Rename Topic   _<backtab>_: Unindent topic
_DEL_: Delete topic          _Td_: Remove group from topic
^                           ^_Th_: Toggle empty topics
"
    ("a" gnus-group-list-active)
    ("l" gnus-group-list-groups)
    ("L" gnus-group-list-all-groups)
    ("g" gnus-group-get-new-news)
    ("s" gnus-group-enter-server-mode :color blue)
    ("t" gnus-topic-mode)

    ("#" gnus-topic-mark-topic)
    ("u" gnus-group-unmark-topic)
    ("k" gnus-group-kill-group)
    ("w" gnus-group-kill-region)
    ("y" gnus-group-yank-group)

    ("m" gnus-group-mail             :color blue)
    ("c" gnus-topic-catchup-articles :color pink)
    ("S" gnus-group-make-nnir-group  :color blue)
    ("z" gnus-group-suspend          :color blue)
    ("q" nil nil)

    ("Tn" gnus-topic-create-topic)
    ("Tr" gnus-topic-rename)
    ("DEL" gnus-topic-delete)

    ("<tab>" gnus-topic-indent)
    ("<backtab>" gnus-topic-unindent)
    ("Td" gnus-topic-remove-group)
    ("Th" gnus-topic-toggle-display-empty-topics))
  (define-key gnus-group-mode-map "?" #'hydra-gnus-group/body))


(use-package gnus-sum    ;; gnus-summary-mode
  :defer t
  :config
  (defhydra hydra-gnus-summary (:color pink :hint nil)
    "
Sending/Replying^^             ^Marking^                  ^Actions^
----------------------------------------------------------------------------
_r_: Reply                     _!_: Pin mail              _D_: Download MIME
_R_: Reply with original       _?_: Mark as dormant       _a_: Article Hydra
_w_: Wide reply                _P_: Mark as read          _g_: Refresh
_W_: Wide reply with original  _U_: Unread mail           _q_: Quit
_f_: Forward                   _C_: Read all
_e_: Resend                    _#_: Mark processable
^^                             _u_: Unmark processable
"
    ("r" gnus-summary-reply                    :color blue)
    ("R" gnus-summary-reply-with-original      :color blue)
    ("w" gnus-summary-wide-reply               :color blue)
    ("W" gnus-summary-wide-reply-with-original :color blue)
    ("f" gnus-summary-mail-forward             :color blue)
    ("e" gnus-summary-resend-message-edit      :color blue)

    ("!" gnus-summary-tick-article-forward)
    ("?" gnus-summary-mark-as-dormant)
    ("P" gnus-summary-mark-as-read-forward)
    ("U" gnus-summary-clear-mark-forward)
    ("C" gnus-summary-catchup)
    ("#" gnus-summary-mark-as-processable)
    ("u" gnus-summary-unmark-as-processable)

    ("D" gnus-summary-save-parts)
    ("a" hydra-gnus-article/body :exit t)
    ("g" gnus-summary-insert-new-articles)
    ("q" nil :exit t))
  (define-key gnus-summary-mode-map "?" #'hydra-gnus-summary/body))


(use-package gnus-art   ;; gnus-article-mode
  :defer t
  :config
  (defhydra hydra-gnus-article (:color pink :hint nil)
    "
Sending/Replying^^             ^Treatment^            ^Display^            ^Actions^
-----------------------------------------------------------------------------------------------
_r_: Reply                     _t_: Toggle headers    _dg_: Gravatar       _s_: Summary Hydra
_R_: Reply with original       _c_: Caesar            _df_: Face           _v_: View MIME
_w_: Wide reply                _m_: Morse             _dx_: X-Face         _o_: Save MIME
_W_: Wide reply with original  _u_: UTF-8 -> ASCII    _ds_: Smileys        _g_: Refresh/Revert
_f_: Forward                   _o_: Deuglify Outlook  _dw_: Show images    _q_: Quit
_e_: Resend                    _F_: Fill long lines   _dd_: Remove images

"
    ("r" gnus-article-reply                    :color blue)
    ("R" gnus-article-reply-with-original      :color blue)
    ("w" gnus-article-wide-reply               :color blue)
    ("W" gnus-article-wide-reply-with-original :color blue)
    ("f" gnus-summary-mail-forward             :color blue)
    ("e" gnus-summary-resend-message-edit      :color blue)

    ("t" gnus-summary-toggle-header)
    ("c" gnus-summary-caesar-message)
    ("m" gnus-summary-morse-message)
    ("u" gnus-article-treat-non-ascii)
    ("o" gnus-article-outlook-deuglify-article)
    ("F" gnus-article-fill-long-lines)

    ("dg" gnus-treat-from-gravatar)
    ("df" gnus-article-display-face)
    ("dx" gnus-article-display-x-face)
    ("ds" gnus-treat-smiley)
    ("dw" gnus-html-show-images)
    ("dd" gnus-article-remove-images)

    ("s" hydra-gnus-summary/body :exit t)
    ("v" gnus-mime-view-part)
    ("o" gnus-mime-save-part)
    ("g" gnus-summary-show-article)
    ("q" nil :exit t))
  (define-key gnus-article-mode-map "?" #'hydra-gnus-article/body))


(use-package delim-col
  :after hydra
  :config
  (defhydra hydra-delim-col (:color pink :hint nil)
    "
Actions^^                ^Presets^               ^Options^
-----------------------------------------------------------------------------------------------
_r_: Apply on region     _pl_: Python lists       _B_: Before all columns     [%`delimit-columns-str-before]
_R_: Apply on rectangle  _pd_: Python dictionary  _s_: Column separator       [%`delimit-columns-str-separator]
^ ^                      _ps_: Python set         _A_: After all columns      [%`delimit-columns-str-after]
^ ^                      _dd_: Default            _b_: Before each column     [%`delimit-columns-before]
^ ^                      ^  ^                     _a_: After each column      [%`delimit-columns-separator]
^ ^                      ^  ^                     _S_: Separator regex        [%`delimit-columns-after]
^ ^                      ^  ^                     _e_: Odd number of columns  [%`delimit-columns-extra]
^ ^                      ^  ^                     _f_: Formatting             [%`delimit-columns-format]
"
    ("B" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-before str)))
    ("s" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-separator str)))
    ("A" (lambda (str)
           (interactive "s")
           (setq delimit-columns-str-after str)))
    ("b" (lambda (str)
           (interactive "s")
           (setq delimit-columns-before str)))
    ("S" (lambda (str)
           (interactive "s")
           (setq delimit-columns-separator str)))
    ("a" (lambda (str)
           (interactive "s")
           (setq delimit-columns-after str)))
    ("e" (lambda ()
           (interactive)
           (setq delimit-columns-extra (not delimit-columns-extra))))
    ("f" (lambda ()
           (interactive)
           (setq delimit-columns-format
                 (cond ((eq delimit-columns-format nil)
                        t)
                       ((eq delimit-columns-format t)
                        'separator)
                       ((eq delimit-columns-format 'separator)
                        'padding)
                       ((eq delimit-columns-format 'padding)
                        nil)))))

    ("pl" (lambda ()
            (interactive)
            (setq delimit-columns-str-before "[")
            (setq delimit-columns-str-separator ", ")
            (setq delimit-columns-str-after "]")
            (setq delimit-columns-separator "[[:space:]]")
            (setq delimit-columns-format 'padding)))
    ("pd" (lambda () (interactive)
            (setq delimit-columns-str-before "")
            (setq delimit-columns-str-separator ": ")
            (setq delimit-columns-str-after ",")
            (setq delimit-columns-before "")
            (setq delimit-columns-separator "[[:space:]]")
            (setq delimit-columns-format 'padding)))
    ("ps" (lambda () (interactive) nil))
    ("dd" (lambda () (interactive)
            (custom-reevaluate-setting 'delimit-columns-str-before)
            (custom-reevaluate-setting 'delimit-columns-str-separator)
            (custom-reevaluate-setting 'delimit-columns-str-before)
            (custom-reevaluate-setting 'delimit-columns-before)
            (custom-reevaluate-setting 'delimit-columns-separator)
            (custom-reevaluate-setting 'delimit-columns-after)
            (custom-reevaluate-setting 'delimit-columns-extra)
            (custom-reevaluate-setting 'delimit-columns-format)))

    ("r" delimit-columns-region)
    ("R" delimit-columns-rectangle)
    ("q" nil nil)))


(use-package hydra
  :config
  (defhydra hydra-apps (:color blue :hint nil)
    "
Browser/Calculator ^^Music                  ^^Mail/Chat      ^^Calendar           ^^Stack Exchange
---------------------------------------------------------------------------------------------------
_bo_: EWW Open       _mo_: EMMS Open          _go_: GNUS Open  _co_: Calendar Open  _so_: SX Open
_bb_: EWW Bookmarks  _mp_: EMMS Play library  _ge_: ERC        _ch_: Holidays       _sa_: SX Ask
_ca_: Calc           _mf_: EMMS Play local    _gb_: Bitlbee    _cl_: Holidays List  _sn_: SX Newest
"
    ("bo" eww)
    ("bb" eww-list-bookmarks)
    ("ca" calc)

    ("mo" emms-smart-browse)
    ("mp" emms-play-file)
    ("mf" (let (emms-source-file-default-directory)
            (call-interactively #'emms-play-file)))

    ("go" gnus)
    ("ge" erc)
    ("gb" bitlbee)

    ("so" sx-tab-all-questions)
    ("sa" sx-ask)
    ("sn" sx-tab-newest)

    ("co" calendar)
    ("ch" holidays)
    ("cl" holiday-list)

    ("q" nil))
  (global-set-key (kbd "C-c a") #'hydra-apps/body))


(use-package image-mode
  :defer t
  :after (hydra image+)
  :config
  (require 'image+)
  (defhydra imagex-hydra (:color red :hint nil)
    "
Zoom         ^^Manipulate           ^^Rotate           ^^Actions
--------------------------------------------------------------------------------
_+_: Zoom in  _m_: Maximize          _r_: Rotate right  _a_: Auto adjust
_-_: Zoom out _o_: Restore original  _l_: Rotate left   _i_: Imagemagick Hydra
^ ^           _s_: Save              ^ ^                _q_: Quit
"
    ("+" imagex-sticky-zoom-in)
    ("-" imagex-sticky-zoom-out)
    ("m" imagex-sticky-maximize)
    ("o" imagex-sticky-restore-original)
    ("s" imagex-sticky-save-image)
    ("r" imagex-sticky-rotate-right)
    ("l" imagex-sticky-rotate-left)
    ("a" imagex-auto-adjust-mode)
    ("i" imagemagick-hydra/body :exit t)
    ("q" nil))

  (defhydra imagemagick-hydra (:color red :hint nil)
    "
Zoom           ^^Manipulate           ^^Rotate           ^^Actions
--------------------------------------------------------------------------------
_+_: Zoom in    _h_: Fit to height     _r_: Rotate right  _a_: Auto adjust
_-_: Zoom out   _w_: Fit to width      _l_: Rotate left   _i_: Image+ Hydra
_s_: Set scale  _o_: Restore original  _R_: Rotate free   _q_: Quit
"
    ("+" (image-transform-set-scale (* image-transform-scale 1.1)))
    ("-" (image-transform-set-scale (* image-transform-scale 0.9)))
    ("s" image-transform-set-scale)

    ("h" image-transform-fit-to-height)
    ("w" image-transform-fit-to-width)
    ("o" image-transform-reset)

    ("r" (image-transform-set-rotation  (+ image-transform-rotation 90)))
    ("l" (image-transform-set-rotation  (- image-transform-rotation 90)))
    ("R" image-transform-set-rotation)

    ("a" imagex-auto-adjust-mode)
    ("i" imagex-hydra/body :exit t)
    ("q" nil))
  (define-key image-mode-map (kbd "?") #'imagemagick-hydra/body))


(use-package paredit
  :defer t
  :config
  (defhydra paredit-hydra (:color blue :columns 3 :idle 1.0)
    "Paredit"
    ("(" paredit-wrap-round "Wrap round")
    ("[" paredit-wrap-square "Wrap square")
    ("]" paredit-wrap-square "Wrap square")
    ("{" paredit-wrap-curly "Wrap curly")
    ("s" paredit-splice-sexp "Splice")
    ("S" paredit-split-sexp "Split")
    ("j" paredit-join-sexps "Join")
    ("J" paredit-join-with-next-list "Join next list")
    ("M-J" paredit-join-with-previous-list "Join prev list")
    ("C" paredit-convolute-sexp "Convolute")
    ("M-c" paredit-copy-as-kill "Copy as kill")
    ("r" paredit-raise-sexp "Raise s-expression")
    ("q" nil "Quit"))
  (define-key paredit-mode-map (kbd "C-c P") #'paredit-hydra/body))


(use-package lsp-mode
  :defer t
  :config
  (defhydra lsp-mode-hydra (:exit t :hint nil)
    "
 Buffer^^       Symbol^^                                                      Server
------------------------------------------------------------------------------------------------
 [_f_] Format   [_d_] Declaration  [_i_] Impl  [_o_] Documentation     [_M-s_] Session
 [_m_] Imenu    [_e_] Definition   [_t_] Type  [_O_] Organize Imports  [_M-r_] Restart
 [_x_] Execute  [_r_] References   [_l_] Lens  [_R_] Rename            [_S_] Shutdown
"
    ("f" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action)

    ("d" lsp-find-declaration)
    ("e" lsp-ui-peek-find-definitions)
    ("r" lsp-ui-peek-find-references)

    ("i" lsp-ui-peek-find-implementation)
    ("t" lsp-find-type-definition)
    ("l" lsp-lens-mode)

    ("o" lsp-describe-thing-at-point)
    ("O" lsp-organize-imports)
    ("R" lsp-rename)

    ("M-s" lsp-describe-session)
    ("M-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))
  (define-key lsp-mode-map (kbd "C-c l") #'lsp-mode-hydra/body))


(use-package gud
  :defer t
  :config

  (gud-def gud-record "record" nil "Enable recording.")
  (gud-def gud-record-stop "record stop" nil "Disable recording")
  (gud-def gud-exec-fwd "set exec-direction forward" nil "Forward Debugging")
  (gud-def gud-exec-rev "set exec-direction reverse" nil "Reverse Debugging")

  (defmacro defun-toggle (name &optional docstring decl &rest args)
    "Create a toggle function with NAME."
    (declare (indent 1))
    `(defun ,name ()
       ,docstring
       ,decl
       (if (get (function ,name) 'state)
           (progn
             (put (function ,name) 'state nil)
             ,(plist-get args :post))
         (put (function ,name) 'state t)
         ,(plist-get args :pre))))

  (defun-toggle gud-record-toggle
    "Toggle switch for the GUD (GDB) record command."
    (interactive)
    :pre (call-interactively #'gud-record)
    :post (call-interactively #'gud-record-stop))

  (defun-toggle gud-exec-dir-toggle
    "Toggle switch for the execution direction for the GUD (GDB) debugger."
    (interactive)
    :pre (call-interactively #'gud-exec-rev)
    :post (call-interactively #'gud-exec-fwd))

  (defhydra gud-hydra (:color pink :hint nil)
    "
 Source^^      Assembly^^      Breakpoints^^     Navigate^^    Reverse^^        Server^^
------------------------------------------------------------------------------------------------
 [_n_] Next    [_j_] Jump      [_b_] Breakpoint  [_M-p_] Up    [_R_] Record     [_r_] Run
 [_s_] Step    [_c_] Continue  [_t_] Tmp Break   [_M-n_] Down  [_D_] Direction  [_M-s_] Stop
 [_u_] Until   [_N_] Next      [_w_] Watch       [_p_] Print   ^   ^            [_C-c_] Interrupt
 [_f_] Finish  [_S_] Step      [_d_] Delete      [_l_] Refresh ^   ^            [_q_] Quit
"
    ("s" gud-step)
    ("n" gud-next)
    ("c" gud-cont)
    ("u" gud-until)
    ("f" gud-finish)
    ("j" gud-jump)

    ("S" gud-stepi)
    ("N" gud-nexti)

    ("b" gud-break)
    ("t" gud-tbreak)
    ("d" gud-remove)
    ("w" gud-watch)

    ("M-p" gud-up)
    ("M-n" gud-down)

    ("R" gud-record-toggle)
    ("D" gud-exec-dir-toggle)

    ("p" gud-print)
    ("l" gud-refresh)
    ("r" gud-run)
    ("M-s" gud-stop-subjob)
    ("C-c" gud-stop-subjob)
    ("q" nil)
    ("M-x" nil))
  (define-key gud-mode-map (kbd "C-c d") #'gud-hydra/body)
  (define-key gud-minor-mode-map (kbd "C-c d") #'gud-hydra/body))

;;; my-hydras.el ends here
