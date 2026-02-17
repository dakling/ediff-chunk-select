;;; ediff-chunk-select.el --- Per-hunk accept/reject for ediff  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Dario Klingenberg
;; Keywords: tools, convenience

;;; Commentary:

;; Layers per-hunk accept/reject on top of standard ediff sessions.
;; Each hunk can be independently accepted (use proposed change from B)
;; or rejected (keep original from A).  The result is assembled from
;; the selected combination of hunks.
;;
;; Optional Claude Code IDE integration is available via
;; `ediff-chunk-select-claude-code-setup'.

;;; Code:

(require 'cl-lib)
(require 'ediff)

;;; ─── Section 1: Core chunk-select ──────────────────────────────

;;; Faces

(defface ediff-chunk-select-pending-A
  '((t :background "#4a4000" :extend t))
  "Face for pending hunks in buffer A (undecided)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-pending-B
  '((t :background "#4a4000" :extend t))
  "Face for pending hunks in buffer B (undecided)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-accepted-A
  '((t :background "#1a3a1a" :extend t))
  "Face for accepted hunks in buffer A (will be replaced)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-accepted-B
  '((t :background "#1a3a1a" :extend t))
  "Face for accepted hunks in buffer B (chosen change)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-rejected-A
  '((t :background "#2a1a1a" :extend t))
  "Face for rejected hunks in buffer A (kept original)."
  :group 'ediff-chunk-select)

(defface ediff-chunk-select-rejected-B
  '((t :background "#2a1a1a" :strikethrough t :extend t))
  "Face for rejected hunks in buffer B (discarded)."
  :group 'ediff-chunk-select)

;;; Buffer-local state (in ediff control buffer)

(defvar-local ediff-chunk-select--hunk-states nil
  "Vector of `pending', `accepted', or `rejected' per diff index.")

(defvar-local ediff-chunk-select--overlays-A nil
  "Vector of overlay objects in buffer A.")

(defvar-local ediff-chunk-select--overlays-B nil
  "Vector of overlay objects in buffer B.")

(defvar-local ediff-chunk-select--undo-stack nil
  "List of (INDEX . OLD-STATE) for undo.")

(defvar-local ediff-chunk-select--completion-callback nil
  "Called with (ACCEPTED-P CONTENT HUNK-SUMMARY) on finish.
HUNK-SUMMARY is an alist with keys `total', `accepted', `rejected',
and `all-accepted'.")

(defvar-local ediff-chunk-select--active nil
  "Non-nil when chunk-select is active in this ediff session.")

;;; Setup hook variable (set before ediff-buffers, consumed in startup hook)

(defvar ediff-chunk-select--pending-callback nil
  "Callback for the next ediff session.  Consumed by the startup hook.")

;;; Overlay management

(defun ediff-chunk-select--face-for (state buffer-label)
  "Return the face for STATE (`pending', `accepted', `rejected') in BUFFER-LABEL (`A' or `B')."
  (pcase (cons state buffer-label)
    ('(pending . A)  'ediff-chunk-select-pending-A)
    ('(pending . B)  'ediff-chunk-select-pending-B)
    ('(accepted . A) 'ediff-chunk-select-accepted-A)
    ('(accepted . B) 'ediff-chunk-select-accepted-B)
    ('(rejected . A) 'ediff-chunk-select-rejected-A)
    ('(rejected . B) 'ediff-chunk-select-rejected-B)))

(defun ediff-chunk-select--diff-region (n buf-label)
  "Return (BEG . END) for diff N in buffer BUF-LABEL (`A' or `B').
Must be called from the ediff control buffer.  `ediff-get-diff-posn'
returns markers valid in the target buffer, so no buffer switch is needed."
  (cons (ediff-get-diff-posn buf-label 'beg n)
        (ediff-get-diff-posn buf-label 'end n)))

(defun ediff-chunk-select--create-overlays ()
  "Create overlays for all diffs.  Must be called from the control buffer."
  (let ((n ediff-number-of-differences))
    (setq ediff-chunk-select--overlays-A (make-vector n nil))
    (setq ediff-chunk-select--overlays-B (make-vector n nil))
    (dotimes (i n)
      (let ((region-a (ediff-chunk-select--diff-region i 'A))
            (region-b (ediff-chunk-select--diff-region i 'B)))
        (let ((ov-a (make-overlay (car region-a) (cdr region-a) ediff-buffer-A)))
          (overlay-put ov-a 'priority 100)
          (overlay-put ov-a 'face (ediff-chunk-select--face-for 'pending 'A))
          (overlay-put ov-a 'ediff-chunk-select t)
          (aset ediff-chunk-select--overlays-A i ov-a))
        (let ((ov-b (make-overlay (car region-b) (cdr region-b) ediff-buffer-B)))
          (overlay-put ov-b 'priority 100)
          (overlay-put ov-b 'face (ediff-chunk-select--face-for 'pending 'B))
          (overlay-put ov-b 'ediff-chunk-select t)
          (aset ediff-chunk-select--overlays-B i ov-b))))))

(defun ediff-chunk-select--update-overlay (n state)
  "Update overlays for diff N to reflect STATE."
  (when-let ((ov-a (aref ediff-chunk-select--overlays-A n)))
    (overlay-put ov-a 'face (ediff-chunk-select--face-for state 'A)))
  (when-let ((ov-b (aref ediff-chunk-select--overlays-B n)))
    (overlay-put ov-b 'face (ediff-chunk-select--face-for state 'B))))

(defun ediff-chunk-select--delete-all-overlays ()
  "Remove all chunk-select overlays."
  (when ediff-chunk-select--overlays-A
    (cl-loop for ov across ediff-chunk-select--overlays-A
             when ov do (delete-overlay ov)))
  (when ediff-chunk-select--overlays-B
    (cl-loop for ov across ediff-chunk-select--overlays-B
             when ov do (delete-overlay ov))))

;;; Header / mode line

(defun ediff-chunk-select--status-string ()
  "Return a string like [Chunks: 3a 2r 5p] for the header."
  (if (null ediff-chunk-select--hunk-states)
      ""
    (let ((accepted 0) (rejected 0) (pending 0))
      (cl-loop for s across ediff-chunk-select--hunk-states
               do (pcase s
                    ('accepted (cl-incf accepted))
                    ('rejected (cl-incf rejected))
                    ('pending  (cl-incf pending))))
      (format "[Chunks: %da %dr %dp]" accepted rejected pending))))

(defun ediff-chunk-select--update-header ()
  "Update the ediff control buffer header with chunk status."
  (when (and ediff-chunk-select--active
             (boundp 'ediff-control-buffer)
             (buffer-live-p ediff-control-buffer))
    (with-current-buffer ediff-control-buffer
      (setq header-line-format
            (concat " " (ediff-chunk-select--status-string)
                    "  a:accept  x:reject  u:undo  A:accept-all  X:reject-all  RET:finish")))))

;;; Next pending navigation

(defun ediff-chunk-select--next-pending (&optional from-index)
  "Return the index of the next pending hunk after FROM-INDEX, or nil."
  (let ((start (1+ (or from-index ediff-current-difference)))
        (n (length ediff-chunk-select--hunk-states)))
    (cl-loop for i from start below n
             when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
             return i)))

(defun ediff-chunk-select--auto-advance ()
  "Move to the next pending hunk if there is one."
  (when-let ((next (ediff-chunk-select--next-pending)))
    (ediff-jump-to-difference (1+ next))))  ; ediff uses 1-based

;;; Interactive commands

(defun ediff-chunk-select--set-hunk-state (state)
  "Set current hunk to STATE and auto-advance."
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (when (< ediff-current-difference 0)
    (user-error "No current difference"))
  (let ((idx ediff-current-difference)
        (old-state (aref ediff-chunk-select--hunk-states ediff-current-difference)))
    (push (cons idx old-state) ediff-chunk-select--undo-stack)
    (aset ediff-chunk-select--hunk-states idx state)
    (ediff-chunk-select--update-overlay idx state)
    (ediff-chunk-select--update-header)
    (ediff-chunk-select--auto-advance)))

(defun ediff-chunk-select-accept ()
  "Accept the current hunk (use version from buffer B)."
  (interactive)
  (ediff-chunk-select--set-hunk-state 'accepted))

(defun ediff-chunk-select-reject ()
  "Reject the current hunk (keep version from buffer A)."
  (interactive)
  (ediff-chunk-select--set-hunk-state 'rejected))

(defun ediff-chunk-select-undo ()
  "Undo the last accept/reject action."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (unless ediff-chunk-select--undo-stack
    (user-error "Nothing to undo"))
  (let* ((entry (pop ediff-chunk-select--undo-stack))
         (idx (car entry))
         (old-state (cdr entry)))
    (aset ediff-chunk-select--hunk-states idx old-state)
    (ediff-chunk-select--update-overlay idx old-state)
    (ediff-chunk-select--update-header)
    (ediff-jump-to-difference (1+ idx))))

(defun ediff-chunk-select-accept-all ()
  "Accept all remaining pending hunks."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (dotimes (i (length ediff-chunk-select--hunk-states))
    (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
      (push (cons i 'pending) ediff-chunk-select--undo-stack)
      (aset ediff-chunk-select--hunk-states i 'accepted)
      (ediff-chunk-select--update-overlay i 'accepted)))
  (ediff-chunk-select--update-header))

(defun ediff-chunk-select-reject-all ()
  "Reject all remaining pending hunks."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (dotimes (i (length ediff-chunk-select--hunk-states))
    (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
      (push (cons i 'pending) ediff-chunk-select--undo-stack)
      (aset ediff-chunk-select--hunk-states i 'rejected)
      (ediff-chunk-select--update-overlay i 'rejected)))
  (ediff-chunk-select--update-header))

(defun ediff-chunk-select-next ()
  "Move to the next difference (any state)."
  (interactive)
  (ediff-next-difference))

(defun ediff-chunk-select-prev ()
  "Move to the previous difference (any state)."
  (interactive)
  (ediff-previous-difference))

;;; Result assembly

(defun ediff-chunk-select--build-result ()
  "Build the result string by combining hunks from buffers A and B.
Accepted hunks take content from B, rejected/pending from A.
Inter-hunk regions always come from A.

Uses overlay positions rather than `ediff-get-diff-posn' so this works
even after `ediff-really-quit' has cleared ediff's internal diff vectors
\(which happens before quit hooks run)."
  (let ((n (length ediff-chunk-select--hunk-states))
        (parts nil)
        (prev-end-a nil))
    (with-current-buffer ediff-buffer-A
      (setq prev-end-a (point-min)))
    (dotimes (i n)
      (let* ((ov-a (aref ediff-chunk-select--overlays-A i))
             (ov-b (aref ediff-chunk-select--overlays-B i))
             (state (aref ediff-chunk-select--hunk-states i))
             (beg-a (overlay-start ov-a))
             (end-a (overlay-end ov-a))
             (beg-b (overlay-start ov-b))
             (end-b (overlay-end ov-b)))
        ;; Inter-hunk text from buffer A
        (push (with-current-buffer ediff-buffer-A
                (buffer-substring-no-properties prev-end-a beg-a))
              parts)
        ;; Hunk content: from B if accepted, from A if rejected/pending
        (push (if (eq state 'accepted)
                  (with-current-buffer ediff-buffer-B
                    (buffer-substring-no-properties beg-b end-b))
                (with-current-buffer ediff-buffer-A
                  (buffer-substring-no-properties beg-a end-a)))
              parts)
        (setq prev-end-a end-a)))
    ;; Remaining text after the last diff from buffer A
    (push (with-current-buffer ediff-buffer-A
            (buffer-substring-no-properties prev-end-a (point-max)))
          parts)
    (apply #'concat (nreverse parts))))

;;; Finish / finalize

(defun ediff-chunk-select-finish (&optional skip-quit)
  "Finalize the chunk-select session.
If hunks are still pending, prompt the user; pending defaults to rejected.
When SKIP-QUIT is non-nil, don't call `ediff-really-quit' (used when
called from the quit hook to avoid re-entrancy)."
  (interactive)
  (unless ediff-chunk-select--active
    (user-error "Chunk-select is not active"))
  (let ((pending-count (cl-count 'pending ediff-chunk-select--hunk-states)))
    (when (and (> pending-count 0)
               (not (y-or-n-p
                     (format "%d hunk(s) still pending (will keep original). Finish? "
                             pending-count))))
      (user-error "Aborted"))
    ;; Treat remaining pending as rejected
    (dotimes (i (length ediff-chunk-select--hunk-states))
      (when (eq (aref ediff-chunk-select--hunk-states i) 'pending)
        (aset ediff-chunk-select--hunk-states i 'rejected)))
    (let* ((result (ediff-chunk-select--build-result))
           (total (length ediff-chunk-select--hunk-states))
           (accepted-count (cl-count 'accepted ediff-chunk-select--hunk-states))
           (rejected-count (cl-count 'rejected ediff-chunk-select--hunk-states))
           (any-accepted (> accepted-count 0))
           (all-accepted (= rejected-count 0))
           (hunk-summary `((total . ,total)
                           (accepted . ,accepted-count)
                           (rejected . ,rejected-count)
                           (all-accepted . ,all-accepted)))
           (callback ediff-chunk-select--completion-callback))
      (ediff-chunk-select--delete-all-overlays)
      (setq ediff-chunk-select--active nil)
      ;; Quit ediff cleanly (skip when already inside ediff-really-quit)
      (unless skip-quit
        (ediff-really-quit nil))
      ;; Call the completion callback
      (when callback
        (funcall callback (if any-accepted t nil) result hunk-summary)))))

;;; Keymap setup

(defun ediff-chunk-select--setup-keymap ()
  "Install chunk-select keybindings in the ediff control buffer.
Called from `ediff-keymap-setup-hook'."
  (when ediff-chunk-select--active
    (let ((map ediff-mode-map))
      (define-key map "a" #'ediff-chunk-select-accept)
      (define-key map "x" #'ediff-chunk-select-reject)
      (define-key map "u" #'ediff-chunk-select-undo)
      (define-key map "A" #'ediff-chunk-select-accept-all)
      (define-key map "X" #'ediff-chunk-select-reject-all)
      (define-key map "]c" #'ediff-chunk-select-next)
      (define-key map "[c" #'ediff-chunk-select-prev)
      (define-key map (kbd "RET") #'ediff-chunk-select-finish)
      (define-key map "Q" #'ediff-chunk-select-finish))
    ;; Normalize evil keymaps if evil-collection is active
    (when (fboundp 'evil-normalize-keymaps)
      (evil-normalize-keymaps))))

;;; Session initialization (startup hook)

(defun ediff-chunk-select--startup-hook ()
  "Initialize chunk-select state when an ediff session starts.
Consumes `ediff-chunk-select--pending-callback'."
  (when ediff-chunk-select--pending-callback
    (let ((callback ediff-chunk-select--pending-callback))
      (setq ediff-chunk-select--pending-callback nil)
      (when ediff-control-buffer
        (ediff-chunk-select-activate ediff-control-buffer callback)))))

;;; Public API

;;;###autoload
(defun ediff-chunk-select-activate (control-buf callback)
  "Activate chunk-select in CONTROL-BUF with CALLBACK.
CALLBACK is called with (ACCEPTED-P CONTENT HUNK-SUMMARY) on finish.
Use this when the ediff session was created via `save-window-excursion'
and the startup hook didn't fire in the normal window context."
  (with-current-buffer control-buf
    (setq ediff-chunk-select--active t)
    (setq ediff-chunk-select--completion-callback callback)
    (setq ediff-chunk-select--hunk-states
          (make-vector (or ediff-number-of-differences 0) 'pending))
    (setq ediff-chunk-select--undo-stack nil)
    (when (buffer-live-p ediff-buffer-A)
      (with-current-buffer ediff-buffer-A
        (setq buffer-read-only t)))
    (when (buffer-live-p ediff-buffer-B)
      (with-current-buffer ediff-buffer-B
        (setq buffer-read-only t)))
    (ediff-chunk-select--create-overlays)
    (ediff-chunk-select--setup-keymap)
    (ediff-chunk-select--update-header)
    ;; The trailing t tells run-hooks to also run the global value
    ;; (ediff-cleanup-mess), which kills the control buffer and
    ;; auxiliary buffers.
    (setq-local ediff-quit-hook
                (list (lambda ()
                        (when ediff-chunk-select--active
                          (ediff-chunk-select-finish t)))
                      t))))

;;;###autoload
(defun ediff-chunk-select-enable-for-session (&optional callback)
  "Enable chunk-select for the next ediff session.
CALLBACK is called with (ACCEPTED-P NEW-CONTENTS) when the user finishes.
Call this before `ediff-buffers'."
  (setq ediff-chunk-select--pending-callback callback))

;;;###autoload
(defun ediff-chunk-select-buffers (buffer-a buffer-b)
  "Start an ediff session with per-hunk chunk-select between BUFFER-A and BUFFER-B.
Interactively, prompts for two buffers.  When finished, the result
is placed in a new buffer called *chunk-select-result*."
  (interactive
   (list (read-buffer "Buffer A (original): " (current-buffer) t)
         (read-buffer "Buffer B (proposed): " (other-buffer) t)))
  (ediff-chunk-select-enable-for-session
   (lambda (accepted-p content &optional _hunk-summary)
     (if (not accepted-p)
         (message "All hunks rejected — no changes.")
       (let ((buf (get-buffer-create "*chunk-select-result*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert content))
         (switch-to-buffer buf)
         (message "Result assembled with accepted hunks.")))))
  (ediff-buffers (get-buffer buffer-a) (get-buffer buffer-b)))

;;;###autoload
(defun ediff-chunk-select-files (file-a file-b)
  "Start an ediff session with per-hunk chunk-select between FILE-A and FILE-B.
Interactively, prompts for two files."
  (interactive "fFile A (original): \nfFile B (proposed): ")
  (ediff-chunk-select-enable-for-session
   (lambda (accepted-p content &optional _hunk-summary)
     (if (not accepted-p)
         (message "All hunks rejected — no changes.")
       (let ((buf (get-buffer-create "*chunk-select-result*")))
         (with-current-buffer buf
           (erase-buffer)
           (insert content))
         (switch-to-buffer buf)
         (message "Result assembled with accepted hunks.")))))
  (ediff-files file-a file-b))

;;; ─── Section 2: Claude Code IDE integration ────────────────────
;;
;; Optional integration with claude-code-ide.  None of the symbols
;; below are required at byte-compile time; everything is guarded
;; with `bound-and-true-p', `fboundp', or `declare-function'.
;; Call `ediff-chunk-select-claude-code-setup' to activate.

(defvar claude-code-ide-mcp--sessions)
(defvar claude-code-ide-show-claude-window-in-ediff)
(declare-function claude-code-ide-mcp-session-active-diffs "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp-session-project-dir "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide--display-buffer-in-side-window "claude-code-ide")
(declare-function claude-code-ide--get-buffer-name "claude-code-ide")
(declare-function claude-code-ide--session-buffer-p "claude-code-ide")
(declare-function claude-code-ide-mcp--find-session-for-file "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp--get-current-session "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp--get-active-diffs "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp-complete-deferred "claude-code-ide-mcp-handlers")
(declare-function claude-code-ide-mcp--handle-ediff-quit "claude-code-ide-mcp-handlers")

;; ── Diff queue ──────────────────────────────────────────────────

(defvar ediff-chunk-select-pending-queue nil
  "FIFO queue of (tab-name . control-buffer) for deferred diffs.")

(defvar ediff-chunk-select--pending-lighter
  '(:eval (if ediff-chunk-select-pending-queue
              (format " [%d diff%s]"
                      (length ediff-chunk-select-pending-queue)
                      (if (= 1 (length ediff-chunk-select-pending-queue)) "" "s"))
            ""))
  "Mode-line lighter showing pending diff count.")
(put 'ediff-chunk-select--pending-lighter 'risky-local-variable t)

(defvar ediff-chunk-select--current-tab-name nil
  "Tab-name of the diff currently being set up.")

(defvar ediff-chunk-select--current-file-path nil
  "File path of the diff currently being set up.")

;; ── Helpers ─────────────────────────────────────────────────────

(defun ediff-chunk-select--display-claude-side-window ()
  "Re-display the Claude Code side window if configured."
  (when (bound-and-true-p claude-code-ide-show-claude-window-in-ediff)
    (catch 'displayed
      (maphash
       (lambda (_proj-dir session)
         (when-let* ((proj (claude-code-ide-mcp-session-project-dir session))
                     (bn (claude-code-ide--get-buffer-name proj))
                     (cb (get-buffer bn)))
           (when (buffer-live-p cb)
             (claude-code-ide--display-buffer-in-side-window cb)
             (throw 'displayed t))))
       claude-code-ide-mcp--sessions))))

(defun ediff-chunk-select--find-diff-info (tab-name)
  "Find (session . diff-info) for TAB-NAME across all MCP sessions."
  (catch 'found
    (maphash
     (lambda (_proj-dir session)
       (let* ((active-diffs (claude-code-ide-mcp-session-active-diffs session))
              (diff-info (gethash tab-name active-diffs)))
         (when diff-info
           (throw 'found (cons session diff-info)))))
     claude-code-ide-mcp--sessions)
    nil))

(defun ediff-chunk-select--find-control-buffer (buffer-a)
  "Find the ediff control buffer whose `ediff-buffer-A' is BUFFER-A."
  (cl-find-if (lambda (buf)
                (and (buffer-live-p buf)
                     (eq (buffer-local-value 'ediff-buffer-A buf) buffer-a)))
              (buffer-list)))

;; ── Window management ───────────────────────────────────────────

(defun ediff-chunk-select--setup-windows (buf-a buf-b buf-c control-buf)
  "Ediff window setup that preserves the Claude Code side window.
Wraps `ediff-setup-windows-plain': removes side windows before
the layout build (which uses `other-window' and would land on a
surviving side window), then re-displays Claude afterward.
Preserves buffer-local `ediff-quit-hook' across the call."
  ;; Save quit hook — ediff-setup-control-buffer (called inside
  ;; ediff-setup-windows-plain) may reset buffer state.
  (let ((saved-quit-hook (buffer-local-value 'ediff-quit-hook control-buf))
        (saved-chunk-active (buffer-local-value 'ediff-chunk-select--active control-buf)))
    ;; Remove side windows so other-window doesn't cycle into them
    (dolist (window (window-list))
      (when (window-parameter window 'window-side)
        (ignore-errors (delete-window window))))
    (ediff-setup-windows-plain buf-a buf-b buf-c control-buf)
    ;; Restore quit hook and chunk-select state
    (with-current-buffer control-buf
      (setq-local ediff-quit-hook saved-quit-hook)
      (setq ediff-chunk-select--active saved-chunk-active))
    ;; ediff-setup-windows-plain ends with control window selected.
    ;; Re-display Claude side window.
    (let ((ctl-win (selected-window)))
      (ediff-chunk-select--display-claude-side-window)
      ;; Keep control window selected
      (select-window ctl-win))))

(defun ediff-chunk-select--show-ediff (control-buf)
  "Display the ediff session for CONTROL-BUF in the current frame."
  (when (buffer-live-p control-buf)
    (with-current-buffer control-buf
      (when (and (buffer-live-p ediff-buffer-A) (buffer-live-p ediff-buffer-B))
        ;; Use our wrapper as the window-setup-function for this session.
        ;; It clears side windows before ediff-setup-windows-plain (preventing
        ;; the other-window bug), re-adds Claude after, and preserves quit hooks.
        (setq-local ediff-window-setup-function
                    #'ediff-chunk-select--setup-windows)
        ;; Build layout via ediff's dispatcher (calls our wrapper since
        ;; ediff-keep-window-config is nil on first call).
        ;; ediff-setup-control-buffer stamps ediff-window-config-saved,
        ;; so subsequent j/k match and skip rebuilding.
        (ediff-setup-windows ediff-buffer-A ediff-buffer-B
                             ediff-buffer-C control-buf)
        ;; Jump to first diff
        (ignore-errors (ediff-next-difference))
        ;; Ensure control panel is selected
        (when (window-live-p ediff-control-window)
          (select-window ediff-control-window))
        ;; Defer Claude side-window display so it runs after all
        ;; synchronous window operations (ediff-recenter etc.) complete
        (run-with-idle-timer 0 nil #'ediff-chunk-select--display-claude-side-window)))))

;; ── Review pending diffs ────────────────────────────────────────

;;;###autoload
(defun ediff-chunk-select-review-pending ()
  "Pop the oldest pending diff from the queue and display it for review."
  (interactive)
  (unless ediff-chunk-select-pending-queue
    (user-error "No pending diffs to review"))
  (let* ((entry (car (last ediff-chunk-select-pending-queue)))
         (tab-name (car entry))
         (control-buf (cdr entry)))
    ;; Remove from queue (FIFO: take from end)
    (setq ediff-chunk-select-pending-queue
          (butlast ediff-chunk-select-pending-queue))
    (force-mode-line-update t)
    (if (not (buffer-live-p control-buf))
        (progn
          (message "Diff session for %s is no longer alive, skipping." tab-name)
          (when ediff-chunk-select-pending-queue
            (ediff-chunk-select-review-pending)))
      ;; Update saved-winconf to current layout so restoration goes back to HERE
      (when-let ((found (ediff-chunk-select--find-diff-info tab-name)))
        (let* ((session (car found))
               (active-diffs (claude-code-ide-mcp-session-active-diffs session))
               (diff-info (gethash tab-name active-diffs)))
          (when diff-info
            (setf (alist-get 'saved-winconf diff-info)
                  (current-window-configuration))
            (puthash tab-name diff-info active-diffs))))
      (ediff-chunk-select--show-ediff control-buf))))

;; ── Advice for openDiff / closeTab handlers ─────────────────────

(defun ediff-chunk-select--open-diff-advice (orig-fn arguments)
  "Set up chunk-select callback before claude-code-ide opens ediff.
Queues the diff instead of displaying it immediately."
  ;; Store in defvars to avoid lexical scoping issues across macro boundaries
  (setq ediff-chunk-select--current-tab-name (alist-get 'tab_name arguments))
  (setq ediff-chunk-select--current-file-path (alist-get 'old_file_path arguments))
  (let* ((the-tab-name ediff-chunk-select--current-tab-name)
         (the-file-path ediff-chunk-select--current-file-path)
         (in-claude-window (claude-code-ide--session-buffer-p (current-buffer)))
         (chunk-callback
          (lambda (accepted-p content &optional hunk-summary)
            (let* ((session (or (claude-code-ide-mcp--find-session-for-file the-file-path)
                                (claude-code-ide-mcp--get-current-session)))
                   (active-diffs (when session
                                   (claude-code-ide-mcp--get-active-diffs session)))
                   (diff-info (when active-diffs
                                (gethash the-tab-name active-diffs)))
                   (saved-winconf (when diff-info
                                    (alist-get 'saved-winconf diff-info))))
              ;; Defer window restoration so it runs AFTER ediff-really-quit
              (when saved-winconf
                (run-with-idle-timer
                 0 nil
                 (lambda ()
                   (set-window-configuration saved-winconf)
                   (ediff-chunk-select--display-claude-side-window))))
              ;; Send deferred MCP response
              (when session
                (run-with-idle-timer
                 0 nil
                 (lambda ()
                   (if accepted-p
                       (let* ((total (alist-get 'total hunk-summary 0))
                              (accepted-count (alist-get 'accepted hunk-summary 0))
                              (rejected-count (alist-get 'rejected hunk-summary 0))
                              (all-accepted (alist-get 'all-accepted hunk-summary t))
                              (response
                               (if all-accepted
                                   (list `((type . "text") (text . "FILE_SAVED"))
                                         `((type . "text") (text . ,content)))
                                 (list `((type . "text") (text . "FILE_SAVED"))
                                       `((type . "text") (text . ,content))
                                       `((type . "text")
                                         (text . ,(format "PARTIAL_EDIT: The user accepted %d of %d proposed changes. %d change(s) were rejected and the original code was kept for those hunks. The saved file content above reflects only the accepted changes. Do NOT re-propose the rejected changes."
                                                          accepted-count total rejected-count)))))))
                         (claude-code-ide-mcp-complete-deferred
                          session "openDiff" response the-tab-name)
                         (when active-diffs
                           (puthash the-tab-name
                                    (cons '(responded . t) diff-info)
                                    active-diffs)))
                     (claude-code-ide-mcp-complete-deferred
                      session "openDiff"
                      (list `((type . "text") (text . "DIFF_REJECTED"))
                            `((type . "text") (text . ,the-tab-name)))
                      the-tab-name)
                     (when active-diffs
                       (puthash the-tab-name
                                (cons '(responded . t) diff-info)
                                active-diffs))))))))))
    ;; Call original inside save-window-excursion to prevent display takeover
    (let ((result (save-window-excursion (funcall orig-fn arguments))))
      ;; Windows are now restored. Set up chunk-select and queue the diff.
      (condition-case err
          (let ((control-buf (car ediff-session-registry)))
            (when (and control-buf (buffer-live-p control-buf))
              ;; Activate chunk-select for this ediff session
              (ediff-chunk-select-activate control-buf chunk-callback)
              ;; Show immediately or queue based on whether user is in Claude window
              (if in-claude-window
                  (progn
                    ;; Update saved-winconf so restoration returns to current layout
                    (when-let ((found (ediff-chunk-select--find-diff-info the-tab-name)))
                      (let* ((session (car found))
                             (active-diffs (claude-code-ide-mcp-session-active-diffs session))
                             (diff-info (gethash the-tab-name active-diffs)))
                        (when diff-info
                          (setf (alist-get 'saved-winconf diff-info)
                                (current-window-configuration))
                          (puthash the-tab-name diff-info active-diffs))))
                    (ediff-chunk-select--show-ediff control-buf))
                (push (cons the-tab-name control-buf) ediff-chunk-select-pending-queue)
                (force-mode-line-update t)
                (message "[diff-queue] Queued diff for %s (%d pending)"
                         the-tab-name (length ediff-chunk-select-pending-queue)))))
        (error
         (message "[chunk-select] Error during setup: %s" err)))
      result)))

(defun ediff-chunk-select--close-tab-advice (orig-fn arguments)
  "Deactivate chunk-select before Claude closes a diff tab.
Also removes from pending diff queue if queued."
  (when-let ((tab-name (alist-get 'tab_name arguments)))
    ;; Remove from pending queue if present
    (setq ediff-chunk-select-pending-queue
          (cl-remove-if (lambda (entry) (equal (car entry) tab-name))
                        ediff-chunk-select-pending-queue))
    (force-mode-line-update t)
    (catch 'done
      (maphash
       (lambda (_proj-dir session)
         (let* ((session-diffs (claude-code-ide-mcp-session-active-diffs session))
                (diff-info (gethash tab-name session-diffs)))
           (when diff-info
             (when-let ((control-buf (alist-get 'control-buffer diff-info)))
               (when (buffer-live-p control-buf)
                 (with-current-buffer control-buf
                   (when (bound-and-true-p ediff-chunk-select--active)
                     (ediff-chunk-select--delete-all-overlays)
                     (setq ediff-chunk-select--active nil)
                     (setq ediff-quit-hook nil)))))
             (throw 'done t))))
       claude-code-ide-mcp--sessions)))
  (funcall orig-fn arguments))

;; ── Setup ───────────────────────────────────────────────────────

;;;###autoload
(defun ediff-chunk-select-claude-code-setup ()
  "Activate Claude Code integration for ediff-chunk-select.
Installs advice on openDiff/closeTab handlers and mode-line lighter."
  (advice-add 'claude-code-ide-mcp-handle-open-diff
              :around #'ediff-chunk-select--open-diff-advice)
  (advice-add 'claude-code-ide-mcp-handle-close-tab
              :around #'ediff-chunk-select--close-tab-advice)
  (unless (memq 'ediff-chunk-select--pending-lighter global-mode-string)
    (push 'ediff-chunk-select--pending-lighter global-mode-string)))

;;; ─── Hook installation ─────────────────────────────────────────

(add-hook 'ediff-startup-hook #'ediff-chunk-select--startup-hook)
(add-hook 'ediff-keymap-setup-hook #'ediff-chunk-select--setup-keymap)

(provide 'ediff-chunk-select)
;;; ediff-chunk-select.el ends here
