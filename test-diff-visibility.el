;;; test-diff-visibility.el --- Test script for context-aware diff hiding

;; This test demonstrates the context-aware diff hiding feature

;;; Test Setup
(require 'monet)

;; Enable the feature
(setq monet-hide-diff-when-irrelevant t)

(message "Testing context-aware diff hiding...")
(message "Feature enabled: %s" monet-hide-diff-when-irrelevant)

;; Create a mock session for testing
(let* ((session (make-monet--session
                 :key "test-session"
                 :directory "/Users/steve/repos/monet"
                 :port 12345
                 :initialized t
                 :auth-token "test-token"
                 :opened-diffs (make-hash-table :test 'equal)
                 :deferred-responses (make-hash-table :test 'equal)))
       (test-buffer (get-buffer-create "*test-file*"))
       (diff-buffer (get-buffer-create "*Diff: test*")))
  
  ;; Set up test buffer as if it's a file in the session directory
  (with-current-buffer test-buffer
    (setq buffer-file-name "/Users/steve/repos/monet/test.el"))
  
  ;; Create a mock diff context
  (let ((diff-context `((diff-buffer . ,diff-buffer)
                        (initiating-file . "/Users/steve/repos/monet/test.el")
                        (session-directory . "/Users/steve/repos/monet")
                        (tab-name . "test-tab"))))
    
    ;; Store the diff in the session
    (puthash "test-tab" diff-context (monet--session-opened-diffs session))
    
    ;; Test 1: Check if buffer is relevant to session
    (message "\nTest 1: Buffer relevance check")
    (message "  Buffer in session dir: %s" 
             (monet--is-buffer-relevant-to-session-p 
              test-buffer 
              "/Users/steve/repos/monet"
              "/Users/steve/repos/monet/test.el"))
    
    ;; Test 2: Check if diff should be shown
    (message "\nTest 2: Should show diff check")
    (message "  Should show diff for test buffer: %s"
             (monet--should-show-diff-p diff-context test-buffer))
    
    ;; Test 3: Check with unrelated buffer
    (let ((unrelated-buffer (get-buffer-create "*unrelated*")))
      (with-current-buffer unrelated-buffer
        (setq buffer-file-name "/tmp/unrelated.el"))
      (message "\nTest 3: Unrelated buffer check")
      (message "  Should show diff for unrelated buffer: %s"
               (monet--should-show-diff-p diff-context unrelated-buffer)))
    
    ;; Test 4: Check with initiating file
    (let ((initiating-buffer (get-buffer-create "*initiating*")))
      (with-current-buffer initiating-buffer
        (setq buffer-file-name "/Users/steve/repos/monet/test.el"))
      (message "\nTest 4: Initiating file check")
      (message "  Should show diff for initiating file: %s"
               (monet--should-show-diff-p diff-context initiating-buffer))))
  
  ;; Clean up
  (kill-buffer test-buffer)
  (kill-buffer diff-buffer))

(message "\nTest completed!")

;;; test-diff-visibility.el ends here