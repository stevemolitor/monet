# Monet

![Water Lilies by Claude Monet](https://cdn.zappy.app/669c203862db189e5bfd2055f3a99bc4.png)
<sub>Water Lilies (1840–1926). Original from the Art Institute of Chicago.</sub>

Monet is an Emacs package that implements the (undocumented) [Claude Code](https://docs.anthropic.com/en/docs/claude-code) IDE protocol, enabling Claude to interact with your Emacs environment through a WebSocket connection.

You can use Monet with Claude Code running in your favorite terminal emulator (Ghostty, Kitty, iTerm2, WezTerm), or with packages like [claude-code.el](https://github.com/stevemolitor/claude-code.el) that run Claude Code directly inside Emacs.

_Warning:_ Monet is a work in progress. Expect bugs. 

## Features

- Selection context: current selection in Emacs is automatically shared with Claude Code
- Send diagnostics from Flymake/Flycheck (and thus LSP in LSP modes) to Claude
- Create diff views in Emacs before Claude applies changes
- Project-aware session management
- Multiple concurrent sessions support

## Requirements

- Emacs 30.0 or later
- [websocket](https://github.com/ahyatt/emacs-websocket) package

## Installation

### Using straight.el

```elisp
(straight-use-package
 '(monet :type git :host github :repo "stevemolitor/monet"))
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/stevemolitor/monet.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/monet")
   (require 'monet)
   ```

## Usage

### Quick Start

1. Enable Monet mode:
   ```elisp
   M-x monet-mode
   ```

2. Start a Monet server:
   ```
   C-c m s    ; Start server in current project/directory
   ```

3. In Claude Code, start a new chat and Claude will automatically connect to your Emacs session.

### Example

Here's Monet in action - Claude running in Ghostty terminal communicating with Emacs:

![Claude running in Ghostty communicating with Emacs](https://cdn.zappy.app/d38bcc5c3ee4894795dbbc5c1cd062e4.png)

### Key Bindings

When `monet-mode` is enabled, the following key bindings are available (default prefix: `C-c m`):

- `C-c m s` - Start server
- `C-c m q` - Stop server (with completion)
- `C-c m Q` - Stop all servers
- `C-c m l` - List active sessions
- `C-c m L` - Enable logging
- `C-c m D` - Disable logging

### Session Management

Monet automatically creates session keys based on your context:
- When in a project (via `project.el`), uses the project name
- Otherwise, uses the current directory name
- Automatically generates unique keys for multiple sessions (e.g., `project<2>`)

With a prefix argument (`C-u C-c m s`), you can manually select a directory.

### Customization

```elisp
;; Change the prefix key (default: "C-c m")
(setq monet-prefix-key "C-c C-m")

;; Or disable prefix key and use M-x commands only
(setq monet-prefix-key nil)

;; Change log buffer name
(setq monet-log-buffer-name "*My Monet Log*")
```

#### Custom Diff Tool

You can customize how Monet displays diffs by providing your own diff tool function. Diff tools must:

1. Accept parameters: `(old-file-path new-file-path new-file-contents on-accept on-quit)`
2. Display the diff to the user
3. Call `on-accept` when the user accepts changes or `on-quit` when they reject
4. Return a context object (an alist) containing at least a `control-buffer` entry

The context object should contain:
- `control-buffer` - The main buffer for the diff display (required)
- `old-temp-buffer` - Temporary buffer with old content (optional, for cleanup)
- `new-temp-buffer` - Temporary buffer with new content (optional, for cleanup)
- Additional entries as needed by your tool

Here's a working example using ediff:

```elisp
(defun my-ediff-tool (old-file-path new-file-path new-file-contents on-accept on-quit)
  "Use ediff to display changes with context object approach."
  (let ((old-buffer (find-file-noselect old-file-path))
        (new-buffer (generate-new-buffer "*monet-new*")))
    (with-current-buffer new-buffer
      (insert new-file-contents))
    
    ;; Create the context object that will be returned
    (let* ((window-config (current-window-configuration))
           (original-buffer (current-buffer))
           (context (list (cons 'control-buffer nil)  ; Will be set in startup hook
                          (cons 'on-accept on-accept)
                          (cons 'on-quit on-quit)
                          (cons 'new-buffer new-buffer)
                          (cons 'window-config window-config)
                          (cons 'original-buffer original-buffer))))
      
      ;; Start ediff with a startup hook to capture the control buffer
      (ediff-buffers old-buffer new-buffer
                     (list (lambda ()
                             ;; Store the control buffer in our context
                             (setf (alist-get 'control-buffer context) (current-buffer))
                             ;; Store context for access in hooks
                             (setq-local monet--diff-context context)
                             
                             ;; Define cleanup function
                             (defun my-janitor ()
                               (let ((context monet--diff-context))
                                 (when context
                                   ;; Call the appropriate callback
                                   (let ((on-quit (alist-get 'on-quit context)))
                                     (when (functionp on-quit)
                                       (funcall on-quit)))
                                   ;; Clean up
                                   (let ((new-buffer (alist-get 'new-buffer context)))
                                     (when (buffer-live-p new-buffer)
                                       (kill-buffer new-buffer)))
                                   ;; Restore window configuration
                                   (let ((window-config (alist-get 'window-config context)))
                                     (when window-config
                                       (set-window-configuration window-config)))))
                               ;; Call ediff's cleanup
                               (ediff-janitor nil nil))
                             
                             ;; Set up the hook
                             (add-hook 'ediff-cleanup-hook 'my-janitor nil t))))
      
      ;; Return the context object
      context)))

;; Set your custom diff tool
(setq monet-diff-tool 'my-ediff-tool)

;; Define a cleanup function that uses the context object
(defun my-ediff-cleanup (context)
  "Clean up ediff session using CONTEXT object."
  (when context
    (let ((control-buffer (alist-get 'control-buffer context)))
      (when (and control-buffer (buffer-live-p control-buffer))
        (with-current-buffer control-buffer
          (when (functionp 'ediff-quit)
            (ediff-quit nil)))))))

(setq monet-cleanup-diff-tool 'my-ediff-cleanup)
```

For simpler diff tools, you can use the built-in `monet-generic-diff-accept` and `monet-generic-diff-quit` functions, which handle the callbacks automatically when bound to keys like `y` and `q`.

## How It Works

Monet creates a WebSocket server that Claude Code connects to via MCP. This allows Claude to:

- Browse and open files in your project
- See real-time diagnostics from your linters
- Create side-by-side diffs for code review
- Track your current selection/cursor position

Each session is isolated to a specific directory/project, ensuring Claude only accesses files within the intended scope.

## Troubleshooting

- **Enable logging**: `C-c m L` to see all MCP communication
- **Check active sessions**: `C-c m l` to list all running servers
- **Connection issues**: Ensure no firewall is blocking local WebSocket connections
