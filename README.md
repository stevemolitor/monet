# Monet

![Claude Monet Self Portrait](https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Autoportret_Claude_Monet.jpg/512px-Autoportret_Claude_Monet.jpg)

<sub>Self Portrait with a Beret, 1886 by Claude Monet. Source: Wikimedia Commons</sub>

Monet is an Emacs package that implements the (undocumented) [Claude Code](https://docs.anthropic.com/en/docs/claude-code) IDE protocol, enabling Claude to interact with your Emacs environment through a WebSocket connection.

You can use Monet with Claude Code running in your favorite terminal emulator (Ghostty, Kitty, iTerm2, WezTerm), or with packages like [claude-code.el](https://github.com/stevemolitor/claude-code.el) that run Claude Code directly inside Emacs.

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

### Using use-package with :vc (Emacs 30+)

```elisp
(use-package monet
  :vc (:fetcher github :repo "stevemolitor/monet"))
```

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

3. In Claude Code, start a new chat and use the /ide slash command to connect to your Emacs session.

To have Claude automatically connect to your Monet session set `ENABLE_IDE_IDE=t` before starting Claude.

If you have multiple Monet sessions for the same project you can do this to have Claude automatically connect to the desired instance:

```sh
ENABLE_IDE_INTEGRATION=t && CLAUDE_CODE_SSE_PORT=123456 && claude
```

Monet prints a message with the port number when you call `monet-start-server` (`C-c m s`). You can see the list of all running servers with their ports and directories via `monet-list-sessions` (`C-c m l`). 

### Using Monet with [claude-code.el](https://github.com/stevemolitor/claude-code.el)

You can use Monet with [claude-code.el](https://github.com/stevemolitor/claude-code.el) by adding this hook:


```elisp
(add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
```

When claude-code.el starts a new session it will start and associate a Monet session with the current claude-code.el instance. 

### Session Management

Sessions are automatically cleaned up (killed) when you exit the associated Claude session. When you exit Emacs all sessions are cleaned up. You can stop a session manually via `monet-stop-server` (`C-c q`).

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

### Using the Diff Tools

When Claude proposes code changes, Monet displays them in a diff view:

- **Simple Diff Tool** (default): A read-only diff view showing the proposed changes
  - Press `y` to accept Claude's changes exactly as shown
  - Press `q` to reject the changes
  
- **Ediff Tool**: An interactive diff view that allows you to edit the changes before accepting
  - Navigate between differences using `n` (next) and `p` (previous)
  - Edit the proposed changes directly in the buffer
  - Press `C-c C-c` to accept your edited version (your changes will be sent to Claude)
  - Press `q` to reject all changes

**Important**: With the ediff tool, any manual edits you make to the proposed changes are captured and sent to Claude when you accept. This allows you to refine Claude's suggestions before applying them.

### Session Management

Monet automatically creates session keys based on your context:
- When in a project (via `project.el`), uses the project name
- Otherwise, uses the current directory name
- Automatically generates unique keys for multiple sessions (e.g., `project<2>`)

With a prefix argument (`C-u C-c m s`), you can manually select a directory.

You can start multiple sessions per project, or have multiple 

### Customization

```elisp
;; Change the prefix key (default: "C-c m")
(setq monet-prefix-key "C-c C-m")

;; Or disable prefix key and use M-x commands only
(setq monet-prefix-key nil)

;; Change log buffer name
(setq monet-log-buffer-name "*My Monet Log*")

;; Customize diff keybindings
(setq monet-ediff-accept-key "C-c C-a")      ; Default: "C-c C-c"
(setq monet-ediff-quit-key "C-g")            ; Default: "q"
(setq monet-simple-diff-accept-key "C-c C-c")      ; Default: "y"
(setq monet-simple-diff-quit-key "C-g")      ; Default: "q"

;; Change ediff window split direction
(setq monet-ediff-split-window-direction 'vertical)  ; Default: 'horizontal
```

#### Customizing MCP Tools

Most MCP tools that Claude uses to interact with Emacs are now customizable. You can replace the default implementations with your own functions:

```elisp
;; Custom file opener that confirms before opening
(defun my-open-file-tool (uri)
  (when (y-or-n-p (format "Open %s? " uri))
    (monet-default-open-file-tool uri)))
(setq monet-open-file-tool 'my-open-file-tool)

;; Custom diagnostics that only reports errors
(defun my-diagnostics-tool (&optional uri)
  (let ((result (monet-flymake-flycheck-diagnostics-tool uri)))
    ;; Filter to only errors... 
    result))
(setq monet-diagnostics-tool 'my-diagnostics-tool)
```

Available customizable tools:
- `monet-get-current-selection-tool` - Get current text selection
- `monet-get-latest-selection-tool` - Get latest selection from any file
- `monet-open-file-tool` - Open files in the editor
- `monet-save-document-tool` - Save documents to disk
- `monet-check-document-dirty-tool` - Check for unsaved changes
- `monet-get-open-editors-tool` - List open files
- `monet-get-workspace-folders-tool` - List project directories
- `monet-diagnostics-tool` - Get diagnostics (defaults to Flymake/Flycheck)

#### Custom Diff Tool

You can customize how Monet displays diffs by providing your own diff tool functions:

```elisp
;; Use a custom diff tool
(setq monet-diff-tool 'my-diff-tool)
(setq monet-diff-cleanup-tool 'my-diff-cleanup)
```

The diff tool function should take: `(old-file-path new-file-path new-file-contents on-accept on-quit)` and return a context object. The cleanup function takes that context object for cleanup.

## How It Works

Monet creates a WebSocket server that Claude Code connects to via MCP. This allows Claude to:

- Browse and open files in your project
- See real-time diagnostics from your linters
- Create side-by-side diffs for code review
- Track your current selection/cursor position

Each session is isolated to a specific directory/project, ensuring Claude only accesses files within the intended scope.

## Troubleshooting

- **Check active sessions**: `C-c m l` to list all running servers
- **Enable logging**: `C-c m L` to see all MCP communication

## Images

### Simple Monet Diff Tool with Ghostty

![Monet and Ghostty Diff](https://cdn.zappy.app/99797ecef1d5a12fc36bb72b39aa8464.png)

### Monet Ediff Tool inside claude-code.el

![Monet Ediff in claude-code.el](https://cdn.zappy.app/9198756659a0fcfedda2a14bc6b5fdb0.png)


