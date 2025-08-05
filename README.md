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

#### Custom Diff Tool

You can customize how Monet displays diffs by providing your own diff tool functions. Set
`monet-diff-tool` to a function that takes the following arguments: `old-file-path new-file-path new-file-contents on-accept on-quit`, and set `monet-diff-cleanup-tool` to a function that 
takes a single context object.

Your diff tool function should display the diff to the user and return a context object that will be
passed to the diff cleanup tool later. The `on-accept` callback now takes a single argument - the
final content to be sent to Claude. For read-only diff tools (like the simple diff tool), pass the
original `new-file-contents`. For editable diff tools (like the ediff tool), pass the user's edited
content. Your diff tool should call `on-quit` when the user rejects the changes. Do not close your diff
tool buffers on accept or reject; the diff cleanup tool will do that.

The ediff tool demonstrates this pattern - it allows users to manually edit the proposed changes before
accepting them. When the user accepts the diff, the ediff tool extracts the edited content from the
buffer using `buffer-substring-no-properties` and passes it to Claude, allowing for manual refinements
to Claude's suggestions.

After calling your diff in response to a Claude request to display the diff tool, Monet will
store the context object returned from your diff tool function in the Monet session. When the user
accepts or rejects and your tool calls the `on-accept` or `on-quit` callbacks, Monet sends the
appropriate message to Claude. Claude will start accepting or rejecting the changes, and then send a
message to Monet to close the diff tabs. Monet will then call your diff cleanup tool with the
context object that was returned from your diff tool. Your diff cleanup tool should then kill diff
buffers and perform necessary cleanup.
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
