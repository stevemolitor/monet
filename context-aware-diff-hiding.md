# Context-Aware Diff Hiding Implementation Plan

## Overview
Hide diff buffers when the user is editing files outside the associated Claude session context, showing them only when relevant.

## Implementation Approach

### 1. Add Customization Variable
- `monet-hide-diff-when-irrelevant` - boolean flag (default nil for backward compatibility)
- When enabled, diffs are only shown when editing relevant files

### 2. Enhanced Diff Context Storage
Store additional information in the diff context (already in `opened-diffs` hash):
- `:initiating-file` - The file that was active when diff was triggered
- `:session-directory` - The session's root directory
- `:diff-buffer` - Reference to the diff buffer (already stored)
- `:tab-name` - The unique identifier (already stored)

### 3. Visibility Management

#### Core Function: `monet--update-diff-visibility`
- Runs on post-command-hook when feature is enabled
- For each session with active diffs:
  - Check if current buffer is relevant to that session
  - Show/hide diff windows accordingly

#### Relevance Rules
A diff should be visible when:
1. Current buffer's file is under the session directory
2. Current buffer's file matches the initiating file
3. Current buffer IS the diff buffer itself

### 4. Implementation Steps

#### Step 1: Add customization variable
```elisp
(defcustom monet-hide-diff-when-irrelevant nil
  "When non-nil, hide diff buffers when editing unrelated files.
Diff buffers will only be shown when editing files within the
associated Claude session directory or the file that initiated
the diff request."
  :type 'boolean
  :group 'monet-tool)
```

#### Step 2: Modify `monet--tool-open-diff-handler`
- Capture the current buffer's file when diff is created
- Store this in the diff context

#### Step 3: Update `monet-simple-diff-tool`
- Include initiating file and session directory in returned context
- Conditionally display based on current context when feature is enabled

#### Step 4: Create visibility management functions
- `monet--is-buffer-relevant-to-session-p` - Check if buffer belongs to session
- `monet--should-show-diff-p` - Determine if diff should be visible
- `monet--update-diff-visibility` - Main visibility update function
- `monet--show-diff-window` - Show a hidden diff
- `monet--hide-diff-window` - Hide a visible diff

#### Step 5: Hook management
- Add/remove post-command-hook based on:
  - Feature enabled state
  - Presence of active diffs

### 5. Window Management Strategy

Rather than killing/recreating windows, we'll:
- Use `set-window-dedicated-p` to prevent accidental reuse
- Store window configuration in diff context
- Hide by switching window to a different buffer
- Show by restoring the diff buffer to its window

### 6. Future Enhancements

#### For ediff (later):
- Store control buffer as primary buffer to show/hide
- Optionally hide all three ediff buffers (A, B, control)
- Use ediff's window configuration management

## Testing Plan

1. **Basic functionality**
   - Enable feature and create a diff
   - Switch to file outside session - diff should hide
   - Switch back to session file - diff should reappear

2. **Multiple sessions**
   - Create diffs in two different sessions
   - Verify each shows only in its context

3. **Edge cases**
   - Files opened from outside session directory
   - Switching between multiple diff buffers
   - Closing diff buffers manually

4. **Performance**
   - Ensure post-command-hook doesn't impact editing performance
   - Test with many open buffers and active diffs

## Backward Compatibility

- Feature is disabled by default
- Existing behavior unchanged when `monet-hide-diff-when-irrelevant` is nil
- No changes to existing diff tool interfaces