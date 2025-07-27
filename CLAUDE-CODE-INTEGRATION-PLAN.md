# Simplified Plan for Integrating Monet into claude-code.el

## Overview
Since we control both packages and neither is released yet, we can use a direct integration approach where claude-code.el passes the buffer name as the key and directory to monet.

## Changes to Monet (Already Implemented)

1. **Made `monet-start-server-in-directory` public**
   - claude-code.el can call this directly with buffer-name as key

2. **Added public API functions**:
   - `monet-session-port` - Get port from session
   - `monet-get-session` - Get session by key (buffer name)
   - `monet-cleanup-session` - Alias for monet-stop-server

## Changes Needed in claude-code.el

### 1. Update Package Dependencies
```elisp
;; Package-Requires: ((emacs "30.0") (transient "0.9.3") (websocket "1.15") (monet "0.0.1"))
```

### 2. Replace require statement (line 19)
```elisp
(require 'monet)  ; Instead of (require 'claude-code-mcp)
```

### 3. Update Server Start (line 1170-1172)
```elisp
(mcp-port (when claude-code-enable-ide-integration
            (let ((session (monet-start-server-in-directory buffer-name dir)))
              (when session
                (monet-session-port session)))))
```

### 4. Update Session Storage (line 1197)
```elisp
(setq claude-code--mcp-session (monet-get-session buffer-name))
```

### 5. Update Session Cleanup (line 1075)
```elisp
(monet-cleanup-session (buffer-name buffer))
```

## Benefits

1. **Direct Integration**: No wrapper functions needed
2. **Simple Key Management**: Buffer names work perfectly as keys
3. **Clean API**: Only using public functions
4. **Future Flexibility**: Both packages can evolve independently

## Implementation Steps

1. [x] Make necessary monet functions public
2. [ ] Update claude-code.el package dependencies
3. [ ] Replace all claude-code-mcp references with monet
4. [ ] Test the integration
5. [ ] Remove claude-code-mcp.el from the repository

## Testing Plan

1. Start Claude Code and verify MCP connection works
2. Test multiple concurrent Claude buffers
3. Verify cleanup on buffer kill
4. Test all IDE features (file open, diagnostics, diffs)