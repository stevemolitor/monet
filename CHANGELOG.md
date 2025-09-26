# Changelog

All notable changes to this project will be documented in this file.

## [0.0.3]

### Fixed
  - Fix line number reporting when narrowing is in effect

## [0.0.2]

### Added
  - Option to disable diff tools entirely by setting `monet-diff-tool` to `nil`
  - When disabled, Claude uses its built-in diff display instead of creating diff views in Emacs
  - Diff-related MCP tools (`openDiff` and `closeAllDiffTabs`) are conditionally excluded from the tools list
