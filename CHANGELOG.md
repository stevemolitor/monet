# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added
- Option to disable diff tools entirely by setting `monet-diff-tool` to `nil`
  - When disabled, Claude uses its built-in diff display instead of creating diff views in Emacs
  - Diff-related MCP tools (`openDiff` and `closeAllDiffTabs`) are conditionally excluded from the tools list