# Version Log

All notable changes to this project will be documented in this file. The current application version is **0.20.0**.

## 0.20.0 – 2024-06-01

### Added
- Session management hub with drag-and-drop imports, session renaming, and SQLite export/import workflows.
- Interactive Dashboard view featuring log volume timeline brushing and categorical distribution charts.
- Stock Tracker with rebuildable stock_info dataset, timeline overlays, auto-complete suggestions, and searchable history table.
- Focus debugger overlay, UI scale control, timeline bar visibility toggles, and customizable column styles.
- Console enhancements including severity filters, keyword search, and SQL query logging toggle.

### Improved
- Log viewer performance with infinite scroll chunking, adjustable density, and keyboard-driven selection.
- Worker-based import pipeline with granular progress phases and cancellation hooks.
- Timeline integration across viewer, dashboard, and stock tracker via shared density calculations.
- Electron session persistence with per-day log files and resilient settings merging.

### Fixed
- Stock rebuild worker now guarantees the `stock_info` table exists before clearing data and handles non self-closing `<Article>` tags.
- Safer parsing of key/value and SQL message payloads in the detail panel heuristics.
- Prevented concurrent worker executions by guarding long-running import/rebuild tasks.

## Earlier releases

Historical release notes prior to 0.20.0 were not captured in this repository. Future updates should append sections above with the format:

```
## x.y.z – YYYY-MM-DD
### Added
- ...
### Changed
- ...
### Fixed
- ...
```

Include links to related issues or pull requests when available.
