# Version Log

All notable changes to this project will be documented in this file. The current application version is **0.21.0**.

## 0.21.0 – 2025-12-09

### Fixed

- **SQL Injection Prevention**: Added whitelist validation for column names in `getUniqueColumnValues` and `getCountsByColumn` database methods.
- **Race Condition**: Fixed concurrent file processing issue in `processFilesToDb` using synchronous ref tracking.
- **Memory Leak**: Fixed `useResizeObserver` hook to use stable ref object in dependency array.
- **DOMParser Error Boundary**: Added try-catch wrapper around XML parsing to handle malformed input gracefully.
- **Type Coercion**: Removed unsafe `any` casting in TimeRangeSelector by leveraging TypeScript union narrowing.
- **Synchronous File Operations**: Converted all Electron IPC file handlers to async using `fs.promises` to prevent main process blocking.
- **Infinite Refresh Loop**: Fixed endless table refresh when window is resized by using ref to access `infiniteScrollChunkSize` without triggering effect re-runs.

### Improved

- **Filter Comparison Performance**: Replaced O(n log n) sort-based array comparison with O(n) Set-based approach.
- **WHERE Clause Memoization**: Added caching to `_buildWhereClause` to avoid repeated computation for identical filters.
- **Context Value Memoization**: Wrapped DataContext value in `useMemo` to prevent unnecessary consumer re-renders.
- **Regex Compilation**: Moved stock info regex to module level for single compilation at load time.
- **Array Pre-allocation**: Optimized log density array creation using pre-allocation and direct index assignment.
- **FTS5 Documentation**: Added documentation noting current LIKE query limitations and FTS5 as future upgrade path.
- **Lazy Loading**: Implemented React.lazy() for Settings, Info, and StockTracker tabs to improve initial load performance.
- **Debounce Hooks**: Created `useDebounce` and `useDebouncedCallback` custom hooks for filter input optimization.

### Code Quality

- **Type Safety**: Replaced `any` types with proper generic type parameters in `Bar` component. Fixed `Uint8Array` to `ArrayBuffer` compatibility in database export.
- **Named Constants**: Extracted magic numbers to named constants (`ROW_HEIGHT_COMPACT`, `ROW_HEIGHT_NORMAL`, `ROW_HEIGHT_COMFORTABLE`).
- **JSDoc Documentation**: Added documentation comments to key functions in `db.ts`, `parsers.ts`, and all context providers (`UIContext`, `SessionContext`, `ConsoleContext`) including their hooks.
- **React StrictMode**: Verified application runs with React StrictMode enabled.
- **Error Boundary**: Added `ErrorBoundary` component for graceful error handling and recovery.
- **Centralized Constants**: Created `constants.ts` with all app-wide configuration values (version, timing, sizes).
- **Worker Type Safety**: Added proper TypeScript interfaces for worker message payloads (`ImportLogsPayload`, `RebuildStockPayload`) and SqlJs types, eliminating `any` usage.

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

```markdown
## x.y.z – YYYY-MM-DD
### Added
- ...
### Changed
- ...
### Fixed
- ...
```

Include links to related issues or pull requests when available.
