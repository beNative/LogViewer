# Log Analyser

Log Analyser is an Electron desktop application for exploring verbose XML-based integration logs without the friction of a full-featured database client. The app ingests one or more log exports, normalises the data into an embedded SQLite database, and then exposes a set of purpose-built tools to help support teams diagnose issues, understand message flow, and produce audit-friendly exports.

## Feature highlights

- **Session-based workflows** – Group multiple log files into named sessions that can be saved, reopened, renamed, or deleted at any time.
- **Flexible ingestion** – Drag-and-drop raw XML log exports, append files to an existing session, or import a previously saved database snapshot.
- **Powerful filtering** – Slice the data by time range, level, sender type, sender name, file, message contents, or ad-hoc SQL; toggle include/exclude modes per field and store reusable presets.
- **Log viewer ergonomics** – Switch between infinite-scroll and paginated layouts, adjust density, configure column visibility, and inspect structured message details with keyboard navigation.
- **Insightful dashboards** – Visualise log volume over time and distributions by level or sender type; selecting a range pushes filters directly into the viewer.
- **Stock information tracker** – Rebuild supply data from processed logs, search with auto-complete suggestions, and review history alongside log density overlays.
- **Operational console** – Monitor background tasks, SQL tracing, and parsing events with severity filters and keyword search.
- **Cross-platform packaging** – Ship ready-to-install builds for Windows (x64 and ia32) with Electron Builder and auto-update support through GitHub releases.

## Getting started

### Prerequisites

- Node.js 18 or later (Electron 29 requires modern Node features).
- npm 9+ (bundled with Node installations).
- macOS, Windows, or Linux for development; Windows for building NSIS installers.

### Installation

```bash
npm install
```

### Run in development

There are two development workflows depending on whether you need hot reloading:

| Scenario | Commands |
| --- | --- |
| **Iterating on the renderer UI** (fast rebuilds, dev server on port 3000) | `npm run serve` |
| **Running the full Electron shell** (builds assets once, launches desktop window) | `npm start` |

> When using `npm run serve`, esbuild serves assets from `http://localhost:3000`. Launch Electron manually in a second terminal with `npm start` once the dev server is ready.

### Build production assets

```bash
npm run build
```

### Package desktop installers

Electron Builder scripts are provided for common targets:

- `npm run dist` – 64-bit Windows installer.
- `npm run dist:32` – 32-bit Windows installer.
- `npm run dist:64` – Explicit 64-bit build (alias of `dist`).
- `npm run publish` – Build and upload a release using GitHub publisher settings from `package.json`.

Generated installers are written to the `release-builds/` directory.

## Using the application

1. **Open the Data Hub** (default landing view). Create a new session by dropping log files into the upload panel or choose an existing session from the sidebar. You can also import a `.db` snapshot or download the active session for archival.
2. **Review session metadata.** The session summary card reports record counts, storage footprint, covered time range, and the list of contributing log files. Drop additional files onto the card to extend the dataset.
3. **Switch to the Viewer tab** to inspect log entries. Apply filters via the form on the left, use quick chips to remove active filters, and toggle between pagination or continuous scrolling. The detail panel displays parsed key/value, SQL, and grid payloads alongside the raw message.
4. **Analyse trends on the Dashboard tab.** Drag across the timeline to constrain the viewer to a specific interval, or click a pie slice to include the corresponding level or sender type.
5. **Investigate stock movements via Stock Tracker.** Submit searches with optional date/time boundaries, browse auto-complete suggestions, and rebuild derived stock data when the source logs change. The timeline bar can show log density to correlate operational spikes.
6. **Monitor progress in the Console and Status Bar.** Background imports, SQL execution (when enabled), and worker activity surface here. Filter messages by severity or keyword and clear the console when finished.
7. **Adjust preferences in Settings.** Choose light or dark theme, icon set, table density, column visibility, detail panel behaviour, timeline display, and manage saved filter presets.

## Project structure

```
├── App.tsx                # Top-level composition of views and shared UI overlays
├── components/            # Reusable UI elements (DataHub, LogTable, Dashboard, StockTracker, etc.)
├── contexts/              # React context providers for UI, settings, data, session, toast, and console state
├── db.ts                  # Thin SQLite wrapper (sql.js) for storing and querying log data
├── parsers.ts             # Heuristics for parsing structured content from log messages
├── server.js              # Minimal static file server (used when packaging web assets)
├── electron/              # Electron main/renderer preload scripts and auto-update wiring
├── styles.css & index.css # Tailwind configuration entry points
├── tsconfig.json          # TypeScript compiler options
└── vite.config.ts         # Build configuration for the renderer bundle
```

## Technology stack

- **Electron 29** for cross-platform packaging and automatic updates.
- **React 18** with context providers to coordinate UI and data state.
- **Tailwind CSS 3** and PostCSS for styling.
- **esbuild** for fast bundling in development and production.
- **sql.js** (SQLite compiled to WebAssembly) for local, session-scoped storage.

## Troubleshooting

- **Blank window after launching Electron** – Ensure `npm run serve` is running if you expect hot reloads. Otherwise run `npm start` to bundle assets before launching.
- **Slow imports or UI freezes** – Large XML batches run in a Web Worker. Keep the console open to view progress; you can cancel via the progress indicator overlay.
- **Auto-update errors** – Confirm `publish` settings in `package.json` point to a GitHub repository with releases enabled and a valid token when publishing.
- **Missing dependencies** – Delete `node_modules` and reinstall (`rm -rf node_modules package-lock.json && npm install`) to refresh.

## License

This project is licensed under the MIT License. See the license header in `package.json` for details.
