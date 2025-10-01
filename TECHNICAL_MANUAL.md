# Technical Manual

This manual documents the internal architecture of **Log Analyser**, the primary code paths, and recommended practices for extending or maintaining the project.

## 1. System architecture

The application is delivered as an Electron desktop app. It consists of three cooperating runtimes:

1. **Electron main process** (`electron/main.js`) – bootstraps the BrowserWindow, manages auto-updates, persists settings and session metadata to the user’s data directory, and exposes an IPC bridge for renderer operations (session CRUD, file dialogs, shell integration).
2. **Renderer** (`index.tsx` → `App.tsx`) – a React 18 single-page application styled with Tailwind CSS. It orchestrates view rendering, data presentation, and user interactions through a set of context providers.
3. **Web Worker** (`src/workers/data-worker.ts`) – handles CPU-intensive tasks such as XML ingestion and stock data rebuilding using sql.js. Workers allow these operations to run off the UI thread.

```
┌──────────────┐          IPC            ┌────────────────┐
│ Electron Main│◄───────────────────────►│ Renderer (React)│
└──────────────┘                         └────────────────┘
         ▲                                       │
         │    file buffers / status events        ▼
         │                               ┌────────────────┐
         └──────────────────────────────►│ Web Worker     │
                                         └────────────────┘
```

## 2. Renderer composition

`App.tsx` composes the major feature areas (DataHub, LogTable, Dashboard, StockTracker, Console, Settings, Info) and overlays (About dialog, progress indicator, toasts, status bar). State and actions are injected via context hooks:

- **UIContext** – active view, modal visibility, loading/progress indicators, timeline selection, busy flags, and keyboard selection state.
- **SessionContext** – lifecycle of the current SQLite database, session persistence, worker orchestration, and file import/export commands.
- **DataContext** – derived log data (filters, pagination, unique values, timeline density, dashboards, stock history) built on top of the session database.
- **SettingsContext** – user preferences (theme, view mode, column visibility, presets, UI scale, SQL logging, timeline bar visibility).
- **ConsoleContext** – diagnostic log store with severity filters and search.
- **ToastContext** – transient notification queue.

`contexts/AppProvider.tsx` wires these providers together for the root of the component tree.

## 3. Data ingestion flow

1. SessionContext receives dropped files and triggers `processFilesToDb`.
2. Files are serialised (name + text) and posted to `worker.js` (bundled from `src/workers/data-worker.ts`).
3. The worker parses XML `<log ... />` elements using a lightweight regex parser, streams progress back to the renderer, and builds an in-memory sql.js database.
4. After import, the worker exports the database as an ArrayBuffer. The renderer recreates a `Database` instance via `Database.createFromBuffer` and replaces the current session state.
5. SessionContext updates metadata (record counts, time ranges, file list) and persists the session if running inside Electron (compressed via JSZip inside the main process IPC handlers).

## 4. Database layer

`db.ts` wraps sql.js, exposing convenience methods for:

- Creating/dropping tables and indexes (`logs`, `meta`, `stock_info`).
- Persisting metadata such as saved filters and dashboard configuration via the `meta` table.
- Querying filtered log entries, counts, timeline buckets, categorical distributions, unique column values, file time ranges, log density, and stock analytics.
- Inserting logs within transactions, exporting/importing buffers, and logging SQL when enabled (`setLogger`).

The renderer never interacts with sql.js directly; all operations go through this class to keep logging and schema migrations consistent.

## 5. Filtering and pagination

The DataContext maintains two filter states:

- `formFilters` – what the user is editing in the UI.
- `appliedFilters` – the snapshot pushed into database queries.

When the user applies filters, DataContext resets offsets, fetches counts, recomputes dashboards, and retrieves the first page (or chunk) of rows via `db.getFilteredEntries`. Infinite scroll is implemented by tracking an offset and requesting additional chunks sized to roughly two table viewports (computed from the current container height and density), whereas pagination derives `pageTimestampRanges` to support jumping between pages while maintaining timeline context.

Keyboard navigation and the timeline cursor are coordinated through `useUI` so other views (e.g., charts) can highlight the selected record.

## 6. Dashboard and timeline integration

`components/Dashboard.tsx` renders three charts:

- **TimelineChart** – area chart of log volume over time. Drag selection triggers `handleTimeRangeSelect`, which updates DataContext filters.
- **CategoryChart** – pie charts for levels and sender types. Slice clicks call `handleCategorySelect` to append filters.

Timeline density and date chips are computed via `db.getLogDensityByLevel` and `db.getDatesWithLogs`, feeding both the viewer’s timeline bar and Stock Tracker overlays.

## 7. Stock data pipeline

Stock info is derived from log messages that include `<StockInfoMessage>` payloads. The process is:

1. Worker rebuild: `handleRebuildStockData` clears `stock_info`, scans relevant log messages with a regex, and inserts structured records.
2. SessionContext receives the rebuilt database buffer, swaps the current Database instance, and updates stock metadata (overall time range, density heatmap).
3. DataContext’s stock actions (`handleSearchStock`, `handleFetchStockSuggestions`) execute SQL queries against `stock_info`, updating the StockTracker view with history tables and chart overlays.

## 8. Electron main process responsibilities

The main process (`electron/main.js`):

- Creates the BrowserWindow and loads either the development server (`http://localhost:3000`) or `index.html` from the packaged bundle.
- Persists user settings (`settings.json`) and session files (`sessions/`) under `app.getPath('userData')`. Session archives are stored as zipped SQLite databases.
- Exposes IPC handlers for session CRUD, file dialogs, logging, update toggles, and invoking shell commands.
- Configures `electron-updater` (auto updates, manual installation directory selection) with NSIS targets.
- Logs to per-day rotating files stored alongside settings for debugging.

## 9. Build tooling

- **Renderer bundling** – `esbuild` compiles `index.tsx` with JSX automatic runtime. Tailwind is processed via `postcss-cli` with a dedicated entry (`styles.css`).
- **Workers** – `src/workers/data-worker.ts` is compiled separately by esbuild into `dist/worker.js` during the build step.
- **Dev experience** – `npm run serve` launches esbuild in watch/serve mode, serving the renderer on `:3000` and recompiling Tailwind.
- **Packaging** – `npm run dist` / `dist:32` run the full build then call electron-builder, using configuration from `package.json` to produce NSIS installers in `release-builds/`.

## 10. Configuration & persistence

User-facing configuration lives in two places:

- **Renderer** – SettingsContext stores runtime preferences in React state and synchronises with the main process via IPC for persistence.
- **Main** – `settings.json` merges defaults with stored values, including migrations for older keys (e.g., `fontFamily` → `font`). Timeline bar visibility, column styles, filter presets, and SQL logging flags are all serialised.

Session databases are zipped (`.zip`) with metadata (size, timestamps) for listing in the Data Hub sidebar. When the renderer requests a session, the main process streams it back for hydration.

## 11. Coding standards & conventions

- Prefer React function components with hooks; avoid class components.
- Keep state colocated via contexts rather than prop drilling.
- Avoid direct DOM access unless necessary (use refs).
- Do not wrap imports in try/catch (see repository coding guidelines).
- Use Tailwind utility classes for styling; component-specific overrides belong in dedicated files.
- When touching sql.js, route changes through `db.ts` to keep logging and migrations coherent.

## 12. Testing & quality

The project currently lacks automated tests. Recommended additions include:

- Unit tests for parsers (`parsers.ts`) and worker regex heuristics.
- Integration tests covering filter application and pagination flows using Playwright or Spectron.
- Smoke tests for session import/export using prebuilt log fixtures.

Until a formal suite is introduced, rely on manual regression through the Functional Manual scenarios.

## 13. Adding new functionality

When introducing features:

1. Extend context types carefully; update both the provider and hook return values.
2. Keep worker payloads serialisable (plain objects/ArrayBuffers). Recompile `worker.js` after editing worker source.
3. Reflect new settings in both the renderer (SettingsContext defaults) and main process defaults (`getSettings`).
4. Update documentation (README, manuals) and the version log.
5. Test in both development server mode and packaged Electron to catch IPC or path issues.

## 14. Deployment & updates

- Set `GH_TOKEN` in the environment when running `npm run publish` to authenticate with GitHub releases.
- Configure `autoUpdater` policy (allow pre-release, auto-download) via the Settings view; values persist to `settings.json`.
- Installer customisation lives under the `build` section of `package.json` (appId, productName, NSIS options).

## 15. Troubleshooting for developers

| Issue | Resolution |
| --- | --- |
| `worker.js` missing at runtime | Ensure `npm run build` has been executed; the worker bundle lives in `dist/worker.js` and is required by SessionContext. |
| SQL queries return unexpected counts | Enable SQL logging in Settings → Advanced to mirror executed statements in the Console. |
| Electron cannot write sessions | Check filesystem permissions for `app.getPath('userData')`; logs in `app-log-*.log` provide stack traces. |
| TypeScript errors during build | Verify `tsconfig.json` path aliases and ensure new files have `.ts`/`.tsx` extensions recognised by esbuild. |

## 16. Resources

- Electron: [https://www.electronjs.org/docs/latest/](https://www.electronjs.org/docs/latest/)
- sql.js: [https://sql.js.org/](https://sql.js.org/)
- Tailwind CSS: [https://tailwindcss.com/docs](https://tailwindcss.com/docs)
- Electron Builder: [https://www.electron.build/](https://www.electron.build/)
