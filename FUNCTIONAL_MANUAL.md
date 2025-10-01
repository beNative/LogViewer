# Functional Manual

This guide explains how operators and support analysts can use **Log Analyser** to ingest integration logs, investigate incidents, and export curated datasets. It focuses on day-to-day workflows rather than implementation details.

## 1. Application overview

The application runs as a desktop window powered by Electron. A fixed header exposes the primary navigation tabs:

- **Data Hub** – manage sessions, import files, and download backups.
- **Viewer** – inspect log entries in a table with rich filtering.
- **Dashboard** – explore timelines and categorical charts derived from the current filters.
- **Stock Tracker** – query stock-related events reconstructed from logs.
- **Console** – review application diagnostics and background progress.
- **Settings** – customise appearance, layout, and saved filter presets.
- **Info** – product information, shortcuts, and support links.

A title bar surfaces the active view, while the status bar at the bottom echoes progress messages, counts, and warnings. Toast notifications appear in the bottom-right corner when user actions complete or require attention.

## 2. Starting the application

1. Install dependencies (`npm install`) and launch the desktop shell with `npm start`.
2. On first launch the Data Hub view opens with a new, unnamed session. When running as a packaged build the window opens directly to the last active session.
3. Drag the window edges to resize; the layout automatically adapts between compact and wide breakpoints.

## 3. Managing data in the Data Hub

### 3.1 Creating or extending a session

- **Create a new session** by dropping one or more XML log files onto the “Start a new session” dropzone.
- **Append files** to the active session by dropping them into the “Add Log Files” area inside the session summary card.
- The loader shows progress for each file; cancel import at any time from the progress overlay.

### 3.2 Session cards and metadata

Once data is loaded, the session card shows:

- Total records inserted into the database.
- The time range spanned by the logs.
- Total database size and contributing file names.

Use the pencil icon to rename the session in place. The name is validated and saved immediately.

### 3.3 Session management actions

- **New session** – Clears the current workspace and starts with an empty database.
- **Download database** – Exports the current SQLite database for archival or sharing.
- **Import database** – Replaces the active session with a previously exported database file.
- **Session list** – Browse saved sessions from the sidebar, open one, rename it, or delete it. A “dirty” indicator warns when unsaved changes exist.

## 4. Analysing logs in the Viewer

### 4.1 Layout

The viewer is split into three regions:

1. **Filter panel** (left) – choose date/time ranges, levels, sender types, sender names, file names, include/exclude phrases, and optional SQL queries. Toggle each field between include and exclude modes, and switch to advanced SQL when needed.
2. **Log table** (centre) – displays filtered entries with columns for timestamp, level, sender type/name, message, and source file. A secondary pane shows structured message breakdowns (key/value tables, SQL statements, or grid data) and raw text.
3. **Applied filter summary** (top) – chips representing active filters; click a chip to remove it quickly.

### 4.2 Navigating results

- Switch between **pagination** and **infinite scroll** in Settings. Pagination displays the current page number, total pages, and items per page selector.
- Change **table density** to display more rows on screen or prioritise readability.
- Toggle **detail panel visibility** and resize or pin sections via Settings → Layout.
- Use **keyboard navigation**: arrow keys move the active row, `Enter` focuses details, and the selected record persists in the detail panel.

### 4.3 Timeline & contextual filtering

- The log viewer integrates with the **timeline bar** showing log density. Drag to adjust the visible range or jump between highlighted dates.
- Right-click any table cell to add include/exclude filters immediately.
- Click counts in the detail sidebar (levels, sender types, files) to append that value to the current filters.
- Saved **filter presets** appear in the preset menu. Create a preset from the current filter state or delete existing ones from Settings.

### 4.4 Loading additional data

When filters yield more results than fit on screen:

- Click **Load more** (in infinite scroll mode) to fetch the next block of results.
- Use pagination controls to jump to a specific page or change the page size.
- The **status bar** reports how many entries match the filters and when more data is available.

## 5. Exploring trends on the Dashboard

- The **Log Volume Over Time** chart supports click-and-drag selection. Releasing the mouse applies the chosen time range to the viewer filters.
- The **Distribution by Log Level** and **Distribution by Sender Type** charts are clickable; selecting a slice adds the respective category to the filter set.
- Hover over points to see counts and exact timestamps.

## 6. Working with Stock Tracker

The Stock Tracker view reconstructs stock movement events stored in `stock_info`.

- Define a search term and optional date/time range, then click **Search** to update the results table.
- Auto-complete suggestions appear while typing; use arrow keys or the mouse to pick one.
- Select a history row to see detailed metadata and highlight the associated period on the timeline chart.
- Use **Rebuild stock data** to regenerate stock tables from the current log session. This operation can take time and will display progress in the status bar.
- Toggle the **timeline bar visibility** to compare stock events against overall log density.

## 7. Monitoring activity in Console and Status Bar

- The **Console** collects diagnostic messages with severity tags (DEBUG, INFO, WARNING, ERROR). Use the filter buttons to show or hide severities, search within messages, and clear the log when finished.
- The **Status Bar** mirrors high-level progress, including import counts, time range information, and warnings when the viewer is busy.

## 8. Configuring Settings

Settings persist per session unless explicitly reset. Key sections include:

- **Appearance** – Choose light or dark theme, select the icon set, and scale the UI.
- **Viewer layout** – Configure pagination vs infinite scroll, table density, column visibility, column ordering, and detail panel widths.
- **Timeline controls** – Show or hide the log timeline bar, reset zoom, and pick the default view range.
- **Filter presets** – Save the current filter state, load an existing preset, or delete unused presets.
- **Advanced options** – Enable SQL query logging to the console and toggle experimental focus debugging overlays when diagnosing keyboard focus issues.

## 9. Helpful shortcuts & tips

- Drag-and-drop anywhere within the window to import files; the app automatically routes them to the relevant dropzone.
- Hold `Shift` while selecting filters to extend multi-select fields (levels, sender types, sender names, files).
- Use the search box above the console to highlight matching terms in diagnostic messages.
- The Info view summarises keyboard shortcuts and displays the current application version.

## 10. Troubleshooting

| Symptom | Recommended action |
| --- | --- |
| Import stalls midway | Open the Console to confirm progress; if stuck, cancel via the progress overlay and retry with smaller batches. |
| Filters return no results | Check the time range boundaries, ensure include/exclude toggles are set correctly, and clear SQL mode if unnecessary. |
| Stock search is empty | Rebuild stock data to ensure derived tables match the current session contents. |
| Saved session missing | Confirm the session directory permissions in the packaged application’s data folder; sessions are stored per OS user. |

## 11. Getting help

Consult the Info tab for quick links to documentation and release notes. When escalating issues to engineering, include the exported database (`Download Database`) and a console log capture to speed up troubleshooting.
