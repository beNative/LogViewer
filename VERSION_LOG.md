# Version Log

This document tracks the major changes, new features, and bug fixes for the Log Analyser application.

---

### **Version 0.15.0** - Professional Auto-Updates

_Date: 2025-09-02_

This release overhauls the application's update mechanism to be more professional, informative, and user-friendly.

#### ✨ Features & Improvements
- **Toast-Based Update Notifications**: Replaced the native OS dialogs for updates with a clean, non-intrusive toast notification system. Users are now informed of every stage of the update process (checking, downloading, ready to install) without interrupting their workflow.
- **Pre-release Update Channel**: Added a new "Update to Pre-releases" toggle in the Settings tab. This allows users to opt-in to receive beta versions of the application, giving them early access to new features. A restart is required for this change to take effect.
- **Robust Update Logic**: The underlying update logic in the Electron main process has been significantly improved to provide more detailed status events to the UI, ensuring the toast notifications are always accurate.

---

### **Version 0.14.0** - Full-Screen Application Log & UX Polish

_Date: 2025-09-01_

This release enhances the developer experience by providing an improved, full-screen view for the Application Log with new filtering capabilities.

#### ✨ Features & Improvements
- **Full-Screen Application Log**: The Application Log view now utilizes the full client area, removing the inset card-like container for a more expansive and immersive diagnostic experience.
- **Log Filtering and Highlighting**: A new search bar has been added to the Application Log header. Users can now type to filter messages in real-time, and any matching text is highlighted, making it much easier to find specific information.
- **Scrollable Log View**: Fixed a bug where the Application Log view was not scrollable, preventing access to older messages when the content overflowed the screen.

---

### **Version 0.13.0** - UI/UX & System Polish

_Date: 2025-08-31_

This release focuses on a wide range of user interface enhancements, bug fixes, and under-the-hood improvements to make the application more robust, visually appealing, and intuitive to use.

#### ✨ Features & Improvements
- **Colored Header Navigation**: The main navigation buttons in the header now feature a distinct color scheme, making it easier to identify the active tab and improving the overall visual appeal.
- **Improved UI Scaling**: The UI Scale slider in the Settings now has a dedicated "Apply" button, giving users more deliberate control over when the interface is rescaled.
- **Icon System Overhaul**: The entire icon system has been re-engineered. All icons have been replaced with high-quality, verified versions, fixing rendering glitches. The "solid" icon set is now complete, ensuring a consistent look when switching themes.

#### 🐛 Bug Fixes
- **Status Bar Layout**: Fixed a critical bug where the status bar would detach from the bottom of the screen when the UI scale was adjusted.
- **Icon Set Switching**: Corrected an issue where switching between icon sets in the settings did not visually update the icons in the UI.
- **Build Process Reliability**: Improved the build scripts to prevent "Access is denied" errors on Windows, making the packaging process more reliable.
- **Packaged Documentation**: Fixed a bug that caused the markdown manual files to be missing from the final packaged Electron application.
- **Settings Layout**: The configuration cards on the Settings page are now stacked vertically for better readability and responsiveness.

#### 🛠️ Under the Hood
- **Code Cleanup**: Performed a significant code cleanup, removing obsolete components (`VirtualizedLogView`), unused files, and refactoring shared code for better organization.
- **Styling Consolidation**: Refactored the application's styling to consistently use Tailwind CSS utility classes, improving maintainability and readability.

---

### **Version 0.12.0** - Advanced Settings Management

_Date: 2025-08-30_

This release introduces powerful new capabilities for managing application settings, giving advanced users direct control over the configuration.

#### ✨ Features & Improvements
- **Editable Settings JSON**: The "JSON Source" tab within the Settings page now features a full, syntax-highlighted editor, allowing for direct modification of the `settings.json` content.
- **Import/Export Settings**: Users can now export their current settings to a `.json` file for backup or sharing. A corresponding import function allows for quickly loading a configuration file.
- **Robust Save/Discard**: When editing the settings JSON, users can save their changes to apply them globally or discard them to revert to the last saved state. The editor provides real-time validation to prevent saving invalid JSON.

---

### **Version 0.11.0** - Exclusion Filters & UI Polish

_Date: 2025-08-29_

This release brings a powerful and highly requested feature to the forefront: the ability to manage exclusion filters directly from the main filter panel.

#### ✨ Features & Improvements
- **Exclusion Filters in UI**: The "Log Attributes" section of the filter panel now includes dedicated multi-select dropdowns for exclusion filters (e.g., "Level is NOT", "Sender Type is NOT"). This powerful feature, previously only accessible via the right-click context menu, is now fully manageable from the main UI for creating more complex queries.
- **Active Exclusion Pills**: When an exclusion filter is active, it now appears as a distinct red "pill" in the "Active Filters" bar, making it easy to distinguish from standard inclusion filters at a glance.
- **Improved Filter Logic**: The underlying filter state management and UI components have been updated to cleanly separate inclusion and exclusion criteria, improving maintainability and clarity.

---

### **Version 0.10.5** - UI Overhaul: Status Bar & Toolbar

_Date: 2025-08-28_

This release introduces a major user interface enhancement by adding a persistent global status bar and reorganizing the Log Viewer controls for a more intuitive and informative experience.

#### ✨ Features & Improvements
- **New Global Status Bar**: A new status bar has been added to the bottom of the application. It provides at-a-glance metrics (total vs. filtered entry count), session status, context-aware view controls (like pagination), a live feed of the latest application log message, and a quick-access theme toggle.
- **Dedicated Log Viewer Toolbar**: To de-clutter the interface, the controls for managing the table's appearance (View Mode, Density, Columns, Details toggle) have been moved into a clean new toolbar that sits directly above the log table.
- **Layout Stability Fix**: Resolved a critical CSS layout bug that caused the resizable side panels (Filters and Details) to become misaligned and unresponsive.

---

### **Version 0.10.4** - Documentation Engine Polish

_Date: 2025-08-27_

This is a minor release that enhances the in-app documentation viewer.

#### ✨ Features
- **Strikethrough Support**: The internal markdown parser for the "Info" tab now supports strikethrough formatting (using `~~text~~`). This allows for more expressive and accurate documentation within the application.

---

### **Version 0.10.3** - Bug Fixes & UX Polish

_Date: 2025-08-26_

This release addresses several bugs related to UI feedback and timeline interaction, improving the application's stability and user experience.

#### 🐛 Bug Fixes & Improvements
- **Scroll Mode Jump-to-Time**: Fixed a critical bug in "Scroll" mode where clicking the timeline to jump to a log entry would fail if the entry wasn't already loaded, causing the cursor to snap back to its previous position. The application now correctly fetches and displays the data surrounding the target time.
- **Progress Dialog Reset**: Corrected an issue where the "Processing Files..." dialog would show stale information from a previous import when loading a new session. The dialog is now properly initialized for each operation.
- **Improved Pagination Feedback**: The log table in "Paginate" mode now displays an animated "Processing..." indicator while fetching data, replacing the misleading "No log items found" message that was shown during loading.

---

### **Version 0.10.2** - Timeline Interaction & Zoom

_Date: 2025-08-25_

This release fixes a critical bug in the timeline selector and introduces an intuitive auto-zooming feature for a more seamless analysis workflow.

#### ✨ Features & Fixes
- **Auto-Zoom on Selection**: When clicking a "Date" or "File" segment in the timeline, the view now automatically zooms in to that specific time range.
- **Contextual Bar Fix**: This auto-zoom behavior fixes a bug where the other contextual bars (like "Pages" and "Density") would appear to vanish. They were rendering correctly but on a timeline that was too wide to see them. Now they are always visible within the focused, zoomed-in view.
- **Zoom Reset**: The "Zoom to Extent" button is now correctly enabled after auto-zooming, allowing for a quick and easy way to return to the full timeline view.

---

### **Version 0.10.1** - Time Selector Polish & Interactivity

_Date: 2025-08-24_

This release adds several usability improvements to the Time Range Selector, making it more intuitive and interactive.

#### ✨ UI/UX Improvements
- **Draggable Cursor**: The main red cursor line in the time selector is now draggable with the mouse, allowing for faster and more fluid seeking through the timeline.
- **Improved Handle Visibility**: The start and end handles for the selected time range are now thicker and styled with the primary blue color, significantly improving their visibility and making them easier to grab.
- **Centered Filename**: The filename text within its bar in the time selector is now properly centered, aligning its look and feel with other elements.

---

### **Version 0.10.0** - Multi-Bar Time Range Selector

_Date: 2025-08-23_

This release completely redesigns the time range selector into a powerful, multi-layered data visualization tool, providing rich context at a glance.

#### ✨ Features
- **Multi-Bar Contextual Display**: The single timeline has been replaced with a set of stacked horizontal bars, each offering a different perspective on the data:
    - **Page Bar**: When in "Paginate" mode, this bar visualizes each page as a distinct colored block, with its width representing the time duration.
    - **File Bar**: This bar shows each log file as a colored section, making it easy to see the time range covered by individual files.
    - **Log Density Heatmap**: A new heatmap bar visualizes the concentration of log messages over time. Darker "hot" areas indicate high log volume, while lighter "cool" areas show lulls, allowing for instant identification of activity spikes.
- **Unified Selection Overlay**: The time selection is now a single, translucent overlay that spans all bars, making it clear how the selected range correlates with pages, files, and log density.
- **Improved Interaction**: The intuitive drag-to-select and handle adjustment functionality is preserved, now operating on the new multi-bar visualization for a more integrated and powerful filtering experience.

#### 🛠️ Under the Hood
- Added new, efficient database queries to fetch the data required for the new visualizations (time ranges per file, log density).
- Completely rewrote the `TimeRangeSelector.tsx` component to support the new multi-bar layout and data sources.

---

### **Version 0.9.10** - Definitive Layout Fix (CSS Grid)

_Date: 2025-08-22_

#### 🐛 Bug Fixes
- **Definitive Layout Fix for Large Tables**: Replaced the internal `flexbox` layout of the `VirtualizedLogView` component with a more robust **CSS Grid** layout (`grid-rows-[auto_1fr]`). This is a definitive fix for the persistent bug where the view would overflow its container on very large datasets, which was caused by a browser layout engine quirk. The grid layout strictly constrains the scrolling area, ensuring the application layout remains stable regardless of data size.
- **Debug Tool Accuracy**: Fixed a bug in the layout debugging tool that caused it to measure the wrong elements inside the virtualized view, leading to confusing and incorrect reports. The tool is now accurate.

---

### **Version 0.9.9** - Final Layout Fix & Enhanced Debugging

_Date: 2025-08-21_

#### 🐛 Bug Fixes
- **Definitive Layout Fix**: Applied `overflow-hidden` to the direct parent of the virtualized view. This acts as a hard container, preventing the view's enormous virtual size from causing the browser's flexbox engine to miscalculate the layout and push the footer off-screen. This should finally resolve the persistent layout bug on very large tables.

#### ✨ Features
- **Enhanced Comparative Debugging**: Following a user suggestion, the layout debugging tool has been significantly enhanced. It now performs manual, step-by-step height calculations for each container and compares the expected values against the browser's actual rendered dimensions, providing a detailed report to pinpoint the exact source of any layout discrepancy.

---

### **Version 0.9.8** - Comparative Layout Debugging

#### ✨ Features
- **Enhanced Layout Debugging**: Overhauled the layout debugging tool. It now calculates the *expected* height of each critical container and compares it against the *actual* rendered height. The report clearly indicates which container is overflowing and by how much, providing crucial data to finally diagnose the persistent layout bug with very large tables.

---

### **Version 0.9.7** - Layout Stability Fix (Flexbox Re-implementation)

#### 🐛 Bug Fixes
- **Definitive Layout Fix**: Reverted the main layout from CSS Grid back to a more robust and standard Flexbox implementation to definitively fix the overflow bug with large tables. This uses a canonical 'sticky footer' pattern which is more stable for this type of complex layout problem.

---

### **Version 0.9.6** - Layout Debugging Tool

#### ✨ Features
- **Layout Debugging Tool**: Added a new "Debug" button and menu to the Log Viewer footer. This provides a "Log Layout Dimensions" action which prints a detailed report of the key layout containers' sizes and positions to the **Application Log**. This is intended to help diagnose the persistent and difficult-to-reproduce layout bug where the scroll view expands past the viewport on very large datasets.

---

### **Version 0.9.5** - Flexbox Layout Stability Fix

#### 🐛 Bug Fixes
- **Layout Stability with Large Tables**: Fixed a critical and persistent flexbox layout bug where the "Scroll" mode view would incorrectly expand and push the footer off-screen when dealing with very large datasets (100,000+ records). This was caused by the browser struggling to calculate the layout with an extremely large virtualized spacer element. Adding `min-h-0` to the correct container forces it to be constrained by its parent, ensuring the layout remains stable regardless of the data size.

---

### **Version 0.9.4** - Stability & UX Polish

This release focuses on fixing critical stability bugs related to view switching and improving the user experience during data loading operations.

#### ✨ UI/UX Improvements
- **Subtle Loading Indicator**: Replaced the disruptive, full-screen "Processing..." spinner with a non-intrusive indicator in the main header. The application remains fully interactive during data operations.

#### 🐛 Bug Fixes
- **Application Freeze**: Fixed a major bug that caused the application to freeze when switching from "Scroll" mode (after loading many entries) back to "Paginate" mode.
- **Layout Integrity**: Resolved a persistent layout issue where the virtualized scroll view would expand and push the footer off-screen. The view is now correctly contained.
- **Scrollbar Jitter**: Fixed a bug that caused the scrollbar to jitter and reset its position when switching from pagination to scroll mode while an item was selected.
- **Unwanted Panel Display**: Prevented the Log Details panel from appearing unexpectedly when scrolling in the virtualized view.

---

### **Version 0.9.3** - Keyboard Navigation Overhaul

This release significantly enhances keyboard navigation in the Log Viewer, making it faster and more intuitive, especially in pagination mode.

#### ✨ Features
- **Intuitive Pagination Navigation**:
    - `PageUp`/`PageDown` now navigate to the first/last item on the *current* page.
    - `Ctrl + PageUp`/`Ctrl + PageDown` now navigate to the previous/next page.
- **Global `Home` and `End` Keys**:
    - `Home`/`End` now navigate to the first/last item on the *current* page.
    - `Ctrl + Home`/`Ctrl + End` now jump to the absolute first item on page 1 or the absolute last item on the last page, respectively.

#### 🐛 Bug Fixes
- **Consistent Selection**: Fixed a bug where navigating to a previous page with the keyboard (`Ctrl+PgUp` or `ArrowUp`) would incorrectly select the first item instead of the last, which is now corrected.

---

### **Version 0.9.2** - UI Enhancements & Persistence

This release introduces major UI layout flexibility and bug fixes for a smoother user experience.

#### ✨ Features
- **Resizable Filter Panel**: The left-hand filter panel is now resizable, allowing users to customize their workspace layout.
- **Persistent Panel Widths**: The application now remembers and restores the widths of both the filter panel and the log details panel across sessions by saving them to `settings.json`.

#### 🐛 Bug Fixes
- **Markdown Rendering**: Fixed a bug where underscores in text (e.g., `_Date: ..._`) were not rendered correctly as italics. The parser is now more robust.

---

### **Version 0.9.1** - Advanced Filter Presets

This release enhances the power of filter presets by allowing advanced SQL queries to be saved and reused.

#### ✨ Features
- **SQL Query Presets**: The "Presets" section in the filter bar is now enabled when using an advanced SQL query. This allows users to save, load, and manage complex SQL queries as presets, making it easy to reuse them.

---

### **Version 0.9.0** - Filter Preset UI Overhaul

This release fixes a critical bug preventing users from saving custom filter presets and replaces the problematic UI with a modern, reliable inline form.

#### 🐛 Bug Fixes
- **Save Filter Preset**: Fixed a major bug where the "Save As Preset..." button did nothing, making it impossible to save custom filters. The underlying `window.prompt` was unreliable and has been completely removed.

#### ✨ Features
- **Inline Preset Naming**: When saving a filter preset, a new inline form now appears directly in the filter panel, allowing users to enter a name and confirm or cancel the action within the app's UI. This is a much more robust and user-friendly approach.

---

### **Version 0.8.2** - Reliability and UX Improvements

This release focuses on improving the reliability of settings management and providing a more detailed and accurate user experience during file processing.

#### 🐛 Bug Fixes
- **Filter Preset Saving**: Fixed a race condition that could cause saved filter presets (or other settings) to be lost if multiple settings were changed in quick succession. All settings are now saved using a more robust `async/await` pattern to ensure changes are applied sequentially.

#### 🎨 Improvements
- **Granular Import Progress**: The progress bar for importing and processing log files has been made more accurate and detailed. The progress is now weighted more realistically (30% for reading/unzipping, 70% for parsing/database insertion). The status messages are also more informative, showing the number of records being inserted from each file.

---

### **Version 0.8.1** - Build & Initialization Fixes

This is a maintenance release that addresses several bugs related to the build process and application startup.

#### 🐛 Bug Fixes
- **Build Error**: Fixed a build failure caused by a syntax error (a stray character) in the `BusySpinner.tsx` component.
- **Initialization Error**: Resolved a `ReferenceError` for `handleNewSession` that could occur on startup by reordering function definitions within `App.tsx` to respect initialization order.
- **Build Warning**: Suppressed a Tailwind CSS performance warning by making the content scanning paths in `tailwind.config.js` more specific, preventing it from scanning the entire `node_modules` directory.

---

### **Version 0.8.0** - Desktop Session Management

This version introduces robust session management for the desktop application, allowing users to save, load, and manage their analysis sessions effortlessly.

#### ✨ Features
- **Automatic Session Saving**: When running as a desktop app, any new data (from log files or imported DBs) is automatically saved as a new timestamped `.sqlite` session file.
- **Session Manager UI**: The Data Hub on desktop now features a dedicated Session Manager.
- **Browse & Load Sessions**: View a list of all saved sessions with details like file size and modified date. Load any session with a single click.
- **Rename & Delete**: Easily rename sessions for better organization or delete old sessions directly from the UI.
- **New Blank Session**: A "New Blank Session" button has been added for a clean start.
- **Streamlined Workflow**: The desktop workflow is now centered around creating new sessions or loading existing ones.

#### 🎨 Improvements
- **Data Hub Redesign (Desktop)**: The Data Hub has been redesigned to accommodate the new session management features, separating session management from file import actions.

#### 🛠️ Under the Hood
- Added extensive new IPC handlers in `electron/main.js` for listing, loading, saving, renaming, and deleting session files stored in the user's application data directory.
- Created a new `SessionManager.tsx` component to handle the session list UI and interactions.
- Refactored `App.tsx` and `DataHub.tsx` to be session-aware when running in an Electron environment.

---

### **Version 0.7.0** - Light Theme & UI Polish

This is a major visual overhaul of the application.

#### ✨ Features
- **Light & Dark Themes**: The application now supports both a polished dark theme and a new, clean light theme.
- **Theme Toggle**: A theme toggle has been added to the **Settings** tab, allowing users to switch between modes instantly. The preference is saved and loaded on startup.
- **Colorful Buttons**: In light mode, buttons now use more color to help group them by function and make primary actions stand out.

#### 🎨 Improvements
- **Theme-Aware Charts**: All charts in the Dashboard now adapt their colors to the current theme for optimal readability.
- **UI Consistency**: Every component has been reviewed and updated to ensure a consistent, high-quality look and feel in both light and dark modes.

#### 🛠️ Under the Hood
- Replaced simple CSS with a **PostCSS/Tailwind CSS** build process to enable advanced theme-aware styling.
- Refactored all components to use Tailwind's `dark:` variants for styling.

---

### **Version 0.6.4** - UI Text Improvement

#### 🎨 Improvements
- Renamed "Select Visible" and "Deselect Visible" buttons in the filter dropdowns to the more conventional "Select All" and "Deselect All". The functionality remains scoped to the visible (searched) items for better usability.

---

### **Version 0.6.3** - Filter Usability Improvement

#### ✨ Features
- **Bulk Select/Deselect in Filters**: Added "Select Visible" and "Deselect Visible" buttons to the multi-select dropdowns in the filter panel. This allows users to quickly select or deselect all items that match the current search term within the dropdown, significantly speeding up the filter configuration process for long lists.

---

### **Version 0.6.2** - Dashboard Interaction Fix

#### 🐛 Bug Fixes
- **Dashboard Filter Sync**: Fixed a critical bug where selecting a time range on the dashboard's timeline chart would not correctly update the date/time filters in the Log Viewer UI. This was due to a stale state closure and has been resolved by refactoring the state update logic to be more robust.

---

### **Version 0.6.1** - Timeline Chart Display Fix

#### 🐛 Bug Fixes
- **Timeline Chart Axis Labels**: Fixed an issue where the timeline chart's horizontal axis would only display the time and not the date, which was confusing for data spanning multiple days. The chart is now configured to show both date and time on its labels when appropriate.

---

### **Version 0.6.0** - Dashboard & Viewer Synchronization

This version introduces a major architectural change to properly synchronize the Log Viewer and the Dashboard, making the application's behavior more intuitive and robust.

#### ✨ Features
- **True Synchronization**: The Dashboard and Log Viewer now only update when the "Apply" button is clicked in the filter panel, ensuring the visualizations and the log table always reflect the same data set.
- **Seamless Chart Interaction**: Selecting a time range on the timeline chart or clicking a category on a donut chart now updates the filters in the sidebar and immediately applies them, refreshing both the Log Viewer and the Dashboard in one action.

#### 🎨 Improvements
- **Tab Order**: The "Dashboard" tab has been moved after the "Log Viewer" tab for a more logical workflow.

#### 🛠️ Under the Hood
- Refactored state management in `App.tsx` to use `formFilters` (for live input changes) and `appliedFilters` (for submitted queries), which is now the single source of truth for all data fetching.
- Consolidated all data fetching for the viewer and dashboard into a single `useEffect` hook that reacts to changes in `appliedFilters`.

---

### **Version 0.5.1** - Data Visualization Dashboard [DEPRECATED]

_This version's approach was found to be insufficient and has been superseded by the architecture in v1.5.0. It attempted to sync the views but lacked the robust state separation needed._

#### ✨ Features
- **Dynamic Dashboard**: The Dashboard updates to visualize the data subset that matches the active filters in the Log Viewer.
- **Smarter Interactions**: Clicking a slice on a dashboard category chart *adds* to the current filter set instead of replacing it.

---

### **Version 0.5.0** - Data Visualization Dashboard

This is a major feature release that introduces a new Dashboard tab for high-level data analysis through interactive charts.

#### ✨ Features
- **New Dashboard Tab**: A new primary view has been added to provide a visual overview of the loaded log data.
- **Interactive Timeline Chart**: Displays log volume over time in a bar chart. Users can click and drag to select a time range, which automatically applies a filter and switches to the Log Viewer. The time grouping (by minute, hour, or day) is dynamically adjusted based on the total time span of the data.
- **Category Distribution Charts**: Two donut charts show the distribution of logs by `Level` and `Sender Type`. Clicking on a slice of a chart filters the Log Viewer by that category.

#### 🛠️ Under the Hood
- Integrated `Chart.js` for rendering all visualizations.
- Added `getLogVolumeByInterval` and `getCountsByColumn` methods to the database class for efficient data aggregation.

---

### **Version 0.4.0** - UX and Filter Improvements

This version introduces significant usability improvements to the filtering and data analysis workflow.

#### ✨ Features
- **Interactive Filter Assistance**: Multi-select dropdowns in the filter panel now include a search input, making it much easier and faster to find specific items in long lists (e.g., Sender Names).
- **Search Term Highlighting**: When filtering by "Message contains", the matching search terms are now automatically highlighted in the log table and the detail panel, providing immediate visual feedback.

#### 🛠️ Under the Hood
- Added a new `utils.ts` module with a `highlightText` helper function to safely handle the search term highlighting logic.

---

### **Version 0.3.0** - UI Text Improvement

#### 🎨 Improvements
- Renamed "Diagnostic Console" to "Application Log" across the UI and in all documentation for better clarity and a more intuitive user experience.

---

### **Version 0.2.0** - Console Filtering

This version introduces powerful filtering capabilities to the Diagnostic Console.

#### ✨ Features
- **Console Filtering**: Added toggle buttons to the console header to filter messages by type. Each button includes a counter for the number of messages in its category.
- **New Message Types**: Console message types have been revised to `DEBUG`, `INFO`, `WARNING`, and `ERROR` for better clarity and alignment with standard logging practices.

#### 🎨 Improvements
- Added a new `ExclamationTriangleIcon` for the `WARNING` message type.
- Re-classified existing console log messages to fit the new, more descriptive types.

---

### **Version 0.1.0** - Initial Release

This is the initial version of the Log Analyser, establishing the core feature set and architecture.