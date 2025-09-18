# Log Analyser - Technical Manual

This document outlines the technical architecture and implementation details of the Log Analyser application.

## 1. Core Architecture

The Log Analyser is a **desktop application** built using **Electron**. This architecture encapsulates the web-based frontend into a native shell, allowing it to run as a standalone application on the user's computer. All processing, data storage, and rendering happen locally.

- **Application Shell**: **Electron** is used to create the native application wrapper and provides access to the underlying operating system via Node.js APIs.
- **Framework**: The UI is built using **React** with **TypeScript**. It leverages functional components and hooks for state management and side effects.
- **Bundler**: **esbuild** is used for fast and efficient bundling of the TypeScript and React code.
- **Data Storage**: Log data is stored in an in-memory **SQLite database** powered by **sql.js**, a library that compiles SQLite to WebAssembly. This allows for powerful and efficient SQL-based querying directly in the browser context of the Electron window. The database is structured into a main `logs` table and a specialized `stock_info` table for optimized queries.
- **Log Parsing Engine (`parsers.ts`)**: A sophisticated parsing module that analyzes raw log messages. It uses a series of heuristics to detect and format various content types, including XML, key-value pairs (with complex prefixes), multi-format tables, and SQL statements. It provides the structured data for the Log Viewer's "Parsed" details tab.
- **Data Visualization**: Charts are rendered using **Chart.js**, a powerful and flexible open-source charting library. The application also uses `chartjs-adapter-date-fns` for time-series axes and `chartjs-plugin-zoom` for interactive drag-to-select functionality.
- **Styling**: **Tailwind CSS** is used for utility-first styling. A **PostCSS** build step processes the CSS to include vendor prefixes and apply Tailwind transformations. The application has been refactored to consolidate all styling into utility classes and a central stylesheet, removing inline styles for improved maintainability.

## 2. Desktop Application Architecture (Electron)

The desktop application architecture is designed for security and robustness, clearly separating the native main process from the sandboxed renderer process.

### 2.1. `package.json`
This is the central configuration file for the Node.js project. It defines:
- **Metadata**: Application name, version, and entry point (`main`).
- **Dependencies**: `devDependencies` like `electron`, `electron-builder`, `esbuild`, and `tailwindcss`.
- **Scripts**:
    - `npm run build`: Compiles the application's CSS and JavaScript assets into the `dist/` directory.
    - `npm start`: Builds the assets and then runs the application in development mode using the local Electron dependency.
    - `npm run dist`: Packages the application into a distributable Windows installer using `electron-builder`.
- **Build Configuration**: The `build` section contains settings for `electron-builder`, specifying the application ID, target platform (`win`, `nsis`), and files to include in the final package (`index.html`, `dist/`, `electron/`).

### 2.2. Main Process (`electron/main.js`)
This is the backbone of the Electron application. It's a Node.js script that runs in the background and has full access to the operating system. Its primary responsibilities include:
- **Window Management**: Creating and managing the main `BrowserWindow` where the UI is rendered.
- **Application Lifecycle**: Handling events like `ready`, `window-all-closed`, and `activate`.
- **Native Integration**: Managing interactions with the file system, such as reading and writing the `settings.json` file.
- **Inter-Process Communication (IPC)**: Acting as the secure backend for the UI. It listens for messages from the renderer process (the React app) and performs privileged actions on its behalf.

### 2.3. Preload Script (`electron/preload.js`)
This script runs in a special, privileged context that has access to both the renderer's `window` object and a limited subset of Node.js APIs. Its sole purpose is to securely expose specific functionalities from the main process to the renderer process.
- It uses `contextBridge.exposeInMainWorld` to attach a custom API object (`window.electronAPI`) to the renderer's `window` object.
- This API provides safe, invokable functions (e.g., `getSettings`, `setSettings`) that the React application can call. These functions internally use `ipcRenderer.invoke` to send a message to the main process and receive a response.

### 2.4. Native Integration and IPC
The main process acts as a secure backend for the UI, handling tasks that require native OS access. This communication is managed via Inter-Process Communication (IPC).

#### Settings Management
A key feature of the desktop app is the `settings.json` file.
- **Location**: The settings file (`settings.json`) is stored in the standard user data directory for the application, managed by Electron via `app.getPath('userData')`. This separates user configuration from the application's installation files.
- **IPC Handlers (`main.js`)**: The main process defines handlers for IPC channels:
    - `get-settings`: Reads `settings.json` from the user data directory (or creates it with defaults if it doesn't exist) and returns its content.
    - `set-settings`: Receives a settings object from the renderer and writes it to `settings.json`. This is how user preferences are persisted.
- **Renderer Communication (`SettingsContext.tsx`)**: The settings-related components call the functions exposed on `window.electronAPI` (e.g., `window.electronAPI.getSettings()`) to interact with the settings file without ever having direct access to the file system.

#### Automatic File Logging
The application maintains a persistent log file for diagnostic purposes.
- **Log Rotation**: A new log file named `app-log-YYYY-MM-DD.log` is created daily in the application's user data directory.
- **IPC Handler (`main.js`)**: The main process listens on a one-way `log-message` IPC channel. When it receives a message, it formats it with a timestamp and level, and appends it to the current day's log file.
- **Renderer Communication (`ConsoleContext.tsx`)**: The central `logToConsole` function, provided by the `useConsole` hook, both updates the in-app console's state and calls `window.electronAPI.logMessage()`. This sends the log event to the main process to be written to disk.

#### Session Management
A core feature of the desktop application is persistent session management. This allows the user's work to be saved automatically and resumed later.

- **Storage Location**: Session files (which are standard `.sqlite` database files) are stored in a dedicated `sessions` directory within the application's user data directory (`app.getPath('userData')`). This follows standard application design and separates user data from the application's installation files.
- **IPC Handlers (`main.js`)**: The main process exposes a suite of handlers for session management:
    - `list-sessions`: Reads the contents of the `sessions` directory, gets file stats for each `.sqlite` file, and returns a sorted list of `SessionFile` objects.
    - `get-session-buffer`: Reads a specified session file from disk and returns its contents as a `Buffer`.
    - `save-session`: Receives a database buffer from the renderer. It generates a new timestamped filename (or uses a provided hint) and saves the buffer as a new `.sqlite` file in the `sessions` directory.
    - `delete-session`: Deletes a specified session file from the `sessions` directory.
    - `rename-session`: Renames a session file within the `sessions` directory.
- **Renderer Communication (`SessionContext.tsx`, `DataHub.tsx`)**:
    - The `SessionProvider` calls `window.electronAPI.listSessions()` to fetch the list of existing sessions.
    - The `DataHub.tsx` component consumes this list from `useSession()` and renders the UI for browsing and interacting with the sessions.
    - When the user adds new files via the `Dropzone`, the component orchestrates the processing via `useSession()`, which then calls `window.electronAPI.saveSession()` to persist the new database.
    - All actions (load, rename, delete) are initiated from the UI, call the relevant `window.electronAPI` function via the context, and then trigger a refresh of the session list to update the UI.

## 3. File Structure

- `package.json`: Project manifest and build configuration.
- `tailwind.config.js`, `postcss.config.js`: Configuration for the styling build process.
- `electron/`: Directory containing all Electron-specific code.
    - `main.js`: The main process script.
    - `preload.js`: The preload script for bridging the main and renderer processes.
- `electron.d.ts`: TypeScript type definitions for the `window.electronAPI` object.
- `index.html`: The main entry point for the renderer process.
- `index.tsx`: The script that bootstraps the React application.
- `styles.css`: The source file for Tailwind CSS styles.
- `dist/`: The output directory for all built assets (CSS and JS).
- `App.tsx`: The root component of the application, managing views and layout.
- `db.ts`: The `Database` class abstraction for `sql.js`.
- `types.ts`, `utils.ts`, `parsers.ts`: Shared types, utility functions, and the core log message parsing engine.
- `components/`: Directory containing all reusable React components.
- `contexts/`: Directory containing all React Context providers for state management.
- `*.md`: Documentation files.

## 4. State Management and Data Flow

The application's state management has been architected for scalability and maintainability, adhering to modern React principles of composition and separation of concerns. The core of this architecture is a system of **React Context Providers** and **custom hooks**.

### 4.1. Context Providers (`contexts/` directory)

Instead of a single, monolithic state object, the application's state is divided into logical domains, each managed by its own Context Provider. This ensures that components only re-render when the specific slice of state they care about changes. The providers are composed in `contexts/AppProvider.tsx`.

-   **`SessionContext`**: Manages the core database instance (`db`), session files (`.sqlite`), and all related logic, including creating, saving, loading, and processing sessions.
-   **`DataContext`**: Responsible for all data fetching and filtering logic. It handles querying the database for log entries and stock information based on the applied filters.
-   **`SettingsContext`**: Manages all user-configurable application settings (theme, view mode, column styles, etc.) and handles their persistence to the `settings.json` file.
-   **`UIContext`**: Controls global UI state, such as the active view, loading indicators, dialog visibility, and other transient UI flags.
-   **`ConsoleContext`**: Manages the state for the in-app "Application Log", including messages and filters.
-   **`ToastContext`**: Handles the state and lifecycle of toast notifications.

### 4.2. Custom Hooks

Components access state and dispatch actions by using custom hooks, which are simple wrappers around `useContext`. This provides a clean, descriptive API for interacting with the application's state.

-   `useSession()`: Provides access to session data and functions like `loadSession`, `saveSession`, etc.
-   `useData()`: Exposes filtered data (`filteredEntries`, `stockHistory`) and data-related actions like `handleApplyFilters`.
-   `useSettings()`: Allows components to read and update user settings.
-   `useUI()`: Provides access to UI state like `isLoading`, `activeView`, etc.

### 4.3. Data Flow Example: Applying a Filter

1.  A user interacts with a filter control in the `FilterBar.tsx` component.
2.  The component calls a state setter function obtained from `useData()`, updating the local `formFilters` state within `DataContext`.
3.  The user clicks the "Apply" button, which calls `handleApplyFilters()` from `useData()`.
4.  `handleApplyFilters()` updates the `appliedFilters` state within `DataContext`.
5.  A `useEffect` hook inside `DataProvider` detects the change in `appliedFilters`. It then uses the `db` instance from `SessionContext` to execute new queries against the database.
6.  The results of the queries (e.g., a new set of `filteredEntries`) are stored in `DataContext`'s state.
7.  Components like `LogTable.tsx`, which also use `useData()`, receive the new `filteredEntries` and re-render to display the updated data.

This decoupled, context-based flow ensures that concerns are neatly separated: `SettingsContext` knows nothing about data filtering, and `DataContext` knows nothing about session file management. The root `App.tsx` component is now a clean container responsible only for layout and composing the main views, passing down the necessary functions and state obtained from the custom hooks.