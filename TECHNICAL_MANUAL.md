# Log Analyser - Technical Manual

This document outlines the technical architecture and implementation details of the Log Analyser application.

## 1. Core Architecture

The Log Analyser is a **desktop application** built using **Electron**. This architecture encapsulates the web-based frontend into a native shell, allowing it to run as a standalone application on the user's computer. All processing, data storage, and rendering happen locally.

- **Application Shell**: **Electron** is used to create the native application wrapper and provides access to the underlying operating system via Node.js APIs.
- **Framework**: The UI is built using **React** with **TypeScript**. It leverages functional components and hooks for state management and side effects.
- **Bundler**: **esbuild** is used for fast and efficient bundling of the TypeScript and React code.
- **Data Storage**: Log data is stored in an in-memory **SQLite database** powered by **sql.js**, a library that compiles SQLite to WebAssembly. This allows for powerful and efficient SQL-based querying directly in the browser context of the Electron window.
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
- **Renderer Communication (`App.tsx`, `Settings.tsx`)**: The React components call the functions exposed on `window.electronAPI` (e.g., `window.electronAPI.getSettings()`) to interact with the settings file without ever having direct access to the file system.

#### Automatic File Logging
The application maintains a persistent log file for diagnostic purposes.
- **Log Rotation**: A new log file named `app-log-YYYY-MM-DD.log` is created daily in the application's user data directory.
- **IPC Handler (`main.js`)**: The main process listens on a one-way `log-message` IPC channel. When it receives a message, it formats it with a timestamp and level, and appends it to the current day's log file.
- **Renderer Communication (`App.tsx`)**: The central `logToConsole` function in the React app has been augmented. In addition to updating the in-app console's state, it also calls `window.electronAPI.logMessage()`. This sends the log event to the main process to be written to disk.

#### Session Management
A core feature of the desktop application is persistent session management. This allows the user's work to be saved automatically and resumed later.

- **Storage Location**: Session files (which are standard `.sqlite` database files) are stored in a dedicated `sessions` directory within the application's user data directory (`app.getPath('userData')`). This follows standard application design and separates user data from the application's installation files.
- **IPC Handlers (`main.js`)**: The main process exposes a suite of handlers for session management:
    - `list-sessions`: Reads the contents of the `sessions` directory, gets file stats for each `.sqlite` file, and returns a sorted list of `SessionFile` objects.
    - `get-session-buffer`: Reads a specified session file from disk and returns its contents as a `Buffer`.
    - `save-session`: Receives a database buffer from the renderer. It generates a new timestamped filename (or uses a provided hint) and saves the buffer as a new `.sqlite` file in the `sessions` directory.
    - `delete-session`: Deletes a specified session file from the `sessions` directory.
    - `rename-session`: Renames a session file within the `sessions` directory.
- **Renderer Communication (`App.tsx`, `DataHub.tsx`)**:
    - On startup, `App.tsx` calls `window.electronAPI.listSessions()` to fetch the list of existing sessions.
    - The `DataHub.tsx` component passes this list to the new `SessionManager.tsx` component, which renders the UI for browsing and interacting with the sessions.
    - When the user adds new files via the `Dropzone`, `App.tsx` orchestrates the processing and then calls `window.electronAPI.saveSession()` to persist the new database.
    - All actions (load, rename, delete) are initiated from the UI, call the relevant `window.electronAPI` function, and then trigger a refresh of the session list to update the UI.

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
- `App.tsx`: The root component of the application, managing state and views.
- `db.ts`: The `Database` class abstraction for `sql.js`.
- `types.ts`, `utils.ts`, `parsers.ts`: Shared types and helper functions.
- `components/`: Directory containing all reusable React components.
- `*.md`: Documentation files.

## 4. State Management and Data Flow
The state management and data flow within the React application remain largely unchanged from the web version, with the primary addition being the initial fetching of application settings from the main process via the preload script. The separation between `formFilters` and `appliedFilters` continues to be the core pattern for ensuring synchronized data fetching between the Log Viewer and the Dashboard.

The UI features a global **`StatusBar.tsx`** component, which is managed at the root `App.tsx` level to provide persistent application-wide feedback. The **`LogTable.tsx`** component has been refactored to delegate its display controls (e.g., density, column visibility) to a new dedicated toolbar, cleaning up its internal structure. The previous complex `VirtualizedLogView` has been removed, with the infinite scroll implementation now handled directly within `LogTable.tsx`, simplifying the rendering logic.

### Column Visibility
The visibility of columns in the Log Viewer is managed by a new state object, `columnVisibility`, within `App.tsx`. This state is initialized from and persisted to the `settings.json` file via IPC handlers in the main process. The state is passed as props to `LogTable.tsx`, which conditionally renders table headers (`<th>`) and cells (`<td>`) based on the current visibility settings. A new component, `ColumnSelector.tsx`, provides the UI for modifying this state.