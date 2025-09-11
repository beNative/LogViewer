# Log Analyser (Desktop Edition)

A sophisticated, client-side log analysis tool that allows users to drag-and-drop XML or ZIP log files, parses them, and displays them in a powerful, filterable, and searchable interface. It runs as a standalone desktop application using Electron, installed via a standard setup wizard that allows for customizing the installation directory.

## Key Features

- **Standalone Desktop App**: Packaged with Electron for a native desktop experience on Windows, complete with a standard setup wizard that allows for customizing the installation directory.
- **Professional Automatic Updates**: The app seamlessly checks for updates and notifies you with non-intrusive toast messages for downloading progress and installation readiness.
- **Prerelease Channel**: Opt-in to receive beta and pre-release versions via a new toggle in the Settings tab.
- **Session Management (Desktop Only)**:
    - **Automatic Saving**: Sessions are automatically saved as `.sqlite` files in a dedicated folder within your user profile's AppData directory.
    - **Load, Rename, Delete**: The Data Hub provides a full UI to manage previous sessions, allowing you to resume your work effortlessly.
- **Light and Dark Themes**: A polished UI with a theme toggle in the Settings tab.
- **Polished User Experience**: Features a color-coded header for intuitive navigation, a global status bar for at-a-glance information, and multiple icon sets for customization.
- **Drag-and-Drop Interface**: Easily create new sessions by dropping log files (`.xml` or `.zip` archives).
- **In-Memory Database**: Utilizes `sql.js` to store log data in an in-memory SQLite database, enabling fast and complex queries without a server.
- **Tabbed Navigation**:
    - **Data Hub**: A central area to manage sessions, create new sessions from files, and import/export external databases.
    - **Log Viewer**: The main analysis view featuring a filter panel, a dedicated toolbar for display options, the main log table, and a resizable detail panel. A new global status bar provides persistent data and view controls.
    - **Dashboard**: A visualization hub with interactive charts that provide a high-level overview of the *currently filtered* data.
    - **Application Log**: See real-time feedback, processing status, and errors from the application in a clean, full-screen interface with search and filtering capabilities.
    - **Info**: View in-app documentation, manuals, and version logs.
    - **Settings**: Manage application settings, including the UI theme.
- **Dynamic Data Visualization Dashboard**:
    - The dashboard is **fully synchronized** with the filters set in the Log Viewer and updates when filters are applied.
    - **Interactive Timeline**: A bar chart showing log volume over time. Click and drag to select a time range, which instantly updates the filters in the viewer and applies them.
    - **Category Charts**: Donut charts showing the distribution of logs by `Level` and `Sender Type`. Click a slice to add a filter for that category and apply it.
- **Advanced Filtering**:
    - Filter by date/time range.
    - Multi-select dropdowns for log attributes like `level`, `sender type`, `sender name`, and `filename`, with dedicated inputs for both **inclusion** and **exclusion**.
    - Advanced message content filtering with multi-line inputs for "include" and "exclude" terms.
    - **AND/OR Logic**: Toggle between matching all (AND) or any (OR) search terms for precise filtering.
    - **Advanced SQL Queries**: Bypass the UI with a full SQL editor to write custom queries directly against the log database. These complex queries can now be saved as presets.
- **Advanced Settings Management**: Directly edit the raw `settings.json` configuration in a syntax-highlighted editor, with options to import and export settings files.
- **Configuration File**: Application settings are stored in a `settings.json` file within the application's user data directory (e.g., `%APPDATA%\log-analyser\settings.json` on Windows). This file can be viewed and edited directly within the application via the 'JSON Source' tab in Settings, which also provides import/export capabilities.

## Technology Stack

- **Application Shell**: Electron
- **Automatic Updates**: electron-updater
- **Frontend**: React, TypeScript
- **Bundler**: esbuild
- **Styling**: Tailwind CSS with a PostCSS build step
- **Database**: sql.js (SQLite compiled for the browser)
- **Charting**: Chart.js
- **File Handling**: JSZip (for decompressing `.zip` files)

## Getting Started (Development)

To run the application in a development environment:

1.  **Install Dependencies**: Make sure you have Node.js and npm installed. Then run:
    ```bash
    npm install
    ```
2.  **Run the App**:
    - **For the Desktop App**:
      ```bash
      npm start
      ```
      This will build the necessary assets and start the Electron application.
    - **For the Web Browser**:
      ```bash
      npm run serve
      ```
      This will start a development server at `http://localhost:3000` with live reloading.

## Building and Releasing

To package the application and publish a new release to GitHub:

1.  **Set GitHub Token**: Before you can publish, you must set an environment variable named `GH_TOKEN` with a personal access token for your GitHub account. This token needs the `repo` scope to create releases.
    ```bash
    # On Windows (Command Prompt)
    set GH_TOKEN=your_token_here

    # On Windows (PowerShell)
    $env:GH_TOKEN="your_token_here"
    ```
2.  **Run the Publish Script**:
    ```bash
    npm run publish
    ```
    This script will build, package, and upload the release artifacts (e.g., the `.exe` installer) to a new draft release on your GitHub repository.
3.  **Finalize Release**: Go to the "Releases" page of your GitHub repository, edit the newly created draft, add release notes, and publish it. `electron-updater` will then automatically detect this new release for users.