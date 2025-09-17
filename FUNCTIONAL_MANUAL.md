# Log Analyser - Functional Manual

This document provides a comprehensive guide on how to use the Log Analyser desktop application.

## 1. Introduction

The Log Analyser is a desktop tool designed to help you analyze structured XML log files. You can load data, perform complex filtering, and inspect individual log entries without needing to install any software beyond the application itself.

The application will automatically check for updates and notify you via non-intrusive toast messages in the corner of the screen when a new version is available, being downloaded, or ready to install.

## 2. Main Interface

The application is organized into several main tabs, which you can switch between using the header navigation. The recommended workflow is now session-centric:

- **Data Hub**: Your starting point. Here you will load an existing session or create a new one.
- **Log Viewer**: For analyzing and filtering your generic log entries within the active session.
- **Stock Tracker**: For searching and visualizing stock-specific information found within the logs.
- **Dashboard**: For a high-level visual overview of your filtered data within the active session.
- **Application Log**: For viewing application status messages and errors.
- **Info**: For viewing in-app documentation, manuals, version logs, and the "About" dialog.
- **Settings**: For managing application configuration and appearance.

---

## 3. Data Hub

This is the default view and your starting point for all data-related tasks. The layout is optimized for the desktop application's session-based workflow.

### 3.1. Local Sessions

This is the main panel on the left. It lists all the analysis sessions you have saved previously. Each item in the list shows:
- The session name (e.g., `session_2024-08-06_10-30-00.sqlite`).
- The file size and the date it was last modified.

For each session, you have several actions:
- **Load (Reload Icon)**: Click this to load the session into the application. The Log Viewer and Dashboard will now reflect this session's data. The active session is highlighted in the list.
- **Rename (Pencil Icon)**: Click to edit the session's filename. This is useful for giving sessions descriptive names (e.g., `Server-A-Crash-Logs.sqlite`).
- **Delete (Trash Icon)**: Click to permanently delete the session file. A confirmation prompt will appear before deletion.

At the top of this panel, you can click **"New Blank"** to start a completely empty session, clearing any loaded data.

### 3.2. Create New Session from Files

On the right side of the screen, you will find the area to create new sessions from your log files.
- **Drag and Drop**: Drag one or more `.xml` or `.zip` files from your computer and drop them onto the large dropzone area.
- **Click to Select**: Alternatively, click anywhere inside the dropzone to open a file selection dialog.

When you add files, the application will automatically:
1. Process the files and load them into a new database.
2. **Save this new database as a new session file** with a timestamped name.
3. Refresh the "Local Sessions" list, where your new session will appear at the top.
4. Automatically load this new session, making it the active one.

### 3.3. Other Actions

This section provides tools for moving data between this application and other systems.
- **Import External DB...**: Click this button to load a `.sqlite` database file that did *not* originate from this application's session manager (e.g., one you received from a colleague). When imported, it will be saved as a new session.
- **Export Active Session...**: Click this to save a copy of the currently loaded session to a location of your choice (e.g., your Desktop or Downloads folder). This is useful for sharing your analysis with others.

---

## 4. Log Viewer

Once you have loaded data, this is the primary tab for analysis. The view is divided into three main areas: the **Filter Panel** on the left, the **Main Content Area** in the center, and the **Details Panel** on the right (when visible). The gray bars separating the panels are draggable splitters, allowing you to resize the layout to your preference.

### 4.1. Filter Panel

This panel on the left side of the screen contains all the controls to filter your data. **Data in the Log Viewer and Dashboard will only update after you click the "Apply" button.**

- **Presets**:
    At the top of the Filter Panel is the Presets section. This allows you to save and load complex filter configurations.
    - **Saving Presets**: Once you have configured your filters (including Date Range, Attributes, Message Content, or even an Advanced SQL Query), you can save this configuration for later use. Click the "Save As..." button, provide a name in the inline form, and save.
    - **Loading Presets**: Select a saved preset from the dropdown menu to instantly load its configuration into the filter panel. You must still click "Apply" to see the results.
    - **Deleting Presets**: Select a preset from the dropdown and click the "Delete Preset" button to remove it.

- **Date Range**:
    - **From/To**: Select start and end dates and times. The query will include logs within this range.

- **Log Attributes**:
    - Use the multi-select dropdowns to filter by specific values for `Level`, `Sender Type`, `Sender Name`, and `Filename`. For each attribute, you can use the **Include/Exclude** toggle switch next to the dropdown to control whether the selected items should be part of the result set or removed from it.
    - **Quick Search**: For dropdowns with many options (like Sender Name), you can click to open the dropdown and then use the search bar at the top to quickly find the item you're looking for.

### 4.2. Main Content Area

This is where you view and interact with your log data.

#### The Log Table
This table displays the log entries that match your currently applied filters. You can:
- **Click** a row to select it and view its full contents in the Details Panel.
- **Use Keyboard Navigation** (Arrow keys, PageUp/Down, Home/End) to move through the entries.
- **Right-click on a cell** to open a context menu for quick filtering (for both inclusion and exclusion) or copying data.
- **Right-click on the table header** to open a context menu for showing or hiding columns.

### 4.3. The Details Panel
When toggled on, this panel appears on the right and shows the full, parsed details of the currently selected log entry. It provides a structured view of the data and can render special formats like XML, SQL, and Key-Value pairs.

---

## 5. Stock Tracker

This tab is a specialized tool for analyzing stock level information that is embedded within the log messages. When log files are first processed, the application automatically extracts any `StockInfoMessage` data and stores it in a separate, structured table for efficient querying.

### 5.1. Searching
- **Article Search**: Enter the name or ID of an article (minimum 2 characters). As you type, a dropdown will appear with suggestions from the database. You can click a suggestion or continue typing.
- **Date Range**: Use the date and time inputs, or the graphical timeline selector, to narrow your search to a specific period.
- **Search Button**: Click "Search" to execute the query. The results will appear below.

### 5.2. Results
After a search, two panels will display the results:
- **Stock History Table**: A chronological list of all stock level changes for the searched article within the selected time frame. It shows the exact timestamp, article name, and the reported quantity at that time.
- **Quantity Over Time Chart**: A line graph that visually plots the quantity from the history table over time, making it easy to spot trends, drops, or replenishments.

### 5.3. Rebuild Stock Data
- **Purpose**: This button is an advanced utility. If you suspect the stock data is incomplete or corrupted, or if the parsing logic has been updated in a new version of the application, you can use this feature.
- **Action**: It will delete all existing stock data for the current session and re-scan every log entry to extract it again. This can be a slow process and the session is saved automatically upon completion.

---

## 6. The Status Bar

A persistent status bar is located at the very bottom of the application window. It provides at-a-glance information and quick controls.

- **Left Side**: Displays key data metrics:
    - **Total Entries**: The total number of logs in the entire session database.
    - **Filtered Entries**: The number of logs matching the current filters.
    - **Session Name**: The name of the active session file. An asterisk (`*`) indicates unsaved changes.

- **Center**: Shows context-aware view information. In **Paginate Mode**, it displays full pagination controls. In **Scroll Mode**, it shows the row count. This area also contains controls for adjusting the table row **Density** (Compact, Normal, Comfortable) and toggling the **Details Panel** visibility.

- **Right Side**: Provides real-time application feedback:
    - **Status Message**: Shows the most recent message from the "Application Log", giving you live feedback on what the app is doing (e.g., "Applying filters...").
    - **Theme Toggle**: A sun/moon icon to quickly switch between light and dark themes.

---

## 7. Dashboard

This tab provides a high-level visual overview of your **currently filtered** log data.

### 7.1. Timeline Chart
This bar chart shows the volume of log entries over time. You can **click and drag** horizontally across the chart to select a specific time range. Doing so will immediately apply that range as a filter and switch you back to the Log Viewer to see the results.

### 7.2. Category Charts
These donut charts show the distribution of logs by **Level** and **Sender Type**. You can **click on a slice** of a chart to instantly add a filter for that category and see the results in the Log Viewer.

---

## 8. Application Log

This tab displays a real-time feed of messages from the application itself, presented in a clean, full-screen view for maximum readability. It's useful for:
- Seeing the progress of file processing.
- Diagnosing errors if something goes wrong.
- Understanding what the application is doing in the background.

You can filter the messages by type (DEBUG, INFO, WARNING, ERROR) using the toggle buttons at the top, and search for specific text using the filter input.

---

## 9. Settings

This tab allows you to configure various aspects of the application and is divided into two main views: **Controls** and **JSON Source**.

### 9.1. Controls View
This is the default view and provides user-friendly controls for common settings, organized by category.

- **Appearance**: Toggle between Light and Dark themes, change the active icon set, adjust the global UI Scale, and adjust the density of rows in the log table.
- **Behavior**: Control the default log viewer mode (Paginate/Scroll), toggle the default visibility of the main Timeline, and individually toggle the visibility of each contextual bar within the timeline (Pages, Files, Dates, Density, and the Overview).
- **Updates**: Opt-in to receive pre-release (beta) versions of the application. Enabling this requires an application restart to take effect.
- **Table Styles**: Customize the font, font size, style (bold/italic), and color for each column in the Log Viewer.
- **Debugging**: Contains options for developers, such as an inspector for UI focus and hover states.

### 9.2. JSON Source View
For advanced users, this tab provides direct access to the `settings.json` configuration file.
- **Editable JSON**: The main part of this view is a syntax-highlighted editor displaying the raw JSON content of your settings file. You can make direct changes here.
- **Save**: After editing the JSON, click this button to apply and save your changes. The application will validate the JSON before saving.
- **Discard**: If you've made changes you don't want to keep, click this to revert the editor's content back to the last saved version.
- **Import**: Click to open a file dialog and select a `.json` settings file to load into the editor. This is useful for restoring a backup or sharing configurations.
- **Export**: Click to download the current settings in the editor as a `.json` file.
- **Show Settings File**: This button will open your operating system's file explorer with the `settings.json` file selected, showing you where it's stored on your computer.