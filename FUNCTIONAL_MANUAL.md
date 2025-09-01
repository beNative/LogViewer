# Log Analyser - Functional Manual

This document provides a comprehensive guide on how to use the Log Analyser desktop application.

## 1. Introduction

The Log Analyser is a desktop tool designed to help you analyze structured XML log files. You can load data, perform complex filtering, and inspect individual log entries without needing to install any software beyond the application itself.

## 2. Main Interface

The application is organized into five main tabs, which you can switch between using the header navigation. The recommended workflow is now session-centric:

- **Data Hub**: Your starting point. Here you will load an existing session or create a new one.
- **Log Viewer**: For analyzing and filtering your log entries within the active session.
- **Dashboard**: For a high-level visual overview of your filtered data within the active session.
- **Application Log**: For viewing application status messages and errors.
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

Once you have loaded data, switch to this tab to analyze it. The view is divided into a filter panel (left) and a content area (right).

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
    - Use the multi-select dropdowns to filter by specific values for `Level`, `Sender Type`, `Sender Name`, and `Filename`. You can select multiple values in each dropdown.
    - **Quick Search**: For dropdowns with many options (like Sender Name), you can click to open the dropdown and then use the search bar at the top to quickly find the item you're looking for.
    - **Bulk Selection**: After searching, you can use the **"Select All"** and **"Deselect All"** buttons to check or uncheck all the items that are currently showing in the list.

- **Message Content**:
    - **Message contains**: In this text area, enter terms you want to find in the log message. **Enter one term per line.**
    - **Message does NOT contain**: In this text area, enter terms you want to exclude. **Enter one term per line.**
    - **AND/OR Toggle**: For both "contains" and "does not contain" filters, you can switch between:
        - `OR`: Matches logs containing *any* of the specified terms.
        - `AND`: Matches logs containing *all* of the specified terms.
        
- **Advanced SQL Query**:
  For maximum control, you can bypass the standard UI filters and write your own SQL queries.
  - **Enable**: Use the "Enable SQL Query Filter" toggle to activate this mode. When enabled, all other UI filters above it will be disabled.
  - **SQL Editor**: Write your query in the provided SQL editor with syntax highlighting. The editor has a visible caret for easy editing.
  - **Query Reference**: Below the editor, a helper box shows the table name (`logs`) and a list of available columns (`id, time, level`, etc.) for your convenience.
  - **Example**: Click "Insert Example" to populate the editor with a predefined query to get you started.
  - **Saving as a Preset**: SQL queries can be saved as presets just like any other filter combination. Simply enable the query, write your SQL, and use the "Save As..." button in the Presets section.

- **Action Buttons**:
    - **Apply**: Click this to execute the query with your selected filters. The Log Table and Dashboard will update to reflect these filters.
    - **Clear**: Click this to reset all filters to their default state and apply them immediately.

### 4.2. Log Table

This is the main area where the filtered log data is displayed.

- **Pagination**: Navigate through pages of results using the "Previous" and "Next" buttons at the bottom. The display shows the current page, total pages, and which rows are currently being shown.
- **Selecting an Entry**: Click anywhere on a row in the table to open the Log Detail Panel on the right, showing all data for that specific entry. The selected row will be highlighted.
- **Search Highlighting**: When you filter by "Message contains", any matching terms will be automatically highlighted in yellow within the "Message" column, making them easy to spot.
- **Column Visibility**: At the bottom-right of the log viewer, next to the pagination controls, you will find a new "Columns" button. Clicking this opens a menu that allows you to show or hide specific columns in the log table. Your preferences are saved automatically and will be restored the next time you open the application.

### 4.3. Log Detail Panel

When a log entry is selected, this panel appears on the right.

- **Information**: It displays all fields for the selected log entry in a clean, readable format.
- **Message Display**: The `msg` content is shown in a monospaced font inside a code block, making it easy to read structured or multi-line messages.
- **Search Highlighting**: If the selected log was found using a "Message contains" filter, the matching search terms will also be highlighted in the detailed message view.
- **Resizing**: You can resize the panel by clicking and dragging the splitter bar that appears between the table and the panel.
- **Closing**: Click the 'X' button at the top right of the panel to close it.

### 4.4. Debugging Tools
To help diagnose complex visual bugs (especially layout issues with very large tables), a debugging tool is available in the Log Viewer footer.

- **Debug Button**: In the footer, next to the "Columns" button, you will find a "Debug" button with a bug icon.
- **Log Layout Dimensions**: Clicking this button will reveal a menu. Select **"Log Layout Dimensions"**. This action will *not* change anything visually, but it will capture a detailed snapshot of the application's current layout state.
- **Viewing the Report**: After clicking, an alert will confirm that the report has been generated. Switch to the **Application Log** tab to view it. You will see a new message starting with `--- LAYOUT DEBUG START ---`. This report can be copied and shared to help developers troubleshoot the issue.
---

## 5. Dashboard

This tab provides a high-level, interactive overview of your log data.

### 5.1. A Dynamic, Synchronized View
The Dashboard is **synchronized** with the Log Viewer. The charts will always visualize the data that matches the **currently applied filters**. When you click "Apply" in the Log Viewer, both the table and the dashboard charts will update.

### 5.2. Log Volume Over Time

This large bar chart displays the number of log entries over time for the filtered data.
- **Interactive Filtering**: To investigate a specific period, **click and drag your mouse horizontally** across the chart. When you release the mouse, the application will automatically update the date/time filters in the Filter Panel, apply them, and switch you to the **Log Viewer** tab.

### 5.3. Distribution Charts

Two donut charts provide a quick breakdown of your filtered logs by category.
- **Distribution by Log Level**: Shows the percentage of logs for each severity level.
- **Distribution by Sender Type**: Shows the percentage of logs from each sender type.
- **Interactive Filtering**: **Click on any slice** of a donut chart. This will **add a new filter** for that category to the Filter Panel, apply it, and switch you to the **Log Viewer**. For example, if you are already filtering by a date range, clicking the "ERROR" slice will add `Level: ERROR` to your filter set and refresh the view.

---

## 6. Application Log

This tab provides a running log of the application's actions. It's useful for understanding what's happening during file processing or for debugging errors.

### 6.1. Filtering Messages

The log's header contains toggle buttons to filter messages by their type. Each button shows a count of how many messages of that type have been logged. Click a button to show or hide that category.

- **DEBUG (Green)**: Detailed technical information, such as successful database operations or file processing steps.
- **INFO (Blue)**: General informational messages about the application's status, like initialization or filter resets.
- **WARNING (Orange)**: Non-critical issues that the application handled, such as skipping an invalid file type or sanitizing file content.
- **ERROR (Red)**: Critical errors that prevented an operation from completing, like a file parsing failure or database import error.

### 6.2. Clearing the Log

- The **Clear** button at the top right will wipe all messages from the log.

---

## 7. Settings

This tab allows you to view and manage the application's configuration.

### 7.1. Appearance

- **Dark Mode Toggle**: Use this switch to toggle the application's appearance between the default dark theme and a new light theme. Your preference is automatically saved and will be applied the next time you launch the application.

### 7.2. The `settings.json` File

All application settings are stored in a file named `settings.json`.
- **Location**: This file is automatically created in the same directory where the main application executable (`Log Analyser.exe`) is located.
- **Purpose**: It allows for persistent configuration of the application. Your chosen theme and column visibility preferences are stored here.

### 7.3. Using the Settings Tab

- **View Current Settings**: The tab displays the current content of your `settings.json` file in a read-only format.
- **Locate and Edit the File**:
    1. Click the **"Show Settings File"** button.
    2. Your system's file explorer will open with the `settings.json` file highlighted.
    3. You can open this file in any text editor (like Notepad) to make changes.
- **Applying Changes**: After saving your changes to `settings.json`, you may need to restart the application for them to take full effect.

---

## 8. Automatic File Logging

For diagnostic purposes, the application automatically writes all log messages (the same ones you see in the **Application Log** tab) to a file on your computer.

- **Log File Location**: The log files are stored in the same directory as the application executable (`Log Analyser.exe`).
- **Log File Naming**: A new log file is created for each day. The files are named using the format `app-log-YYYY-MM-DD.log` (e.g., `app-log-2024-05-25.log`).
- **Content**: The file contains timestamped entries for all `DEBUG`, `INFO`, `WARNING`, and `ERROR` messages generated by the application during its runtime. This is particularly useful for troubleshooting issues that occur on startup or for keeping a persistent record of application activity.

---

## 9. Quitting the Application

The application ensures that you do not accidentally lose your work.

If you attempt to close the application with unsaved changes in your current session (indicated by an asterisk `*` next to the session name), you will be prompted with a dialog.

You have three choices:
- **Save**: This will automatically save your session and then close the application. If it's a brand new session that hasn't been saved before, it will be given a default, timestamped name (e.g., `session_2024-08-06_15-30-00.sqlite`). You will not be asked to provide a name.
- **Don't Save**: This will immediately close the application and discard any unsaved changes.
- **Cancel**: This will abort the closing process, allowing you to return to the application.