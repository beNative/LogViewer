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
    - **Bulk Selection**: After searching, you can use the **"Select All"** and **"Dese