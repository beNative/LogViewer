const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
    // Settings
    getSettings: () => ipcRenderer.invoke('get-settings'),
    setSettings: (settings) => ipcRenderer.invoke('set-settings', settings),
    getSettingsPath: () => ipcRenderer.invoke('get-settings-path'),
    showSettingsFile: () => ipcRenderer.invoke('show-settings-file'),
    
    // Logging
    logMessage: (level, message) => ipcRenderer.send('log-message', { level, message }),
    
    // Session Management
    listSessions: () => ipcRenderer.invoke('list-sessions'),
    getSessionBuffer: (name) => ipcRenderer.invoke('get-session-buffer', name),
    saveSession: (buffer, nameHint) => ipcRenderer.invoke('save-session', buffer, nameHint),
    deleteSession: (name) => ipcRenderer.invoke('delete-session', name),
    renameSession: (oldName, newName) => ipcRenderer.invoke('rename-session', oldName, newName),

    // Markdown content for Info tab
    getMarkdownContent: (fileName) => ipcRenderer.invoke('get-markdown-content', fileName),

    // Custom App functions
    getSystemFonts: () => ipcRenderer.invoke('get-system-fonts'),

    // Window & Quit lifecycle
    setTitle: (title) => ipcRenderer.invoke('set-title', title),
    onSaveBeforeQuit: (callback) => {
        const listener = () => callback();
        ipcRenderer.on('save-before-quit', listener);
        // Return a function to remove the listener
        return () => ipcRenderer.removeListener('save-before-quit', listener);
    },
    savedAndReadyToQuit: () => ipcRenderer.send('saved-and-ready-to-quit'),
});