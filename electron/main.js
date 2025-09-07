const { app, BrowserWindow, ipcMain, shell, dialog, systemPreferences } = require('electron');
const path = require('path');
const fs = require('fs');
const { exec } = require('child_process');
const { autoUpdater } = require('electron-updater');

let mainWindow;

// --- Path and Log Setup ---
// User-specific writable data path (e.g., AppData on Windows)
const appDataPath = app.getPath('userData');

const settingsPath = path.join(appDataPath, 'settings.json');
const sessionsPath = path.join(appDataPath, 'sessions');

// --- Log file setup ---
function getLogfileName() {
    const now = new Date();
    const year = now.getFullYear();
    const month = (now.getMonth() + 1).toString().padStart(2, '0');
    const day = now.getDate().toString().padStart(2, '0');
    return `app-log-${year}-${month}-${day}.log`;
}

const logFilePath = path.join(appDataPath, getLogfileName());

function log(level, message) {
    const timestamp = new Date().toISOString();
    const logEntry = `[${timestamp}] [${level.toUpperCase()}] ${message}`;
    console.log(logEntry);
    try {
        fs.appendFileSync(logFilePath, logEntry + '\n');
    } catch (error) {
        console.error(`!!! CRITICAL: Failed to write to log file at ${logFilePath}:`, error);
    }
}

log('INFO', '--- Application Main Process Starting ---');
log('INFO', `Is Packaged: ${app.isPackaged}`);
log('INFO', `User Data Path: ${app.getPath('userData')}`);
log('INFO', `Sessions Path: ${sessionsPath}`);
log('INFO', `Settings File Path: ${settingsPath}`);

// --- File System Setup ---
try {
    if (!fs.existsSync(sessionsPath)) {
        fs.mkdirSync(sessionsPath, { recursive: true });
        log('INFO', `Created sessions directory at ${sessionsPath}`);
    }
} catch (error) {
    log('ERROR', `Could not create sessions directory: ${error.message}`);
    dialog.showErrorBox('Fatal Error', `Could not create session directory at ${sessionsPath}. The application cannot save data.`);
}


// --- Settings Management ---
function getSettings() {
    const MONO_FONT_STACK = 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';
    const defaultColumnStyles = {
      time: { font: MONO_FONT_STACK, isBold: false, isItalic: false, fontSize: 13, color: '#6B7280', darkColor: '#9CA3AF' },
      level: { font: 'sans-serif', isBold: true, isItalic: false, fontSize: 12, color: '', darkColor: '' },
      sndrtype: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 14, color: '#374151', darkColor: '#D1D5DB' },
      sndrname: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 14, color: '#374151', darkColor: '#D1D5DB' },
      fileName: { font: 'sans-serif', isBold: false, isItalic: false, fontSize: 13, color: '#6B7280', darkColor: '#9CA3AF' },
      msg: { font: MONO_FONT_STACK, isBold: false, isItalic: false, fontSize: 13, color: '#1F2937', darkColor: '#F3F4F6' },
    };

    const defaultSettings = {
        theme: "light",
        viewMode: "pagination",
        allowPrerelease: false,
        columnVisibility: {
            time: true,
            level: true,
            sndrtype: true,
            sndrname: true,
            fileName: true,
            msg: true
        },
        customFilterPresets: {},
        columnStyles: defaultColumnStyles,
        panelWidths: {
            filters: 320,
            details: 500,
        },
        isTimeRangeSelectorVisible: true,
    };
    try {
        if (fs.existsSync(settingsPath)) {
            const settingsData = fs.readFileSync(settingsPath, 'utf-8');
            const loadedSettings = JSON.parse(settingsData);
            
            const mergedColumnStyles = { ...defaultColumnStyles };
            if (loadedSettings.columnStyles) {
                for (const key in mergedColumnStyles) {
                    if (loadedSettings.columnStyles[key]) {
                        // Migration from fontFamily to font for backward compatibility
                        if (loadedSettings.columnStyles[key].fontFamily && !loadedSettings.columnStyles[key].font) {
                            loadedSettings.columnStyles[key].font = loadedSettings.columnStyles[key].fontFamily;
                            delete loadedSettings.columnStyles[key].fontFamily;
                        }
                        mergedColumnStyles[key] = { ...mergedColumnStyles[key], ...loadedSettings.columnStyles[key] };
                    }
                }
            }

            const finalSettings = {
                ...defaultSettings,
                ...loadedSettings,
                allowPrerelease: loadedSettings.allowPrerelease ?? defaultSettings.allowPrerelease,
                columnVisibility: {
                    ...defaultSettings.columnVisibility,
                    ...(loadedSettings.columnVisibility || {}),
                },
                customFilterPresets: {
                    ...defaultSettings.customFilterPresets,
                    ...(loadedSettings.customFilterPresets || {}),
                },
                columnStyles: mergedColumnStyles,
                panelWidths: {
                    ...defaultSettings.panelWidths,
                    ...(loadedSettings.panelWidths || {}),
                },
                isTimeRangeSelectorVisible: loadedSettings.isTimeRangeSelectorVisible ?? defaultSettings.isTimeRangeSelectorVisible,
            };
            
            // Clean up deprecated keys from old versions
            delete finalSettings.apiKey;
            
            return finalSettings;
        }
    } catch (error) {
        log('ERROR', `Could not read or parse settings.json: ${error.message}. Using defaults.`);
    }
    
    // For any error or if file doesn't exist, write a fresh default file
    try {
        fs.writeFileSync(settingsPath, JSON.stringify(defaultSettings, null, 2));
        return defaultSettings;
    } catch (writeError) {
         log('ERROR', `Could not create default settings.json: ${writeError.message}`);
         return defaultSettings; // Return default in memory if write fails
    }
}


// --- Main Window Creation ---
function createWindow() {
  log('INFO', 'createWindow() called.');
  
  const windowOptions = {
    width: 1280,
    height: 800,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
    },
    autoHideMenuBar: true,
  };
  mainWindow = new BrowserWindow(windowOptions);

  mainWindow.once('ready-to-show', () => {
    log('INFO', 'Main window is ready to show. Checking for updates.');
    autoUpdater.checkForUpdates();
  });

  const indexPath = path.join(__dirname, '..', 'index.html');
  mainWindow.loadFile(indexPath);

  let forceQuit = false;
  app.on('before-quit', () => forceQuit = true);

  mainWindow.on('close', async (e) => {
    if (forceQuit) {
      return;
    }
    e.preventDefault();

    try {
      const dirtyState = await mainWindow.webContents.executeJavaScript('window.isAppDirty && window.isAppDirty()', true);
      
      if (!dirtyState || !dirtyState.isDirty) {
        forceQuit = true;
        app.quit();
        return;
      }
      
      const { response } = await dialog.showMessageBox(mainWindow, {
        type: 'question',
        buttons: ['Save', 'Don\'t Save', 'Cancel'],
        defaultId: 0,
        cancelId: 2,
        title: 'Unsaved Changes',
        message: `Do you want to save the changes you made to "${dirtyState.sessionName || 'Unsaved Session'}"?`,
        detail: 'Your changes will be lost if you don\'t save them.'
      });

      if (response === 2) { // Cancel
        return; 
      }
      if (response === 1) { // Don't Save
        forceQuit = true;
        app.quit();
        return;
      }
      if (response === 0) { // Save
        mainWindow.webContents.send('save-before-quit');
      }

    } catch (err) {
      log('ERROR', `Error during close confirmation: ${err.message}`);
      // If something went wrong, just quit to avoid an unresponsive app
      forceQuit = true;
      app.quit();
    }
  });

  if (!app.isPackaged) {
    mainWindow.webContents.openDevTools();
  }
}

// --- IPC Handlers for Session Management ---

ipcMain.handle('list-sessions', () => {
    try {
        const files = fs.readdirSync(sessionsPath).filter(file => file.endsWith('.sqlite'));
        return files.map(file => {
            const filePath = path.join(sessionsPath, file);
            const stats = fs.statSync(filePath);
            return {
                name: file,
                path: filePath,
                size: stats.size,
                mtime: stats.mtime.getTime(),
            };
        }).sort((a, b) => b.mtime - a.mtime); // Sort by most recently modified
    } catch (error) {
        log('ERROR', `IPC: Failed to list sessions: ${error.message}`);
        return [];
    }
});

ipcMain.handle('get-session-buffer', (event, fileName) => {
    try {
        const filePath = path.join(sessionsPath, fileName);
        if (!fs.existsSync(filePath)) {
            throw new Error(`Session file not found: ${fileName}`);
        }
        const buffer = fs.readFileSync(filePath);
        return { success: true, buffer };
    } catch (error) {
        log('ERROR', `IPC: Failed to get session buffer for ${fileName}: ${error.message}`);
        return { success: false, error: error.message };
    }
});

ipcMain.handle('save-session', (event, buffer, fileName) => {
    try {
        if (!fileName) {
            const now = new Date();
            const timestamp = now.toISOString().slice(0, 19).replace(/:/g, '-').replace('T', '_');
            fileName = `session_${timestamp}.sqlite`;
        }
        
        const filePath = path.join(sessionsPath, fileName);
        fs.writeFileSync(filePath, Buffer.from(buffer));
        return { success: true };
    } catch (error) {
        log('ERROR', `IPC: Failed to save session: ${error.message}`);
        return { success: false, error: error.message };
    }
});

ipcMain.handle('delete-session', (event, fileName) => {
    try {
        const filePath = path.join(sessionsPath, fileName);
        if (fs.existsSync(filePath)) {
            fs.unlinkSync(filePath);
            return { success: true };
        }
        throw new Error(`File not found for deletion: ${fileName}`);
    } catch (error) {
        log('ERROR', `IPC: Failed to delete session ${fileName}: ${error.message}`);
        return { success: false, error: error.message };
    }
});

ipcMain.handle('rename-session', (event, oldName, newName) => {
    try {
        const oldPath = path.join(sessionsPath, oldName);
        const newPath = path.join(sessionsPath, newName);

        if (!fs.existsSync(oldPath)) throw new Error(`Source file not found: ${oldName}`);
        if (fs.existsSync(newPath)) throw new Error(`Destination file already exists: ${newName}`);

        fs.renameSync(oldPath, newPath);
        return { success: true };
    } catch (error) {
        log('ERROR', `IPC: Failed to rename session ${oldName} to ${newName}: ${error.message}`);
        return { success: false, error: error.message };
    }
});

ipcMain.handle('get-markdown-content', (event, fileName) => {
    try {
        const appPath = app.getAppPath();
        // In development, appPath is the project root.
        // In production, appPath is `resources/app.asar`, and extraFiles are in `resources`.
        // So we need to go up one level from the asar archive to find the MD files.
        const filePath = app.isPackaged
            ? path.join(appPath, '..', fileName)
            : path.join(appPath, fileName);

        if (fs.existsSync(filePath)) {
            const content = fs.readFileSync(filePath, 'utf-8');
            return { success: true, content };
        } else {
            log('WARNING', `IPC: Markdown file not found at ${filePath}`);
            throw new Error(`File not found: ${fileName}`);
        }
    } catch (error) {
        log('ERROR', `IPC: Failed to get markdown content for ${fileName}: ${error.message}`);
        return { success: false, error: error.message };
    }
});


// --- Application Lifecycle Events ---
app.whenReady().then(() => {
    log('INFO', "Electron 'ready' event fired. Setting up IPC handlers and creating window.");

    // Configure auto-updater
    const settings = getSettings();
    autoUpdater.allowPrerelease = !!settings.allowPrerelease;
    autoUpdater.logger = {
        info: (msg) => log('INFO', `[Updater] ${typeof msg === 'object' ? JSON.stringify(msg) : msg}`),
        warn: (msg) => log('WARNING', `[Updater] ${typeof msg === 'object' ? JSON.stringify(msg) : msg}`),
        error: (err) => log('ERROR', `[Updater] ${err}`),
        debug: (msg) => log('DEBUG', `[Updater] ${typeof msg === 'object' ? JSON.stringify(msg) : msg}`),
    };

    // Standard IPC Handlers
    ipcMain.handle('get-settings', () => getSettings());
    ipcMain.handle('set-settings', (event, settings) => {
        try {
            fs.writeFileSync(settingsPath, JSON.stringify(settings, null, 2));
            // After saving settings, check if the prerelease flag changed and apply it.
            if (settings.hasOwnProperty('allowPrerelease')) {
                autoUpdater.allowPrerelease = !!settings.allowPrerelease;
                log('INFO', `[Updater] allowPrerelease setting updated to: ${autoUpdater.allowPrerelease}`);
            }
            return { success: true };
        } catch (error) {
            return { success: false, error: error.message };
        }
    });
    ipcMain.handle('get-settings-path', () => settingsPath);
    ipcMain.handle('show-settings-file', () => shell.showItemInFolder(settingsPath));
    ipcMain.on('log-message', (event, { level, message }) => log(level, `[Renderer] ${message}`));
    
    // Custom IPC Handlers
    ipcMain.handle('get-system-fonts', () => {
        return new Promise((resolve, reject) => {
            if (process.platform === 'win32') {
                const command = 'powershell -command "[System.Drawing.Text.InstalledFontCollection, System.Drawing, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a]::new().Families.Name"';
                
                exec(command, (error, stdout, stderr) => {
                    if (error) {
                        log('ERROR', `Failed to get system fonts via PowerShell: ${stderr}. Falling back to Electron API.`);
                        // Fallback to the original method
                        try {
                            const fontList = systemPreferences.getFontFamilyList();
                            if (fontList.length > 0 && typeof fontList[0] === 'object' && fontList[0].hasOwnProperty('name')) {
                                resolve(fontList.map(font => font.name));
                            } else {
                                resolve(fontList);
                            }
                        } catch(e) {
                             log('ERROR', `Electron API for fonts also failed: ${e.message}`);
                             reject(e);
                        }
                        return;
                    }
                    
                    const fonts = stdout.split('\r\n').filter(f => f.trim() !== '');
                    log('INFO', `Successfully fetched ${fonts.length} fonts via PowerShell.`);
                    resolve(fonts);
                });
            } else {
                // For macOS, Linux, etc., the original method is generally fine.
                try {
                    log('INFO', 'Fetching fonts using Electron built-in API for non-Windows OS.');
                    const fontList = systemPreferences.getFontFamilyList();
                    if (fontList.length > 0 && typeof fontList[0] === 'object' && fontList[0].hasOwnProperty('name')) {
                        resolve(fontList.map(font => font.name));
                    } else {
                        resolve(fontList);
                    }
                } catch (e) {
                     log('ERROR', `Failed to get system fonts via Electron API: ${e.message}`);
                     reject(e);
                }
            }
        });
    });

    // Quit sequence handler
    ipcMain.on('saved-and-ready-to-quit', () => {
      // This is called by the renderer after a successful save-on-quit
      const allWindows = BrowserWindow.getAllWindows();
      if (allWindows.length > 0) {
        const win = allWindows[0];
        // The 'close' event handler has a flag `forceQuit` which is now true
        // and will allow the app to exit.
        win.destroy(); // destroy instead of close to bypass listener again
      }
      app.quit();
    });

    ipcMain.handle('set-title', (event, title) => {
        if(mainWindow) {
            mainWindow.setTitle(title);
        }
    });

    createWindow();

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0) createWindow();
    });
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

app.on('will-quit', () => {
    log('INFO', '--- Application will quit ---');
});


// --- Auto-Updater Event Handlers ---

autoUpdater.on('checking-for-update', () => {
    log('INFO', '[Updater] Checking for update...');
});

autoUpdater.on('update-available', (info) => {
    log('INFO', `[Updater] Update available: ${info.version}`);
});

autoUpdater.on('update-not-available', (info) => {
    log('INFO', `[Updater] Update not available. Current version: ${info.version}`);
});

autoUpdater.on('error', (err) => {
    log('ERROR', `[Updater] Error in auto-updater. ${err}`);
});

autoUpdater.on('download-progress', (progressObj) => {
    let log_message = "Download speed: " + progressObj.bytesPerSecond;
    log_message = log_message + ' - Downloaded ' + progressObj.percent + '%';
    log_message = log_message + ' (' + progressObj.transferred + "/" + progressObj.total + ')';
    log('INFO', `[Updater] ${log_message}`);
});

autoUpdater.on('update-downloaded', (info) => {
    log('INFO', '[Updater] Update downloaded; will prompt user to restart.');
    dialog.showMessageBox({
        type: 'info',
        title: 'Update Ready to Install',
        message: 'A new version has been downloaded. Restart the application to apply the updates.',
        detail: `Version ${info.version} is now available.`,
        buttons: ['Restart Now', 'Later'],
        defaultId: 0,
        cancelId: 1
    }).then(({ response }) => {
        if (response === 0) {
            setImmediate(() => autoUpdater.quitAndInstall());
        }
    });
});