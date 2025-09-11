// Fix: Changed import from "./types" to "./types.ts"
import { SessionFile, Settings } from "./types.ts";

export interface IElectronAPI {
    // Settings
    getSettings: () => Promise<Settings>;
    setSettings: (settings: any) => Promise<{ success: boolean; error?: string }>;
    getSettingsPath: () => Promise<string>;
    showSettingsFile: () => void;
    // Logging
    logMessage: (level: string, message: string) => void;
    // Session Management
    listSessions: () => Promise<SessionFile[]>;
    getSessionBuffer: (name: string) => Promise<{ success: boolean; buffer?: ArrayBuffer, error?: string }>;
    saveSession: (buffer: Uint8Array, nameHint?: string) => Promise<{ success: boolean; error?: string }>;
    deleteSession: (name: string) => Promise<{ success: boolean; error?: string }>;
    renameSession: (oldName: string, newName: string) => Promise<{ success: boolean; error?: string }>;
    // Markdown content
    getMarkdownContent: (fileName: string) => Promise<{ success: boolean; content?: string; error?: string }>;
    // Custom App functions
    getSystemFonts: () => Promise<string[]>;
    // Window & Lifecycle
    setTitle: (title: string) => void;
    onSaveBeforeQuit: (callback: () => void) => () => void; // Returns a function to unsubscribe
    savedAndReadyToQuit: () => void;
    // Auto-updater
    onUpdateStatus: (callback: (status: string, data: any) => void) => () => void;
    installUpdate: () => void;
}

declare global {
    interface Window {
        electronAPI: IElectronAPI;
        isAppDirty?: () => { isDirty: boolean; sessionName: string | null };
    }
}
