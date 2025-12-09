import React, { createContext, useState, useCallback, useContext } from 'react';
import { ConsoleMessage, ConsoleMessageType } from '../types';

type ConsoleContextType = {
    consoleMessages: ConsoleMessage[];
    lastConsoleMessage: ConsoleMessage | null;
    consoleFilters: Record<ConsoleMessageType, boolean>;
    setConsoleFilters: React.Dispatch<React.SetStateAction<Record<ConsoleMessageType, boolean>>>;
    consoleSearchTerm: string;
    setConsoleSearchTerm: React.Dispatch<React.SetStateAction<string>>;
    logToConsole: (message: string, type: ConsoleMessageType) => void;
    handleClearConsole: () => void;
};

const initialConsoleFilters: Record<ConsoleMessageType, boolean> = {
    DEBUG: true,
    INFO: true,
    WARNING: true,
    ERROR: true,
};

const ConsoleContext = createContext<ConsoleContextType | undefined>(undefined);

/**
 * Provider for application console/logging functionality.
 * Manages diagnostic messages with severity levels (DEBUG, INFO, WARNING, ERROR),
 * integrates with Electron's file logging, and provides filtering and search.
 */
export const ConsoleProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const [consoleMessages, setConsoleMessages] = useState<ConsoleMessage[]>([]);
    const [lastConsoleMessage, setLastConsoleMessage] = useState<ConsoleMessage | null>(null);
    const [consoleFilters, setConsoleFilters] = useState(initialConsoleFilters);
    const [consoleSearchTerm, setConsoleSearchTerm] = useState('');

    const logToConsole = useCallback((message: string, type: ConsoleMessage['type']) => {
        if (window.electronAPI?.logMessage) {
            window.electronAPI.logMessage(type, message);
        }
        const newMessage: ConsoleMessage = {
            message,
            type,
            timestamp: new Date().toLocaleTimeString('en-US', { hour12: false })
        };
        setConsoleMessages(prev => [...prev, newMessage]);
        setLastConsoleMessage(newMessage);
    }, []);

    const handleClearConsole = useCallback(() => {
        setConsoleMessages([]);
        logToConsole('Console cleared.', 'INFO');
    }, [logToConsole]);

    const value = {
        consoleMessages,
        lastConsoleMessage,
        consoleFilters,
        setConsoleFilters,
        consoleSearchTerm,
        setConsoleSearchTerm,
        logToConsole,
        handleClearConsole,
    };

    return (
        <ConsoleContext.Provider value={value}>
            {children}
        </ConsoleContext.Provider>
    );
};

/**
 * Hook to access console logging functionality.
 * @throws Error if used outside of ConsoleProvider
 * @returns ConsoleContextType with message logging and filtering capabilities
 */
export const useConsole = (): ConsoleContextType => {
    const context = useContext(ConsoleContext);
    if (context === undefined) {
        throw new Error('useConsole must be used within a ConsoleProvider');
    }
    return context;
};
