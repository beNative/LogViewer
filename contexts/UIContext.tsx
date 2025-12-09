import React, { createContext, useState, useContext } from 'react';
import { ProgressPhase } from '../types';
import { useToast } from './ToastContext';

type UIContextType = {
    activeView: 'data' | 'viewer' | 'dashboard' | 'settings' | 'info' | 'stock';
    setActiveView: React.Dispatch<React.SetStateAction<'data' | 'viewer' | 'dashboard' | 'settings' | 'info' | 'stock'>>;
    isAboutDialogOpen: boolean;
    setIsAboutDialogOpen: React.Dispatch<React.SetStateAction<boolean>>;

    // Loading & Progress
    isLoading: boolean;
    setIsLoading: React.Dispatch<React.SetStateAction<boolean>>;
    isBusy: boolean;
    setIsBusy: React.Dispatch<React.SetStateAction<boolean>>;
    isStockBusy: boolean;
    setIsStockBusy: React.Dispatch<React.SetStateAction<boolean>>;
    progress: number;
    setProgress: React.Dispatch<React.SetStateAction<number>>;
    progressMessage: string;
    setProgressMessage: React.Dispatch<React.SetStateAction<string>>;
    progressPhase: ProgressPhase;
    setProgressPhase: React.Dispatch<React.SetStateAction<ProgressPhase>>;
    detailedProgress: { currentFile: string; fileBytesRead: number; fileTotalBytes: number; fileLogCount: number | null; };
    setDetailedProgress: React.Dispatch<React.SetStateAction<{ currentFile: string; fileBytesRead: number; fileTotalBytes: number; fileLogCount: number | null; }>>;
    progressTitle: string;
    setProgressTitle: React.Dispatch<React.SetStateAction<string>>;

    // Interaction State
    keyboardSelectedId: number | null;
    setKeyboardSelectedId: React.Dispatch<React.SetStateAction<number | null>>;
    jumpToEntryId: number | null;
    setJumpToEntryId: React.Dispatch<React.SetStateAction<number | null>>;
    isInitialLoad: boolean;
    setIsInitialLoad: React.Dispatch<React.SetStateAction<boolean>>;

    // Expose addToast and setError for convenience
    addToast: ReturnType<typeof useToast>['addToast'];
    error: string | null;
    setError: React.Dispatch<React.SetStateAction<string | null>>;
};

const UIContext = createContext<UIContextType | undefined>(undefined);

/**
 * Provider for global UI state including view navigation, loading indicators,
 * progress tracking, and keyboard selection state.
 * Must wrap components that need access to UI state via useUI hook.
 */
export const UIProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { addToast } = useToast();

    const [activeView, setActiveView] = useState<'data' | 'viewer' | 'dashboard' | 'settings' | 'info' | 'stock'>('data');
    const [isAboutDialogOpen, setIsAboutDialogOpen] = useState(false);

    const [isLoading, setIsLoading] = useState<boolean>(false);
    const [isBusy, setIsBusy] = useState<boolean>(false);
    const [isStockBusy, setIsStockBusy] = useState<boolean>(false);
    const [progress, setProgress] = useState<number>(0);
    const [progressMessage, setProgressMessage] = useState<string>('');
    const [progressPhase, setProgressPhase] = useState<ProgressPhase>('reading');
    const [detailedProgress, setDetailedProgress] = useState({ currentFile: '', fileBytesRead: 0, fileTotalBytes: 0, fileLogCount: null as number | null });
    const [progressTitle, setProgressTitle] = useState('Processing Files...');

    const [keyboardSelectedId, setKeyboardSelectedId] = useState<number | null>(null);
    const [jumpToEntryId, setJumpToEntryId] = useState<number | null>(null);
    const [isInitialLoad, setIsInitialLoad] = useState(true);
    const [error, setError] = useState<string | null>(null);

    const value = {
        activeView, setActiveView,
        isAboutDialogOpen, setIsAboutDialogOpen,
        isLoading, setIsLoading,
        isBusy, setIsBusy,
        isStockBusy, setIsStockBusy,
        progress, setProgress,
        progressMessage, setProgressMessage,
        progressPhase, setProgressPhase,
        detailedProgress, setDetailedProgress,
        progressTitle, setProgressTitle,
        keyboardSelectedId, setKeyboardSelectedId,
        jumpToEntryId, setJumpToEntryId,
        isInitialLoad, setIsInitialLoad,
        addToast,
        error, setError
    };

    return (
        <UIContext.Provider value={value}>
            {children}
        </UIContext.Provider>
    );
};

/**
 * Hook to access global UI state.
 * @throws Error if used outside of UIProvider
 * @returns UIContextType with view state, loading indicators, and progress tracking
 */
export const useUI = (): UIContextType => {
    const context = useContext(UIContext);
    if (context === undefined) {
        throw new Error('useUI must be used within a UIProvider');
    }
    return context;
};