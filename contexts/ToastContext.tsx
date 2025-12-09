import React, { createContext, useState, useCallback, useContext } from 'react';
import { ToastMessage } from '../types';

type ToastContextType = {
    toasts: ToastMessage[];
    addToast: (toast: Omit<ToastMessage, 'id'> & { id?: string }) => void;
    removeToast: (id: string) => void;
};

const ToastContext = createContext<ToastContextType | undefined>(undefined);

/**
 * Provider for toast notification management.
 * Supports adding, updating (via matching id), and removing toast messages.
 * Toast types include info, success, warning, error, and progress.
 */
export const ToastProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const [toasts, setToasts] = useState<ToastMessage[]>([]);

    const addToast = useCallback((toast: Omit<ToastMessage, 'id'> & { id?: string }) => {
        const id = toast.id || new Date().getTime().toString() + Math.random();
        setToasts(prev => {
            if (prev.some(t => t.id === id)) {
                // If a toast with the same ID exists, update it instead of adding a new one.
                // This is crucial for progress toasts.
                return prev.map(t => t.id === id ? { ...t, ...toast, id } : t);
            }
            return [...prev, { ...toast, id }];
        });
    }, []);

    const removeToast = useCallback((id: string) => {
        setToasts(prev => prev.filter(t => t.id !== id));
    }, []);

    const value = { toasts, addToast, removeToast };

    return (
        <ToastContext.Provider value={value}>
            {children}
        </ToastContext.Provider>
    );
};

/**
 * Hook to access toast notification functionality.
 * @throws Error if used outside of ToastProvider
 * @returns ToastContextType with addToast and removeToast functions
 */
export const useToast = (): ToastContextType => {
    const context = useContext(ToastContext);
    if (context === undefined) {
        throw new Error('useToast must be used within a ToastProvider');
    }
    return context;
};
