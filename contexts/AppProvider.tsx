import React from 'react';
import { ToastProvider } from './ToastContext';
import { ConsoleProvider } from './ConsoleContext';
import { SettingsProvider } from './SettingsContext';
import { UIProvider } from './UIContext';
import { SessionProvider } from './SessionContext';
import { DataProvider } from './DataContext';

export const AppProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    return (
        <ToastProvider>
            <ConsoleProvider>
                <SettingsProvider>
                    <UIProvider>
                        <SessionProvider>
                            <DataProvider>
                                {children}
                            </DataProvider>
                        </SessionProvider>
                    </UIProvider>
                </SettingsProvider>
            </ConsoleProvider>
        </ToastProvider>
    );
};
