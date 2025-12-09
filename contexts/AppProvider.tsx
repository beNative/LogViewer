import React from 'react';
import { ToastProvider } from './ToastContext';
import { ConsoleProvider } from './ConsoleContext';
import { SettingsProvider, useSettings } from './SettingsContext';
import { UIProvider } from './UIContext';
import { SessionProvider } from './SessionContext';
import { DataProvider } from './DataContext';
import { ConfirmDialogProvider } from './ConfirmDialogContext';

// Inner provider that has access to settings for the confirm dialog
const ConfirmDialogWrapper: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const { iconSet } = useSettings();
    return (
        <ConfirmDialogProvider iconSet={iconSet}>
            {children}
        </ConfirmDialogProvider>
    );
};

export const AppProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    return (
        <ToastProvider>
            <ConsoleProvider>
                <SettingsProvider>
                    <ConfirmDialogWrapper>
                        <UIProvider>
                            <SessionProvider>
                                <DataProvider>
                                    {children}
                                </DataProvider>
                            </SessionProvider>
                        </UIProvider>
                    </ConfirmDialogWrapper>
                </SettingsProvider>
            </ConsoleProvider>
        </ToastProvider>
    );
};
