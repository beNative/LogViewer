import React from 'react';
import { ToastProvider } from './ToastContext';
import { ConsoleProvider } from './ConsoleContext';
import { SettingsProvider, useSettings } from './SettingsContext';
import { UIProvider } from './UIContext';
import { SessionProvider } from './SessionContext';
import { DataProvider } from './DataContext';
import { AnalyticsProvider } from './AnalyticsContext';
import { TimelineProvider } from './TimelineContext';
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
                                <TimelineProvider>
                                    <DataProvider>
                                        <AnalyticsProvider>
                                            {children}
                                        </AnalyticsProvider>
                                    </DataProvider>
                                </TimelineProvider>
                            </SessionProvider>
                        </UIProvider>
                    </ConfirmDialogWrapper>
                </SettingsProvider>
            </ConsoleProvider>
        </ToastProvider>
    );
};
