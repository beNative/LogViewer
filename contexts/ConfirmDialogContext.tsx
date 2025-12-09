import React, { createContext, useContext, useState, useCallback } from 'react';
import { IconSet } from '../types';
import { Icon, IconName } from '../components/icons';

type ConfirmDialogType = 'danger' | 'warning' | 'info';

interface ConfirmDialogOptions {
    title: string;
    message: string;
    confirmText?: string;
    cancelText?: string;
    type?: ConfirmDialogType;
}

interface ConfirmDialogContextType {
    confirm: (options: ConfirmDialogOptions) => Promise<boolean>;
}

const ConfirmDialogContext = createContext<ConfirmDialogContextType | undefined>(undefined);

interface ConfirmDialogState extends ConfirmDialogOptions {
    isOpen: boolean;
    resolve: ((value: boolean) => void) | null;
}

export const ConfirmDialogProvider: React.FC<{ children: React.ReactNode; iconSet: IconSet }> = ({ children, iconSet }) => {
    const [dialogState, setDialogState] = useState<ConfirmDialogState>({
        isOpen: false,
        title: '',
        message: '',
        confirmText: 'Confirm',
        cancelText: 'Cancel',
        type: 'info',
        resolve: null,
    });

    const confirm = useCallback((options: ConfirmDialogOptions): Promise<boolean> => {
        return new Promise((resolve) => {
            setDialogState({
                isOpen: true,
                title: options.title,
                message: options.message,
                confirmText: options.confirmText || 'Confirm',
                cancelText: options.cancelText || 'Cancel',
                type: options.type || 'info',
                resolve,
            });
        });
    }, []);

    const handleConfirm = useCallback(() => {
        dialogState.resolve?.(true);
        setDialogState(prev => ({ ...prev, isOpen: false, resolve: null }));
    }, [dialogState.resolve]);

    const handleCancel = useCallback(() => {
        dialogState.resolve?.(false);
        setDialogState(prev => ({ ...prev, isOpen: false, resolve: null }));
    }, [dialogState.resolve]);

    const typeStyles: Record<ConfirmDialogType, { icon: IconName; iconBg: string; iconColor: string }> = {
        danger: {
            icon: 'ExclamationTriangle',
            iconBg: 'bg-red-100 dark:bg-red-900',
            iconColor: 'text-red-600 dark:text-red-400',
        },
        warning: {
            icon: 'ExclamationTriangle',
            iconBg: 'bg-amber-100 dark:bg-amber-900',
            iconColor: 'text-amber-600 dark:text-amber-400',
        },
        info: {
            icon: 'QuestionMarkCircle',
            iconBg: 'bg-sky-100 dark:bg-sky-900',
            iconColor: 'text-sky-600 dark:text-sky-400',
        },
    };

    const currentStyle = typeStyles[dialogState.type || 'info'];

    return (
        <ConfirmDialogContext.Provider value={{ confirm }}>
            {children}

            {/* Dialog Overlay */}
            {dialogState.isOpen && (
                <div
                    className="fixed inset-0 bg-gray-900 bg-opacity-70 flex items-center justify-center z-50 animate-fadeIn"
                    onClick={handleCancel}
                    role="dialog"
                    aria-modal="true"
                >
                    {/* Dialog Box */}
                    <div
                        className="bg-white dark:bg-gray-800 rounded-2xl shadow-xl w-full max-w-md p-6 text-center relative"
                        onClick={(e) => e.stopPropagation()}
                    >
                        {/* Icon */}
                        <div className={`mx-auto flex items-center justify-center h-16 w-16 rounded-full ${currentStyle.iconBg} mb-4`}>
                            <Icon
                                name={currentStyle.icon}
                                iconSet={iconSet}
                                className={`h-10 w-10 ${currentStyle.iconColor}`}
                            />
                        </div>

                        {/* Title */}
                        <h3 className="text-2xl font-bold text-gray-900 dark:text-white">
                            {dialogState.title}
                        </h3>

                        {/* Message */}
                        <p className="text-gray-700 dark:text-gray-300 my-6">
                            {dialogState.message}
                        </p>

                        {/* Action Buttons */}
                        <div className="flex gap-3">
                            <button
                                onClick={handleCancel}
                                className="flex-1 rounded-md border border-gray-300 dark:border-gray-600 shadow-sm px-4 py-2 bg-white dark:bg-gray-700 text-base font-medium text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white dark:focus:ring-offset-gray-800 focus:ring-sky-500 sm:text-sm"
                            >
                                {dialogState.cancelText}
                            </button>
                            <button
                                onClick={handleConfirm}
                                autoFocus
                                className="flex-1 rounded-md border border-transparent shadow-sm px-4 py-2 bg-sky-600 text-base font-medium text-white hover:bg-sky-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white dark:focus:ring-offset-gray-800 focus:ring-sky-500 sm:text-sm"
                            >
                                {dialogState.confirmText}
                            </button>
                        </div>
                    </div>
                </div>
            )}
        </ConfirmDialogContext.Provider>
    );
};

export const useConfirmDialog = (): ConfirmDialogContextType => {
    const context = useContext(ConfirmDialogContext);
    if (context === undefined) {
        throw new Error('useConfirmDialog must be used within a ConfirmDialogProvider');
    }
    return context;
};
