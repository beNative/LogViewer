import React, { createContext, useContext, useState, useCallback } from 'react';
import { IconSet } from '../types';
import { Icon } from '../components/icons';

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

    const typeStyles: Record<ConfirmDialogType, { icon: string; iconColor: string; confirmBtnColor: string }> = {
        danger: {
            icon: 'ExclamationTriangle',
            iconColor: 'text-red-500',
            confirmBtnColor: 'bg-red-600 hover:bg-red-700 focus:ring-red-500',
        },
        warning: {
            icon: 'ExclamationTriangle',
            iconColor: 'text-orange-500',
            confirmBtnColor: 'bg-orange-600 hover:bg-orange-700 focus:ring-orange-500',
        },
        info: {
            icon: 'QuestionMarkCircle',
            iconColor: 'text-blue-500',
            confirmBtnColor: 'bg-blue-600 hover:bg-blue-700 focus:ring-blue-500',
        },
    };

    const currentStyle = typeStyles[dialogState.type || 'info'];

    return (
        <ConfirmDialogContext.Provider value={{ confirm }}>
            {children}

            {/* Dialog Overlay */}
            {dialogState.isOpen && (
                <div
                    className="fixed inset-0 z-[100] flex items-center justify-center bg-black/50 backdrop-blur-sm"
                    onClick={handleCancel}
                >
                    {/* Dialog Box */}
                    <div
                        className="bg-white dark:bg-gray-800 rounded-xl shadow-2xl max-w-md w-full mx-4 overflow-hidden transform transition-all"
                        onClick={(e) => e.stopPropagation()}
                    >
                        {/* Header with Icon */}
                        <div className="flex items-center gap-4 p-6 pb-4">
                            <div className={`flex-shrink-0 w-12 h-12 rounded-full flex items-center justify-center ${dialogState.type === 'danger' ? 'bg-red-100 dark:bg-red-900/30' :
                                    dialogState.type === 'warning' ? 'bg-orange-100 dark:bg-orange-900/30' :
                                        'bg-blue-100 dark:bg-blue-900/30'
                                }`}>
                                <Icon
                                    name={currentStyle.icon as any}
                                    iconSet={iconSet}
                                    className={`w-6 h-6 ${currentStyle.iconColor}`}
                                />
                            </div>
                            <div className="flex-grow">
                                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                                    {dialogState.title}
                                </h3>
                            </div>
                        </div>

                        {/* Message */}
                        <div className="px-6 pb-6">
                            <p className="text-sm text-gray-600 dark:text-gray-300 leading-relaxed">
                                {dialogState.message}
                            </p>
                        </div>

                        {/* Action Buttons */}
                        <div className="flex items-center justify-end gap-3 px-6 py-4 bg-gray-50 dark:bg-gray-700/50 border-t border-gray-200 dark:border-gray-700">
                            <button
                                onClick={handleCancel}
                                className="px-4 py-2 text-sm font-medium text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-600 border border-gray-300 dark:border-gray-500 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-500 transition-colors"
                            >
                                {dialogState.cancelText}
                            </button>
                            <button
                                onClick={handleConfirm}
                                autoFocus
                                className={`px-4 py-2 text-sm font-medium text-white rounded-lg focus:outline-none focus:ring-2 focus:ring-offset-2 transition-colors ${currentStyle.confirmBtnColor}`}
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
