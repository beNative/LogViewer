import React from 'react';
import { ToastMessage, IconSet } from '../types.ts';
import { Icon, IconName } from './icons/index.tsx';

interface ToastProps {
    toast: ToastMessage;
    onDismiss: () => void;
    iconSet: IconSet;
}

const toastConfig: Record<ToastMessage['type'], { icon: IconName; iconClass: string; bgClass: string; }> = {
    info: { icon: 'InformationCircle', iconClass: 'text-sky-500 dark:text-sky-400', bgClass: 'bg-sky-50 dark:bg-sky-900/50' },
    success: { icon: 'CheckCircle', iconClass: 'text-green-500 dark:text-green-400', bgClass: 'bg-green-50 dark:bg-green-900/50' },
    warning: { icon: 'ExclamationTriangle', iconClass: 'text-amber-500 dark:text-amber-400', bgClass: 'bg-amber-50 dark:bg-amber-900/50' },
    error: { icon: 'XCircle', iconClass: 'text-red-500 dark:text-red-400', bgClass: 'bg-red-50 dark:bg-red-900/50' },
    progress: { icon: 'Download', iconClass: 'text-sky-500 dark:text-sky-400', bgClass: 'bg-sky-50 dark:bg-sky-900/50' },
};

export const Toast: React.FC<ToastProps> = ({ toast, onDismiss, iconSet }) => {
    const { type, title, message, duration = 5000, progress, actions } = toast;
    const config = toastConfig[type];

    React.useEffect(() => {
        if (duration > 0) {
            const timer = setTimeout(onDismiss, duration);
            return () => clearTimeout(timer);
        }
    }, [duration, onDismiss]);

    return (
        <div className={`w-full bg-white dark:bg-gray-800 shadow-lg rounded-lg pointer-events-auto ring-1 ring-black ring-opacity-5 dark:ring-white/10 overflow-hidden animate-fadeIn`}>
            <div className="p-4">
                <div className="flex items-start">
                    <div className="flex-shrink-0">
                        <Icon name={config.icon} iconSet={iconSet} className={`w-6 h-6 ${config.iconClass}`} aria-hidden="true" />
                    </div>
                    <div className="ml-3 w-0 flex-1 pt-0.5">
                        <p className="text-sm font-medium text-gray-900 dark:text-white">{title}</p>
                        <p className="mt-1 text-sm text-gray-500 dark:text-gray-400">{message}</p>
                        {type === 'progress' && typeof progress === 'number' && (
                            <div className="mt-2 w-full bg-gray-200 dark:bg-gray-700 rounded-full h-2">
                                <div
                                    className="bg-sky-500 h-2 rounded-full transition-all duration-300 ease-out"
                                    style={{ width: `${progress.toFixed(2)}%` }}
                                ></div>
                            </div>
                        )}
                        {actions && actions.length > 0 && (
                            <div className="mt-3 flex space-x-3">
                                {actions.map((action, i) => (
                                    <button
                                        key={i}
                                        onClick={action.onClick}
                                        className="bg-white dark:bg-gray-800 px-3 py-1.5 rounded-md text-sm font-medium text-sky-600 dark:text-sky-400 hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white dark:focus:ring-offset-gray-800 focus:ring-sky-500"
                                    >
                                        {action.label}
                                    </button>
                                ))}
                            </div>
                        )}
                    </div>
                    <div className="ml-4 flex-shrink-0 flex">
                        <button
                            onClick={onDismiss}
                            className="inline-flex text-gray-400 dark:text-gray-500 hover:text-gray-500 dark:hover:text-gray-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-sky-500"
                        >
                            <span className="sr-only">Close</span>
                            <Icon name="XMark" iconSet={iconSet} className="w-5 h-5" aria-hidden="true" />
                        </button>
                    </div>
                </div>
            </div>
        </div>
    );
};
