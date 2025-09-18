import React from 'react';
import { Icon } from './icons/index.tsx';
import { IconSet } from '../types.ts';

export interface AboutDialogProps {
    isOpen: boolean;
    onClose: () => void;
    iconSet: IconSet;
    version: string;
}

export const AboutDialog: React.FC<AboutDialogProps> = ({ isOpen, onClose, iconSet, version }) => {
    const dialogRef = React.useRef<HTMLDivElement>(null);

    React.useEffect(() => {
        if (!isOpen) return;

        const handleKeyDown = (event: KeyboardEvent) => {
            if (event.key === 'Escape') {
                onClose();
            }
        };

        document.addEventListener('keydown', handleKeyDown);

        // Focus management
        const focusableElements = dialogRef.current?.querySelectorAll<HTMLElement>(
            'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
        );
        const firstElement = focusableElements?.[0];
        const lastElement = focusableElements?.[focusableElements.length - 1];

        const handleTabKey = (e: KeyboardEvent) => {
            if (e.key !== 'Tab' || !focusableElements || focusableElements.length === 0) return;
            
            if (e.shiftKey) {
                if (document.activeElement === firstElement) {
                    lastElement?.focus();
                    e.preventDefault();
                }
            } else {
                if (document.activeElement === lastElement) {
                    firstElement?.focus();
                    e.preventDefault();
                }
            }
        };
        
        firstElement?.focus();
        dialogRef.current?.addEventListener('keydown', handleTabKey);

        return () => {
            document.removeEventListener('keydown', handleKeyDown);
            dialogRef.current?.removeEventListener('keydown', handleTabKey);
        };
    }, [isOpen, onClose]);

    if (!isOpen) {
        return null;
    }

    return (
        <div 
            className="fixed inset-0 bg-gray-900 bg-opacity-70 flex items-center justify-center z-50 animate-fadeIn"
            onClick={onClose}
            role="dialog"
            aria-modal="true"
            aria-labelledby="about-dialog-title"
        >
            <div
                ref={dialogRef}
                className="bg-white dark:bg-gray-800 rounded-2xl shadow-xl w-full max-w-md p-6 text-center relative"
                onClick={e => e.stopPropagation()}
            >
                <div className="mx-auto flex items-center justify-center h-16 w-16 rounded-full bg-sky-100 dark:bg-sky-900 mb-4">
                    <Icon name="BugAnt" iconSet={iconSet} className="h-10 w-10 text-sky-600 dark:text-sky-400" />
                </div>
                <h3 id="about-dialog-title" className="text-2xl font-bold text-gray-900 dark:text-white">
                    Log Analyser
                </h3>
                <p className="text-sm text-gray-500 dark:text-gray-400 mb-4">Version {version}</p>

                <div className="text-gray-700 dark:text-gray-300 space-y-2 my-6">
                    <p>Design and concept by Tim Sinaeve.</p>
                    <p>Implementation by Gemini Pro 2.5.</p>
                </div>

                <p className="text-xs text-gray-500 dark:text-gray-500">
                    &copy; 2025 Tim Sinaeve. All rights reserved.
                </p>

                <div className="mt-8">
                    <button
                        type="button"
                        onClick={onClose}
                        className="inline-flex justify-center w-full rounded-md border border-transparent shadow-sm px-4 py-2 bg-sky-600 text-base font-medium text-white hover:bg-sky-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-white dark:focus:ring-offset-gray-800 focus:ring-sky-500 sm:text-sm"
                    >
                        Close
                    </button>
                </div>
            </div>
        </div>
    );
};