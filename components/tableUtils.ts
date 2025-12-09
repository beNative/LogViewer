import { LogTableDensity } from '../types';

/**
 * Returns Tailwind CSS classes for log level badge styling.
 * Provides both light and dark mode variants.
 * @param level - The log level string (ERROR, WARNING, INFO, DEBUG, etc.)
 * @returns String of Tailwind CSS classes for background and text colors
 */
export const getLevelColor = (level: string) => {
    switch (level?.toUpperCase()) {
        case 'ERROR':
        case 'FATAL':
            return 'bg-red-100 text-red-800 dark:bg-red-900/50 dark:text-red-300';
        case 'WARNING':
        case 'WARN':
            return 'bg-amber-100 text-amber-800 dark:bg-amber-900/50 dark:text-amber-300';
        case 'INFO':
            return 'bg-sky-100 text-sky-800 dark:bg-sky-900/50 dark:text-sky-300';
        case 'DEBUG':
        case 'TRACE':
            return 'bg-gray-200 text-gray-800 dark:bg-gray-700 dark:text-gray-300';
        default:
            return 'bg-gray-100 text-gray-700 dark:bg-gray-800 dark:text-gray-400';
    }
};

export const getRowClass = (density: LogTableDensity) => {
    switch (density) {
        case 'compact': return 'py-0';
        case 'normal': return 'py-1';
        case 'comfortable': return 'py-2.5';
    }
};

export const getCellClass = (density: LogTableDensity) => {
    switch (density) {
        case 'compact': return 'px-2';
        case 'normal': return 'px-3';
        case 'comfortable': return 'px-4';
    }
};
