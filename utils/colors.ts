/**
 * Shared color constants and theme utilities for the application.
 * Centralizes color palettes used across charts and visualizations.
 */

import { Theme } from '../types';

/**
 * Color palette for differentiating items in charts and visualizations.
 * Based on Tailwind CSS color palette (500 shades).
 */
export const CHART_PALETTE = [
    '#3b82f6', // blue-500
    '#ef4444', // red-500
    '#10b981', // emerald-500
    '#f59e0b', // amber-500
    '#8b5cf6', // violet-500
    '#ec4899', // pink-500
    '#6366f1', // indigo-500
    '#06b6d4', // cyan-500
    '#d946ef', // fuchsia-500
    '#f43f5e', // rose-500
    '#14b8a6', // teal-500
    '#eab308', // yellow-500
] as const;

/**
 * Log level colors for light and dark themes.
 * Used in density charts and log level indicators.
 */
export const LOG_LEVEL_COLORS: Record<string, { light: string; dark: string }> = {
    'ERROR': { light: '#ef4444', dark: '#f87171' },
    'FATAL': { light: '#b91c1c', dark: '#ef4444' },
    'WARNING': { light: '#f59e0b', dark: '#facc15' },
    'WARN': { light: '#f59e0b', dark: '#facc15' },
    'INFO': { light: '#3b82f6', dark: '#60a5fa' },
    'DEBUG': { light: '#6b7280', dark: '#9ca3af' },
    'TRACE': { light: '#a1a1aa', dark: '#71717a' },
    'DEFAULT': { light: '#d1d5db', dark: '#4b5563' },
};

/**
 * Common chart colors for consistent theming across Chart.js components.
 */
export const CHART_THEME_COLORS = {
    light: {
        tick: '#4b5563',       // gray-600
        tooltipBg: '#ffffff',
        tooltipTitle: '#111827', // gray-900
        tooltipBody: '#374151',  // gray-700
        legend: '#374151',       // gray-700
        border: '#ffffff',
    },
    dark: {
        tick: '#9ca3af',         // gray-400
        tooltipBg: '#1f2937',    // gray-800
        tooltipTitle: '#f3f4f6', // gray-100
        tooltipBody: '#d1d5db',  // gray-300
        legend: '#d1d5db',       // gray-300
        border: '#1f2937',       // gray-800
    },
} as const;

/**
 * Gets the appropriate log level color for the given theme.
 * @param level - The log level string (ERROR, WARNING, INFO, etc.)
 * @param theme - The current theme ('light' or 'dark')
 * @returns Hex color string for the log level
 */
export const getLevelColorForTheme = (level: string, theme: Theme): string => {
    const upperLevel = level?.toUpperCase() || 'DEFAULT';
    return (LOG_LEVEL_COLORS[upperLevel] || LOG_LEVEL_COLORS.DEFAULT)[theme];
};

/**
 * Gets chart theme colors for the given theme.
 * @param theme - The current theme ('light' or 'dark')
 * @returns Object with tick, tooltip, legend, and border colors
 */
export const getChartColors = (theme: Theme) => CHART_THEME_COLORS[theme];
