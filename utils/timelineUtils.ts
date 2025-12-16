/**
 * Timeline utility functions and constants.
 * Extracted from TimeRangeSelector.tsx for reusability and testing.
 */

export type Theme = 'light' | 'dark';

// Color palette for timeline segments (pages, files, dates)
export const TIMELINE_PALETTE = [
    '#3b82f6', '#ef4444', '#10b981', '#f59e0b', '#8b5cf6', '#ec4899',
    '#6366f1', '#06b6d4', '#d946ef', '#f43f5e', '#14b8a6', '#eab308'
];

// Log level colors for density visualization
export const LOG_LEVEL_COLORS: Record<string, { light: string, dark: string }> = {
    'ERROR': { light: '#ef4444', dark: '#f87171' },
    'FATAL': { light: '#b91c1c', dark: '#ef4444' },
    'WARNING': { light: '#f59e0b', dark: '#facc15' },
    'WARN': { light: '#f59e0b', dark: '#facc15' },
    'INFO': { light: '#3b82f6', dark: '#60a5fa' },
    'DEBUG': { light: '#6b7280', dark: '#9ca3af' },
    'TRACE': { light: '#a1a1aa', dark: '#71717a' },
    'DEFAULT': { light: '#d1d5db', dark: '#4b5563' },
};

// Consistent level order for stacked density bars - critical/important levels first
export const LOG_LEVEL_ORDER: Record<string, number> = {
    'FATAL': 0, 'ERROR': 1, 'WARNING': 2, 'WARN': 3,
    'INFO': 4, 'DEBUG': 5, 'TRACE': 6
};

// UI constants
export const TIMELINE_CONSTANTS = {
    MIN_CLICK_DRAG_PX: 5,      // Minimum pixels to distinguish click from drag
    TOOLTIP_GAP_PX: 10,        // Gap from viewport edges for tooltips
    MIN_BAR_WIDTH_PX: 1,       // Minimum width for a timeline segment
    LABEL_MIN_WIDTH_PX: 30,    // Minimum width to show label text
    TICK_MIN_SPACING_PX: 120,  // Minimum spacing between tick marks
    TOOLTIP_HIDE_DISTANCE: 60, // Hide end tooltip when handles are within this distance
};

/**
 * Get the color for a log level based on theme.
 * @param level - Log level string (e.g., 'ERROR', 'INFO')
 * @param theme - Current theme ('light' or 'dark')
 * @returns Hex color string
 */
export const getLevelColor = (level: string, theme: Theme): string => {
    const upperLevel = level?.toUpperCase() || 'DEFAULT';
    return (LOG_LEVEL_COLORS[upperLevel] || LOG_LEVEL_COLORS.DEFAULT)[theme];
};

/**
 * Format a timestamp for display as date and time parts.
 * @param time - Unix timestamp in milliseconds
 * @returns Object with datePart (YYYY-MM-DD) and timePart (HH:MM:SS.mmm)
 */
export const formatTimestamp = (time: number): { datePart: string, timePart: string } | null => {
    if (typeof time !== 'number' || isNaN(time)) return null;
    const d = new Date(time);
    const isoString = d.toISOString(); // YYYY-MM-DDTHH:mm:ss.sssZ
    return {
        datePart: isoString.substring(0, 10),
        timePart: isoString.substring(11, 23) // Includes milliseconds
    };
};

/**
 * Format a tick label based on the time range duration.
 * Shows more detail for shorter durations.
 * @param time - Unix timestamp in milliseconds
 * @param duration - Total range duration in milliseconds
 * @returns Formatted string for display
 */
export const formatTickLabel = (time: number, duration: number): string => {
    const d = new Date(time);
    if (duration <= 2 * 60 * 1000) { // < 2 minutes
        return d.toLocaleTimeString('en-US', { hour12: false, minute: '2-digit', second: '2-digit' });
    }
    if (duration <= 2 * 60 * 60 * 1000) { // < 2 hours
        return d.toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' });
    }
    if (duration <= 2 * 24 * 60 * 60 * 1000) { // < 2 days
        return `${d.toLocaleDateString('en-CA', { month: '2-digit', day: '2-digit' })} ${d.toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' })}`;
    }
    return d.toLocaleDateString('en-CA'); // YYYY-MM-DD
};

/**
 * Calculate nice interval step for tick marks.
 * @param duration - Range duration in milliseconds
 * @param tickCount - Desired number of ticks
 * @returns Step size in milliseconds
 */
export const calculateNiceInterval = (duration: number, tickCount: number): number => {
    const niceIntervals = [
        1000, 5000, 15000, 30000, // seconds
        60 * 1000, 5 * 60 * 1000, 15 * 60 * 1000, 30 * 60 * 1000, // minutes
        60 * 60 * 1000, 2 * 60 * 60 * 1000, 6 * 60 * 60 * 1000, 12 * 60 * 60 * 1000, // hours
        24 * 60 * 60 * 1000, 2 * 24 * 60 * 60 * 1000, 7 * 24 * 60 * 60 * 1000 // days
    ];
    const targetStep = duration / tickCount;
    return niceIntervals.find(i => i > targetStep) || niceIntervals[niceIntervals.length - 1];
};
