/**
 * Application-wide constants.
 * Centralizes magic numbers and configuration values for easier maintenance.
 */

// ============================================================================
// Application Metadata
// ============================================================================

/** Current application version - should match package.json */
export const APP_VERSION = '0.24.0';

/** Application name */
export const APP_NAME = 'Log Analyser';

// ============================================================================
// UI Constants
// ============================================================================

/** Default number of entries to load per chunk in infinite scroll mode */
export const DEFAULT_SCROLL_CHUNK_SIZE = 200;

/** Default number of density buckets for log timeline visualization */
export const DEFAULT_DENSITY_BUCKET_COUNT = 200;

/** Maximum density buckets for high-resolution timeline */
export const MAX_DENSITY_BUCKET_COUNT = 300;

/** Default page sizes for pagination mode */
export const PAGE_SIZE_OPTIONS = [100, 250, 500, 1000, 2500] as const;

// ============================================================================
// Log Table Row Heights (in pixels)
// ============================================================================

/** Row height for compact density mode */
export const ROW_HEIGHT_COMPACT = 28;

/** Row height for normal density mode */
export const ROW_HEIGHT_NORMAL = 36;

/** Row height for comfortable density mode */
export const ROW_HEIGHT_COMFORTABLE = 44;

// ============================================================================
// Debounce/Throttle Timings (in milliseconds)
// ============================================================================

/** Debounce delay for filter input changes */
export const FILTER_DEBOUNCE_MS = 150;

/** Debounce delay for search input changes */
export const SEARCH_DEBOUNCE_MS = 200;

/** Debounce delay for resize observer callbacks */
export const RESIZE_DEBOUNCE_MS = 100;

/** Delay before showing busy indicator */
export const BUSY_INDICATOR_DELAY_MS = 50;

// ============================================================================
// Database Constants
// ============================================================================

/** Number of stock suggestions to return from autocomplete */
export const STOCK_SUGGESTION_LIMIT = 15;

/** Pattern for detecting stock info messages in logs */
export const STOCK_INFO_PATTERN = '<stockinfomessage';

// ============================================================================
// Animation Durations (in milliseconds)
// ============================================================================

/** Standard transition duration for UI elements */
export const TRANSITION_DURATION_MS = 150;

/** Toast notification auto-dismiss duration */
export const TOAST_DURATION_MS = 5000;

// ============================================================================
// Table Configuration
// ============================================================================

import { ColumnKey } from './types.ts';

/** Definition of all available columns in the log table */
export const COLUMN_DEFINITIONS: { key: ColumnKey; label: string; minWidth: number; flex: number }[] = [
    { key: 'time', label: 'Time', minWidth: 170, flex: 0 },
    { key: 'level', label: 'Level', minWidth: 80, flex: 0 },
    { key: 'sndrtype', label: 'Sender Type', minWidth: 100, flex: 0 },
    { key: 'sndrname', label: 'Sender Name', minWidth: 100, flex: 0 },
    { key: 'fileName', label: 'Filename', minWidth: 130, flex: 0 },
    { key: 'msg', label: 'Message', minWidth: 200, flex: 1 },
];
