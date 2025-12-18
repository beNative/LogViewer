import { describe, it, expect } from 'vitest';
import {
    getLevelColor,
    formatTimestamp,
    formatTickLabel,
    calculateNiceInterval,
    LOG_LEVEL_COLORS
} from './timelineUtils';

describe('timelineUtils', () => {
    describe('getLevelColor', () => {
        it('should return correct light theme color for ERROR', () => {
            expect(getLevelColor('ERROR', 'light')).toBe(LOG_LEVEL_COLORS.ERROR.light);
        });

        it('should return correct dark theme color for INFO', () => {
            expect(getLevelColor('INFO', 'dark')).toBe(LOG_LEVEL_COLORS.INFO.dark);
        });

        it('should case-insensitive match levels', () => {
            expect(getLevelColor('error', 'light')).toBe(LOG_LEVEL_COLORS.ERROR.light);
        });

        it('should return DEFAULT color for unknown levels', () => {
            expect(getLevelColor('UNKNOWN_LEVEL', 'light')).toBe(LOG_LEVEL_COLORS.DEFAULT.light);
        });

        it('should handle null/undefined level gracefully', () => {
            expect(getLevelColor(null as any, 'light')).toBe(LOG_LEVEL_COLORS.DEFAULT.light);
            expect(getLevelColor(undefined as any, 'light')).toBe(LOG_LEVEL_COLORS.DEFAULT.light);
        });
    });

    describe('formatTimestamp', () => {
        it('should return null for invalid inputs', () => {
            expect(formatTimestamp(NaN)).toBeNull();
            expect(formatTimestamp(undefined as any)).toBeNull();
        });

        it('should format valid timestamp correctly', () => {
            // Use a fixed timestamp: 2023-01-01T12:00:00.123Z
            const time = new Date('2023-01-01T12:00:00.123Z').getTime();
            const result = formatTimestamp(time);
            expect(result).toEqual({
                datePart: '2023-01-01',
                timePart: '12:00:00.123'
            });
        });
    });

    describe('calculateNiceInterval', () => {
        it('should return appropriate interval for short duration', () => {
            const duration = 10000; // 10 seconds
            const ticks = 5;
            // Target step ~2000. Closest nice interval > 2000 is 5000.
            expect(calculateNiceInterval(duration, ticks)).toBe(5000);
        });

        it('should return appropriate interval for long duration', () => {
            const duration = 24 * 60 * 60 * 1000; // 1 day
            const ticks = 10;
            // Target step ~2.4 hours. Closest > 2.4h is 6h (6 * 60 * 60 * 1000).
            expect(calculateNiceInterval(duration, ticks)).toBe(6 * 60 * 60 * 1000);
        });
    });

    describe('formatTickLabel', () => {
        const time = new Date('2023-01-01T12:30:45.000Z').getTime();

        it('should show seconds for very short duration (< 2 min)', () => {
            const duration = 60 * 1000; // 1 minute
            const label = formatTickLabel(time, duration);
            // Expect MM:SS since hour is excluded in options
            expect(label).toMatch(/\d{2}:\d{2}/);
        });

        it('should show date for long duration (> 2 days)', () => {
            const duration = 3 * 24 * 60 * 60 * 1000;
            const label = formatTickLabel(time, duration);
            // Duration > 2 days should show date only format
            expect(label).toMatch(/^\d{4}-\d{2}-\d{2}$/); // 2023-01-01
        });
    });
});
