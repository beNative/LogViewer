import { describe, it, expect } from 'vitest';
import {
    highlightText,
    areArraysEqualUnordered,
    areFiltersEqual,
    formatBytes,
    parseFilterDateTime
} from './utils';
import { FilterState } from './types';

describe('utils', () => {
    describe('highlightText', () => {
        it('should return escaped text when no terms provided', () => {
            const result = highlightText('Hello <world>', [], 'light');
            expect(result).toBe('Hello &lt;world&gt;');
        });

        it('should return escaped text when terms array is empty', () => {
            const result = highlightText('Test text', [], 'dark');
            expect(result).toBe('Test text');
        });

        it('should highlight a single term case-insensitively', () => {
            const result = highlightText('Hello World', ['world'], 'light');
            expect(result).toContain('<mark');
            expect(result).toContain('World</mark>');
        });

        it('should highlight multiple occurrences of a term', () => {
            const result = highlightText('error in error log with error', ['error'], 'light');
            const matches = result.match(/<mark/g);
            expect(matches).toHaveLength(3);
        });

        it('should highlight multiple different terms', () => {
            const result = highlightText('Error and Warning found', ['error', 'warning'], 'dark');
            expect(result).toContain('Error</mark>');
            expect(result).toContain('Warning</mark>');
        });

        it('should escape special HTML characters in non-highlighted text', () => {
            const result = highlightText('<script>alert("xss")</script>', ['alert'], 'light');
            expect(result).toContain('&lt;script&gt;');
            expect(result).toContain('&quot;');
            expect(result).not.toContain('<script>');
        });

        it('should use dark theme highlight class when theme is dark', () => {
            const result = highlightText('test', ['test'], 'dark');
            expect(result).toContain('bg-yellow-500/40');
        });

        it('should use light theme highlight class when theme is light', () => {
            const result = highlightText('test', ['test'], 'light');
            expect(result).toContain('bg-yellow-300/70');
        });

        it('should handle empty text', () => {
            const result = highlightText('', ['term'], 'light');
            expect(result).toBe('');
        });

        it('should escape regex special characters in search terms', () => {
            const result = highlightText('file.txt (copy)', ['file.txt', '(copy)'], 'light');
            expect(result).toContain('file.txt</mark>');
            expect(result).toContain('(copy)</mark>');
        });
    });

    describe('areArraysEqualUnordered', () => {
        it('should return true for identical arrays', () => {
            expect(areArraysEqualUnordered(['a', 'b', 'c'], ['a', 'b', 'c'])).toBe(true);
        });

        it('should return true for arrays with same elements in different order', () => {
            expect(areArraysEqualUnordered(['c', 'a', 'b'], ['a', 'b', 'c'])).toBe(true);
        });

        it('should return false for arrays with different lengths', () => {
            expect(areArraysEqualUnordered(['a', 'b'], ['a', 'b', 'c'])).toBe(false);
        });

        it('should return false for arrays with different elements', () => {
            expect(areArraysEqualUnordered(['a', 'b', 'c'], ['a', 'b', 'd'])).toBe(false);
        });

        it('should return true for empty arrays', () => {
            expect(areArraysEqualUnordered([], [])).toBe(true);
        });

        it('should handle single element arrays', () => {
            expect(areArraysEqualUnordered(['a'], ['a'])).toBe(true);
            expect(areArraysEqualUnordered(['a'], ['b'])).toBe(false);
        });
    });

    describe('areFiltersEqual', () => {
        const baseFilter: FilterState = {
            dateFrom: '', timeFrom: '', dateTo: '', timeTo: '',
            level: [], levelFilterMode: 'include',
            sndrtype: [], sndrtypeFilterMode: 'include',
            sndrname: [], sndrnameFilterMode: 'include',
            fileName: [], fileNameFilterMode: 'include',
            includeMsg: '', excludeMsg: '',
            includeMsgMode: 'OR', excludeMsgMode: 'AND',
            sqlQueryEnabled: false, sqlQuery: ''
        };

        it('should return true for identical filter objects', () => {
            const a = { ...baseFilter };
            const b = { ...baseFilter };
            expect(areFiltersEqual(a, b)).toBe(true);
        });

        it('should return true for filters with same array values in different order', () => {
            const a = { ...baseFilter, level: ['ERROR', 'WARNING', 'INFO'] };
            const b = { ...baseFilter, level: ['INFO', 'ERROR', 'WARNING'] };
            expect(areFiltersEqual(a, b)).toBe(true);
        });

        it('should return false for filters with different string values', () => {
            const a = { ...baseFilter, dateFrom: '2024-01-01' };
            const b = { ...baseFilter, dateFrom: '2024-01-02' };
            expect(areFiltersEqual(a, b)).toBe(false);
        });

        it('should return false for filters with different array values', () => {
            const a = { ...baseFilter, level: ['ERROR'] };
            const b = { ...baseFilter, level: ['WARNING'] };
            expect(areFiltersEqual(a, b)).toBe(false);
        });

        it('should return false for filters with different boolean values', () => {
            const a = { ...baseFilter, sqlQueryEnabled: false };
            const b = { ...baseFilter, sqlQueryEnabled: true };
            expect(areFiltersEqual(a, b)).toBe(false);
        });

        it('should return true when both filters are null/undefined', () => {
            expect(areFiltersEqual(null as unknown as FilterState, null as unknown as FilterState)).toBe(true);
        });
    });

    describe('formatBytes', () => {
        it('should format 0 bytes', () => {
            expect(formatBytes(0)).toBe('0 Bytes');
        });

        it('should format bytes (< 1KB)', () => {
            expect(formatBytes(512)).toBe('512 Bytes');
        });

        it('should format kilobytes', () => {
            expect(formatBytes(1024)).toBe('1 KB');
            expect(formatBytes(1536)).toBe('1.5 KB');
        });

        it('should format megabytes', () => {
            expect(formatBytes(1048576)).toBe('1 MB');
            expect(formatBytes(1572864)).toBe('1.5 MB');
        });

        it('should format gigabytes', () => {
            expect(formatBytes(1073741824)).toBe('1 GB');
        });

        it('should respect decimal places parameter', () => {
            expect(formatBytes(1536, 0)).toBe('2 KB');
            expect(formatBytes(1536, 1)).toBe('1.5 KB');
            expect(formatBytes(1536, 3)).toBe('1.5 KB');
        });

        it('should handle negative decimal places', () => {
            expect(formatBytes(1536, -1)).toBe('2 KB');
        });
    });

    describe('parseFilterDateTime', () => {
        it('should parse valid date and time strings', () => {
            const result = parseFilterDateTime('2024-01-15', '10:30:00');
            expect(result).toBe(new Date('2024-01-15T10:30:00Z').getTime());
        });

        it('should parse time with milliseconds', () => {
            const result = parseFilterDateTime('2024-01-15', '10:30:00.123');
            expect(result).toBe(new Date('2024-01-15T10:30:00.123Z').getTime());
        });

        it('should return null for empty date', () => {
            expect(parseFilterDateTime('', '10:30:00')).toBeNull();
        });

        it('should return null for empty time', () => {
            expect(parseFilterDateTime('2024-01-15', '')).toBeNull();
        });

        it('should return null for both empty', () => {
            expect(parseFilterDateTime('', '')).toBeNull();
        });

        it('should return null for invalid date format', () => {
            expect(parseFilterDateTime('invalid-date', '10:30:00')).toBeNull();
        });

        it('should return null for invalid time format', () => {
            expect(parseFilterDateTime('2024-01-15', 'invalid-time')).toBeNull();
        });

        it('should handle edge case dates', () => {
            // Start of Unix epoch
            expect(parseFilterDateTime('1970-01-01', '00:00:00')).toBe(0);

            // End of year
            const result = parseFilterDateTime('2024-12-31', '23:59:59');
            expect(result).toBe(new Date('2024-12-31T23:59:59Z').getTime());
        });
    });
});
