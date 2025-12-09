import { FilterState, ColumnKey } from './types.ts';

// A simple utility to escape characters that have special meaning in HTML.
const escapeHtml = (unsafe: string): string => {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

export const COLUMN_DEFINITIONS: { key: ColumnKey; label: string }[] = [
    { key: 'time', label: 'Timestamp' },
    { key: 'level', label: 'Level' },
    { key: 'sndrtype', label: 'Sender Type' },
    { key: 'sndrname', label: 'Sender Name' },
    { key: 'fileName', label: 'Filename' },
    { key: 'msg', label: 'Message' },
];

/**
 * Wraps search terms found within a block of text with <mark> tags for highlighting.
 * The matching is case-insensitive.
 * @param text The text to search within.
 * @param terms The search terms to highlight.
 * @param theme The current theme to apply the correct highlight color.
 * @returns An HTML string with matching terms highlighted.
 */
export const highlightText = (text: string, terms: string[], theme: 'light' | 'dark'): string => {
    if (!text || !terms || terms.length === 0) {
        return escapeHtml(text);
    }

    // Filter out empty terms and escape special regex characters from each term.
    const validTerms = terms.filter(Boolean).map(term =>
        term.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
    );

    if (validTerms.length === 0) {
        return escapeHtml(text);
    }

    // Create a single regex to find all terms at once, case-insensitively.
    const regex = new RegExp(`(${validTerms.join('|')})`, 'gi');

    // Split the text by the regex. The separators (the matched terms) will be
    // included in the resulting array.
    const parts = text.split(regex);

    // This class only sets the background color. The text color is inherited from the parent.
    const highlightClass = theme === 'dark'
        ? 'bg-yellow-500/40'
        : 'bg-yellow-300/70';

    // Rebuild the string, wrapping the matched terms (which will be at odd indices)
    // in <mark> tags and escaping the other parts.
    return parts.map((part, index) => {
        // If the part is a matched term (at an odd index)
        if (index % 2 === 1) {
            return `<mark class="${highlightClass} rounded px-0.5">${escapeHtml(part)}</mark>`;
        }
        // Otherwise, it's a normal part of the text that needs escaping.
        return escapeHtml(part);
    }).join('');
};

/**
 * Compares two arrays of strings for equality, ignoring the order of elements.
 * Uses O(n) Set-based approach instead of O(n log n) sorting.
 * @param a The first array.
 * @param b The second array.
 * @returns True if the arrays contain the same elements, false otherwise.
 */
export const areArraysEqualUnordered = (a: string[], b: string[]): boolean => {
    if (a.length !== b.length) return false;
    if (a.length === 0) return true;

    // Use Set for O(n) lookup instead of O(n log n) sort
    const setA = new Set(a);
    return b.every(item => setA.has(item));
};

/**
 * Performs a deep equality check on two FilterState objects.
 * It specifically handles array properties by comparing their contents
 * without regard to order.
 * @param a The first FilterState object.
 * @param b The second FilterState object.
 * @returns True if the objects are functionally equivalent, false otherwise.
 */
export const areFiltersEqual = (a: FilterState, b: FilterState): boolean => {
    if (!a || !b) return a === b;

    const keysA = Object.keys(a) as (keyof FilterState)[];
    const keysB = Object.keys(b) as (keyof FilterState)[];

    if (keysA.length !== keysB.length) return false;

    for (const key of keysA) {
        const valA = a[key];
        const valB = b[key];

        if (Array.isArray(valA) && Array.isArray(valB)) {
            if (!areArraysEqualUnordered(valA, valB)) return false;
        } else if (valA !== valB) {
            return false;
        }
    }

    return true;
};

export const formatBytes = (bytes: number, decimals = 2) => {
    if (bytes === 0) return '0 Bytes';
    const k = 1024;
    const dm = decimals < 0 ? 0 : decimals;
    const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
};