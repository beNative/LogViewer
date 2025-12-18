import { LogMessageContent, GridData } from './types.ts';

/**
 * Parses a log message to extract key-value pairs separated by semicolons.
 * Uses heuristics to detect patterns like "key=value; key2=value2".
 * @param msg - The log message to parse
 * @returns Object with prefix text and parsed pairs, or null if not a K/V message
 */
const parseKeyValueMessage = (msg: string): { prefix: string | null; pairs: { key: string; value: string }[] } | null => {
    const parts = msg.split(';');
    // Heuristic: Message is likely K/V if it has an equals sign.
    if (parts.length > 0 && msg.includes('=')) {
        const pairs: { key: string; value: string }[] = [];
        let firstKvPartIndex = -1; // The index of the first part (from the right) that contains a K/V pair.

        for (let i = parts.length - 1; i >= 0; i--) {
            const part = parts[i].trim();
            if (!part) continue;

            const eqIndex = part.lastIndexOf('=');
            if (eqIndex <= 0) { // No '=', or starts with '=', invalid.
                break;
            }

            const potentialValue = part.substring(eqIndex + 1);
            const keyPart = part.substring(0, eqIndex);

            // A valid key is the last "word" before the '='.
            // Allowed characters in key: alphanum, underscore, dot, hyphen, and square brackets.
            const keyMatch = keyPart.match(/([a-zA-Z0-9_.\[\]-]+)$/);

            if (keyMatch) {
                const key = keyMatch[0];
                const prefixInPart = keyPart.substring(0, keyMatch.index).trim();

                // Heuristic: If a prefix exists within a part, it must be empty or end with a colon.
                // Otherwise, it's likely part of an unstructured value (like a URL query string) and not a real prefix.
                if (prefixInPart && !prefixInPart.endsWith(':')) {
                    break;
                }

                pairs.unshift({ key, value: potentialValue.trim() });
                firstKvPartIndex = i;

                // If we found a prefix within this part, it's the beginning of the K/V section, so we stop.
                if (prefixInPart) {
                    break;
                }
            } else {
                // Couldn't extract a valid key-like token before the '=', so this isn't a K/V pair.
                break;
            }
        }

        if (pairs.length > 0) {
            // Reconstruct the prefix from all parts before the first K/V part, plus any prefix inside that first part.
            const firstPartWithKv = parts[firstKvPartIndex];
            const eqIndex = firstPartWithKv.lastIndexOf('=');
            const keyPart = firstPartWithKv.substring(0, eqIndex);
            const keyMatch = keyPart.match(/([a-zA-Z0-9_.\[\]-]+)$/);

            const prefixInFirstPart = keyMatch ? keyPart.substring(0, keyMatch.index) : '';

            const prefix = [...parts.slice(0, firstKvPartIndex), prefixInFirstPart].join(';').trim();

            return { prefix: prefix || null, pairs };
        }
    }

    return null;
};


const SQL_KEYWORDS = [
    'SELECT', 'INSERT', 'UPDATE', 'DELETE', 'FROM', 'WHERE', 'GROUP BY', 'ORDER BY',
    'CREATE', 'TABLE', 'DROP', 'ALTER', 'JOIN', 'LIMIT', 'SET', 'VALUES', 'INTO', 'LEFT JOIN', 'RIGHT JOIN'
];

/**
 * Parses a log message to detect SQL statements.
 * Uses multiple heuristics including keyword detection and pattern matching.
 * @param msg - The log message to parse
 * @returns Object with optional prefix, SQL statement, and result, or null if not SQL
 */
const parseSqlMessage = (msg: string): { prefix: string | null; sql: string; result: string | null } | null => {
    const upperMsg = msg.toUpperCase();

    // High-confidence patterns
    const isSelect = upperMsg.includes('SELECT') && upperMsg.includes('FROM');
    const isUpdate = upperMsg.includes('UPDATE') && upperMsg.includes('SET');
    const isInsert = upperMsg.includes('INSERT INTO') && upperMsg.includes('VALUES');
    const isDelete = upperMsg.includes('DELETE FROM') && upperMsg.includes('WHERE');

    // Lower-confidence heuristic
    let keywordCount = 0;
    for (const keyword of SQL_KEYWORDS) {
        if (upperMsg.includes(keyword)) {
            keywordCount++;
        }
    }
    const hasResultArrow = msg.includes('->');

    // A message is likely SQL if it matches a strong pattern, or has several keywords and other hints.
    const isSql = isSelect || isUpdate || isInsert || isDelete ||
        (keywordCount >= 4 && upperMsg.includes('WHERE')) || // multiple keywords + WHERE is a good sign
        (keywordCount >= 2 && msg.includes(';')) || // semicolon is a strong hint
        (keywordCount >= 2 && hasResultArrow);


    if (!isSql) {
        return null;
    }

    let mainPart = msg;
    let result: string | null = null;

    // Using lastIndexOf to avoid issues with `->` inside the query itself.
    const resultSeparatorIndex = msg.lastIndexOf('\n ->');
    if (resultSeparatorIndex !== -1) {
        mainPart = msg.substring(0, resultSeparatorIndex);
        // The separator is `\n ->` which is 4 chars.
        result = msg.substring(resultSeparatorIndex + 4).trim();
    }

    let prefix: string | null = null;
    let sql = mainPart;

    const keywordRegex = new RegExp(`\\b(${SQL_KEYWORDS.join('|')})\\b`, 'i');
    const match = mainPart.match(keywordRegex);

    if (match && typeof match.index === 'number') {
        const firstKeywordIndex = match.index;

        if (firstKeywordIndex > 0 && mainPart.substring(0, firstKeywordIndex).trim().length > 0) {
            prefix = mainPart.substring(0, firstKeywordIndex).trim();
            sql = mainPart.substring(firstKeywordIndex).trim();
        }
    }

    return { prefix, sql, result };
};

/**
 * Parses a log message containing space-delimited tabular data.
 * Detects patterns with header rows containing column names separated by whitespace.
 * @param msg - The log message to parse
 * @returns Object with prefix and grid data (headers + rows), or null if not a table
 */
const parseSpaceDelimitedTable = (msg: string): { prefix: string | null, data: GridData } | null => {
    const lines = msg.split('\n').filter(line => line.trim().length > 0);
    if (lines.length < 2) return null;

    const headerLine = lines[0];
    const dataLines = lines.slice(1);

    // Heuristic 1: This format seems to always have a colon in the header line.
    const lastColonIndex = headerLine.lastIndexOf(':');
    if (lastColonIndex === -1 || lastColonIndex === headerLine.length - 1) {
        return null;
    }

    // Heuristic 2: The first word on the first data line must be a number.
    const firstDataWord = dataLines[0].trim().split(/\s+/)[0];
    if (!firstDataWord || isNaN(parseInt(firstDataWord, 10))) {
        return null;
    }

    // Heuristic 3: The first word of the header part should NOT be a number.
    const potentialHeaderString = headerLine.substring(lastColonIndex + 1).trim();
    const firstHeaderWord = potentialHeaderString.split(/\s+/)[0];
    if (!firstHeaderWord || !isNaN(parseInt(firstHeaderWord, 10))) {
        return null;
    }

    // If all heuristics pass, we are confident this is the target format.
    const prefix = headerLine.substring(0, lastColonIndex + 1).trim();
    const headers = potentialHeaderString.split(/\s+/).filter(Boolean);

    if (headers.length < 3) return null;

    const rows: string[][] = [];
    // This regex specifically looks for a date, then a time. If it fails, it looks for any non-space characters.
    const valueRegex = /(\d{1,2}\/\d{1,2}\/\d{4}\s+\d{2}:\d{2}:\d{2})|(\S+)/g;

    for (const line of dataLines) {
        const matches = [...line.trim().matchAll(valueRegex)];
        if (matches.length === 0) continue;

        const rowValues = matches.map(match => match[0]);

        while (rowValues.length < headers.length) {
            rowValues.push('');
        }
        rows.push(rowValues.slice(0, headers.length));
    }

    if (rows.length === 0) return null;

    return { prefix, data: { headers, rows } };
};


const parseGridViewMessage = (msg: string): { prefix: string | null, data: GridData } | null => {
    // ATTEMPT 1: Try the new space-delimited parser first, as it's more specific.
    const ssvResult = parseSpaceDelimitedTable(msg);
    if (ssvResult) {
        return ssvResult;
    }

    // ATTEMPT 2: Fallback to the original fixed-width parser.
    const lines = msg.split('\n').filter(line => line.trim().length > 0);
    if (lines.length < 2) return null;

    const headerLine = lines[0];

    // Find all word-like sequences on the header line to act as column boundaries
    const columnBoundaries: { name: string; start: number }[] = [];
    const headerRegex = /\S+/g;
    let match;
    while ((match = headerRegex.exec(headerLine)) !== null) {
        columnBoundaries.push({ name: match[0], start: match.index });
    }

    if (columnBoundaries.length < 2) return null;

    // Heuristic check: The first column of the header must not be a number itself.
    const firstHeaderName = columnBoundaries[0].name;
    if (!isNaN(parseFloat(firstHeaderName))) return null;

    let firstHeaderIndex = 0;
    let prefix: string | null = null;

    // Check for a prefix, e.g., "functionName: after:"
    if (firstHeaderName.endsWith(':')) {
        let potentialPrefixEnd = 1;
        if (columnBoundaries.length > 1 && columnBoundaries[1].name.toLowerCase() === 'after:') {
            potentialPrefixEnd = 2;
        }
        firstHeaderIndex = potentialPrefixEnd;
        const lastPrefixPart = columnBoundaries[potentialPrefixEnd - 1];
        prefix = headerLine.substring(0, lastPrefixPart.start + lastPrefixPart.name.length).trim();
    }

    const headers = columnBoundaries.slice(firstHeaderIndex).map(b => b.name);
    if (headers.length < 2) return null;

    const headerPositions = columnBoundaries.slice(firstHeaderIndex);

    // Determine the start position of the first *real* header column.
    // This offset is used to align data rows, which don't have the prefix.
    const dataColumnOffset = headerPositions.length > 0 ? headerPositions[0].start : 0;

    const rows: string[][] = [];
    const dataLines = lines.slice(1);

    for (const dataLine of dataLines) {
        // Skip lines that look like they could be part of a header or are just separators
        if (!/^\s*\S/.test(dataLine)) continue;

        const rowValues: string[] = [];
        for (let i = 0; i < headerPositions.length; i++) {
            const currentBoundary = headerPositions[i];
            const nextBoundary = headerPositions[i + 1];

            // Adjust header positions to be relative to the data lines, which don't have the prefix.
            const start = currentBoundary.start - dataColumnOffset;
            const end = nextBoundary ? (nextBoundary.start - dataColumnOffset) : dataLine.length;

            // Slice the data line using the relative positions.
            const value = dataLine.substring(Math.max(0, start), end).trim();
            rowValues.push(value);
        }

        if (rowValues.length > 0 && rowValues.some(v => v !== '')) {
            rows.push(rowValues);
        }
    }

    if (rows.length === 0) return null;

    return { prefix, data: { headers, rows } };
};

/**
 * Main entry point for parsing log messages into structured content.
 * Attempts to detect and parse various formats in order of priority:
 * 1. XML content
 * 2. SQL statements
 * 3. Grid/table data
 * 4. Key-value pairs
 * Falls back to plain text if no structured format is detected.
 * @param msg - The raw log message to parse
 * @returns Structured LogMessageContent with type, optional prefix, and parsed data
 */
export const parseLogMessage = (msg: string): LogMessageContent => {
    if (!msg) {
        return { type: 'text', prefix: null, data: '' };
    }

    // 1. Check for XML first.
    const xmlStartIndex = msg.indexOf('<');
    if (xmlStartIndex !== -1) {
        const potentialXml = msg.substring(xmlStartIndex);
        try {
            const parser = new DOMParser();
            const doc = parser.parseFromString(potentialXml, "application/xml");
            if (doc.getElementsByTagName("parsererror").length === 0) {
                const prefix = xmlStartIndex > 0 ? msg.substring(0, xmlStartIndex) : null;
                return { type: 'xml', prefix, data: potentialXml };
            }
        } catch {
            // DOMParser can throw on severely malformed input in some browsers
            // Fall through to other parsers
        }
    }

    // 2. Check for SQL (moved up)
    const sqlResult = parseSqlMessage(msg);
    if (sqlResult) {
        return {
            type: 'sql',
            prefix: sqlResult.prefix,
            data: { sql: sqlResult.sql, result: sqlResult.result }
        };
    }

    // 3. Check for Grid View
    const gridResult = parseGridViewMessage(msg);
    if (gridResult) {
        return { type: 'grid', prefix: gridResult.prefix, data: gridResult.data };
    }

    // 4. Check for Key-Value.
    const kvResult = parseKeyValueMessage(msg);
    if (kvResult) {
        return { type: 'kv', prefix: kvResult.prefix || null, data: kvResult.pairs };
    }

    // 5. Fallback to plain text.
    return { type: 'text', prefix: null, data: msg };
};
