import { LogMessageContent, GridData } from './types.ts';

const parseKeyValueMessage = (msg: string): { prefix: string; pairs: { key: string; value: string }[] } | null => {
    if (!msg.includes('=')) {
        return null;
    }

    let messageBody = msg;
    let prefix = '';

    const firstEqIndex = msg.indexOf('=');
    if (firstEqIndex > -1) {
        const lastColonBeforeEq = msg.substring(0, firstEqIndex).lastIndexOf(':');
        if (lastColonBeforeEq > -1) {
            prefix = msg.substring(0, lastColonBeforeEq + 1).trim();
            messageBody = msg.substring(lastColonBeforeEq + 1).trim();
        }
    }
    
    const pairs: { key: string; value: string }[] = [];
    const regex = /\b([a-zA-Z0-9_.\[\]]+)=/g;
    const matches = [...messageBody.matchAll(regex)];

    if (matches.length > 0) {
        for (let i = 0; i < matches.length; i++) {
            const currentMatch = matches[i];
            const nextMatch = matches[i + 1];

            const key = currentMatch[1];
            const valueStartIndex = (currentMatch.index ?? 0) + currentMatch[0].length;
            const valueEndIndex = nextMatch ? nextMatch.index : messageBody.length;
            
            let value = messageBody.substring(valueStartIndex, valueEndIndex).trim();
            
            if (value.endsWith(',') || value.endsWith(';')) {
                value = value.slice(0, -1).trim();
            }

            pairs.push({ key, value });
        }
    }

    if (pairs.length >= 2) {
        return { prefix, pairs };
    }

    return null;
};

const SQL_KEYWORDS = [
  'SELECT', 'INSERT', 'UPDATE', 'DELETE', 'FROM', 'WHERE', 'GROUP BY', 'ORDER BY', 
  'CREATE', 'TABLE', 'DROP', 'ALTER', 'JOIN', 'LIMIT', 'SET', 'VALUES', 'INTO', 'LEFT JOIN', 'RIGHT JOIN'
];

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

const parseGridViewMessage = (msg: string): { prefix: string | null, data: GridData } | null => {
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
        
        // FIX: A single row should be a string array, not a string[][].
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

export const parseLogMessage = (msg: string): LogMessageContent => {
    if (!msg) {
        return { type: 'text', prefix: null, data: '' };
    }

    // 1. Check for XML first.
    const xmlStartIndex = msg.indexOf('<');
    if (xmlStartIndex !== -1) {
        const potentialXml = msg.substring(xmlStartIndex);
        const parser = new DOMParser();
        const doc = parser.parseFromString(potentialXml, "application/xml");
        if (doc.getElementsByTagName("parsererror").length === 0) {
            const prefix = xmlStartIndex > 0 ? msg.substring(0, xmlStartIndex) : null;
            return { type: 'xml', prefix, data: potentialXml };
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