import React from 'react';


interface SqlSyntaxHighlighterProps {
    sql: string;
}

const getTokenClassName = (type: string): string => {
    switch (type) {
        case 'keyword': return 'text-sky-600 dark:text-sky-400 font-bold';
        case 'string': return 'text-green-700 dark:text-green-400';
        case 'number': return 'text-fuchsia-600 dark:text-fuchsia-400';
        case 'comment': return 'text-gray-400 dark:text-gray-500 italic';
        case 'operator': return 'text-gray-600 dark:text-gray-300';
        default: return 'text-gray-900 dark:text-white';
    }
};

export const SqlSyntaxHighlighter: React.FC<SqlSyntaxHighlighterProps> = ({ sql }) => {
    const highlightedNodes = React.useMemo(() => {
        // A list of common SQL keywords, with multi-word keywords first for correct matching.
        const keywordList = [
            'GROUP BY', 'ORDER BY', 'LEFT JOIN', 'RIGHT JOIN', 'INNER JOIN', 'OUTER JOIN',
            'SELECT', 'FROM', 'WHERE', 'JOIN', 'ON', 'ASC', 'DESC', 'LIMIT', 'OFFSET', 'INSERT', 'INTO', 
            'VALUES', 'UPDATE', 'SET', 'DELETE', 'CREATE', 'TEMPORARY', 'TABLE', 'DROP', 'ALTER', 'ADD', 
            'CONSTRAINT', 'PRIMARY', 'KEY', 'FOREIGN', 'REFERENCES', 'DISTINCT', 'AS', 'AND', 'OR', 
            'NOT', 'IN', 'LIKE', 'IS', 'NULL', 'IF', 'EXISTS', 'CASE', 'WHEN', 'THEN', 'ELSE', 'END', 
            'ROUND', 'MOD', 'COUNT', 'MAX', 'MIN', 'AVG', 'SUM', 'CAST', 'CONVERT'
        ];
        
        const regex = new RegExp(
            [
                `(?<comment>--[^\r\n]*|#[^\r\n]*)`, // Comments
                `(?<string>'[^']*'|"[^"]*")`, // Strings
                `(?<keyword>\\b(${keywordList.join('|')})\\b)`, // Keywords
                `(?<number>\\b\\d+(\\.\\d+)?\\b)`, // Numbers
                `(?<operator>[=<>!+\\-*/;(),.])` // Operators
            ].join('|'),
            'gi'
        );

        const nodes: React.ReactNode[] = [];
        let lastIndex = 0;
        
        for (const match of sql.matchAll(regex)) {
            if (match.index === undefined) continue;

            // Add the plain text between matches
            if (match.index > lastIndex) {
                nodes.push(sql.substring(lastIndex, match.index));
            }
            
            const [token] = match;
            const groups = match.groups || {};
            // Find which named capture group matched
            const type = Object.keys(groups).find(key => groups[key] !== undefined) || 'default';

            nodes.push(
                <span key={match.index} className={getTokenClassName(type)}>
                    {token}
                </span>
            );
            
            lastIndex = match.index + token.length;
        }

        // Add any remaining text after the last match
        if (lastIndex < sql.length) {
            nodes.push(sql.substring(lastIndex));
        }

        return nodes;
    }, [sql]);

    return (
        <div className="bg-gray-100 dark:bg-gray-900 rounded-lg p-3 ring-1 ring-gray-200 dark:ring-gray-700 overflow-x-auto">
            <pre className="text-sm whitespace-pre-wrap break-all font-mono">
                <code>{highlightedNodes.map((node, i) => <React.Fragment key={i}>{node}</React.Fragment>)}</code>
            </pre>
        </div>
    );
};