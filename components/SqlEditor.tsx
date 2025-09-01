import React from 'react';

interface SqlEditorProps {
    value: string;
    onChange: (value: string) => void;
}

const highlightSql = (sql: string): string => {
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
            `(?<comment>--[^\r\n]*|#[^\r\n]*)`,
            `(?<string>'[^']*'|"[^"]*")`,
            `(?<keyword>\\b(${keywordList.join('|')})\\b)`,
            `(?<number>\\b\\d+(\\.\\d+)?\\b)`,
            `(?<operator>[=<>!+\\-*/;(),.])`
        ].join('|'),
        'gi'
    );

    const escapeHtml = (unsafe: string) => unsafe.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

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

    return sql.replace(regex, (match, ...args) => {
        const groups = args[args.length - 1];
        const type = Object.keys(groups).find(key => groups[key] !== undefined) || 'default';
        return `<span class="${getTokenClassName(type)}">${escapeHtml(match)}</span>`;
    });
};

export const SqlEditor: React.FC<SqlEditorProps> = ({ value, onChange }) => {
    const textareaRef = React.useRef<HTMLTextAreaElement>(null);
    const highlightRef = React.useRef<HTMLPreElement>(null);

    const handleScroll = () => {
        if (textareaRef.current && highlightRef.current) {
            highlightRef.current.scrollTop = textareaRef.current.scrollTop;
            highlightRef.current.scrollLeft = textareaRef.current.scrollLeft;
        }
    };
    
    const highlightedValue = React.useMemo(() => highlightSql(value), [value]);

    return (
        <div className="relative w-full h-40 font-mono text-sm border border-gray-300 dark:border-gray-600 rounded-lg overflow-hidden">
            <textarea
                ref={textareaRef}
                value={value}
                onChange={(e) => onChange(e.target.value)}
                onScroll={handleScroll}
                className="absolute inset-0 z-10 w-full h-full p-2.5 resize-none bg-transparent text-transparent caret-black dark:caret-white outline-none"
                autoCapitalize="off"
                autoComplete="off"
                autoCorrect="off"
                spellCheck="false"
            />
            <pre
                ref={highlightRef}
                aria-hidden="true"
                className="absolute inset-0 w-full h-full p-2.5 m-0 pointer-events-none overflow-auto bg-gray-50 dark:bg-gray-800/50"
            >
                <code dangerouslySetInnerHTML={{ __html: highlightedValue + '\n' }} />
            </pre>
        </div>
    );
};