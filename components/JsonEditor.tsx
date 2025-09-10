import React from 'react';

interface JsonEditorProps {
    value: string;
    onChange: (value: string) => void;
    readOnly?: boolean;
}

const getTokenClassName = (type: string): string => {
    switch (type) {
        case 'key': return 'text-purple-600 dark:text-purple-400';
        case 'string': return 'text-green-700 dark:text-green-400';
        case 'number': return 'text-amber-600 dark:text-amber-500';
        case 'boolean': return 'text-sky-600 dark:text-sky-400';
        case 'null': return 'text-gray-500 dark:text-gray-400';
        case 'punctuation': return 'text-gray-600 dark:text-gray-300';
        default: return 'text-gray-800 dark:text-gray-200';
    }
};

const escapeHtml = (unsafe: string) => unsafe.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

const highlightJson = (jsonString: string): string => {
    const regex = new RegExp(
        [
            `(?<string>"(\\u[a-zA-Z0-9]{4}|[^"\\\\])*")`, // String
            `(?<key>"(\\u[a-zA-Z0-9]{4}|[^"\\\\])*"\\s*:)`, // Key (string followed by colon)
            `(?<number>\\b-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?\\b)`, // Number
            `(?<boolean>\\btrue\\b|\\bfalse\\b)`, // Boolean
            `(?<null>\\bnull\\b)`, // Null
            `(?<punctuation>[\\[\\]\\{\\},:])`, // Punctuation
        ].join('|'),
        'g'
    );

    let result = '';
    let lastIndex = 0;
    
    for (const match of jsonString.matchAll(regex)) {
        if (match.index === undefined) continue;

        // Add plain text (whitespace) between matches
        if (match.index > lastIndex) {
            result += escapeHtml(jsonString.substring(lastIndex, match.index));
        }
        
        const [token] = match;
        const groups = match.groups || {};
        const type = Object.keys(groups).find(key => groups[key] !== undefined) || 'default';

        result += `<span class="${getTokenClassName(type)}">${escapeHtml(token)}</span>`;
        
        lastIndex = match.index + token.length;
    }

    // Add any remaining text
    if (lastIndex < jsonString.length) {
        result += escapeHtml(jsonString.substring(lastIndex));
    }
    return result;
};


export const JsonEditor: React.FC<JsonEditorProps> = ({ value, onChange, readOnly = false }) => {
    const textareaRef = React.useRef<HTMLTextAreaElement>(null);
    const highlightRef = React.useRef<HTMLPreElement>(null);

    const handleScroll = () => {
        if (textareaRef.current && highlightRef.current) {
            highlightRef.current.scrollTop = textareaRef.current.scrollTop;
            highlightRef.current.scrollLeft = textareaRef.current.scrollLeft;
        }
    };
    
    const highlightedValue = React.useMemo(() => highlightJson(value), [value]);

    return (
        <div className="relative w-full h-full font-mono text-sm border border-gray-300 dark:border-gray-600 rounded-lg overflow-hidden bg-gray-50 dark:bg-gray-800/50">
            <textarea
                ref={textareaRef}
                value={value}
                onChange={(e) => onChange(e.target.value)}
                onScroll={handleScroll}
                readOnly={readOnly}
                className="absolute inset-0 z-10 w-full h-full p-2.5 resize-none bg-transparent text-transparent caret-black dark:caret-white outline-none"
                autoCapitalize="off"
                autoComplete="off"
                autoCorrect="off"
                spellCheck="false"
            />
            <pre
                ref={highlightRef}
                aria-hidden="true"
                className="absolute inset-0 w-full h-full p-2.5 m-0 pointer-events-none overflow-auto"
            >
                <code dangerouslySetInnerHTML={{ __html: highlightedValue + '\n' }} />
            </pre>
        </div>
    );
};