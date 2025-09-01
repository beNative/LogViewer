import React from 'react';

interface JsonSyntaxHighlighterProps {
    jsonString: string;
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

export const JsonSyntaxHighlighter: React.FC<JsonSyntaxHighlighterProps> = ({ jsonString }) => {
    const highlightedNodes = React.useMemo(() => {
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

        const nodes: React.ReactNode[] = [];
        let lastIndex = 0;
        
        for (const match of jsonString.matchAll(regex)) {
             if (match.index === undefined) continue;

            // Add plain text (whitespace) between matches
            if (match.index > lastIndex) {
                nodes.push(jsonString.substring(lastIndex, match.index));
            }
            
            const [token] = match;
            const groups = match.groups || {};
            const type = Object.keys(groups).find(key => groups[key] !== undefined) || 'default';

            nodes.push(
                <span key={match.index} className={getTokenClassName(type)}>
                    {token}
                </span>
            );
            
            lastIndex = match.index + token.length;
        }

        // Add any remaining text
        if (lastIndex < jsonString.length) {
            nodes.push(jsonString.substring(lastIndex));
        }

        return nodes;

    }, [jsonString]);

    return (
        <div className="bg-gray-100 dark:bg-gray-900/70 p-4 rounded-md overflow-x-auto">
            <pre className="text-sm whitespace-pre-wrap break-all font-mono">
                <code>{highlightedNodes.map((node, i) => <React.Fragment key={i}>{node}</React.Fragment>)}</code>
            </pre>
        </div>
    );
};
