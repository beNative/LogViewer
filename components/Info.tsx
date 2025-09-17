
import React from 'react';
import { Icon } from './icons/index.tsx';
import { IconSet } from '../types.ts';

const MARKDOWN_FILES = [
    { name: 'README.md', title: 'README' },
    { name: 'FUNCTIONAL_MANUAL.md', title: 'Functional Manual' },
    { name: 'TECHNICAL_MANUAL.md', title: 'Technical Manual' },
    { name: 'VERSION_LOG.md', title: 'Version Log' },
];

const parseMarkdown = (markdown: string): string => {
    if (!markdown) return '';

    let html = markdown
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');

    // Code blocks (```...```)
    html = html.replace(/```(bash|typescript|javascript|json|html|css|shell|sh|js|ts)?\n([\s\S]*?)```/g, 
        (match, lang, code) => `<pre class="bg-gray-100 dark:bg-gray-900 rounded-lg p-4 my-4 overflow-x-auto font-mono text-sm"><code class="language-${lang || ''}">${code.replace(/</g, '&lt;').replace(/>/g, '&gt;')}</code></pre>`);

    // Headings (order is important)
    html = html.replace(/^#### (.*$)/gim, '<h4 class="text-lg font-bold mt-5 mb-1 text-gray-800 dark:text-gray-200">$1</h4>');
    html = html.replace(/^### (.*$)/gim, '<h3 class="text-xl font-bold mt-6 mb-2 border-b border-gray-200 dark:border-gray-700 pb-1 text-gray-800 dark:text-gray-200">$1</h3>');
    html = html.replace(/^## (.*$)/gim, '<h2 class="text-2xl font-bold mt-8 mb-3 border-b border-gray-200 dark:border-gray-700 pb-2 text-gray-900 dark:text-white">$1</h2>');
    html = html.replace(/^# (.*$)/gim, '<h1 class="text-3xl font-bold mt-4 mb-4 text-gray-900 dark:text-white">$1</h1>');

    // Blockquotes
    html = html.replace(/^\> (.*$)/gim, '<blockquote class="border-l-4 border-gray-300 dark:border-gray-600 pl-4 italic my-4 text-gray-600 dark:text-gray-400">$1</blockquote>');

    // Lists (NEW multi-pass, inside-out logic)
    for (let i = 4; i >= 0; i--) {
        const indent = ' '.repeat(i * 4);
        // Regex to find a list item and its multi-line, indented content.
        // It stops when it sees a new list item at the same level, a double newline, or the end of the string.
        const itemRegex = new RegExp(`^${indent}(\\*|-|\\d+\\.)\\s+([\\s\\S]*?)(?=\\n^${indent}(?:\\*|-|\\d+\\.)|\\n{2,}|$)`, 'gm');
        html = html.replace(itemRegex, (match, marker, content) => {
            // The content might contain already parsed sub-lists or simple text that needs to be dedented.
            const dedentRegex = new RegExp(`^${indent} {2,4}`, 'gm');
            const dedentedContent = content.replace(dedentRegex, '');
            return `${indent}<li>${dedentedContent.trim()}</li>`;
        });
        
        // Regex to wrap a block of <li>s at the current indentation level into a <ul> or <ol>.
        const listBlockRegex = new RegExp(`^(${indent}<li>[\\s\\S]*?</li>\n?)+`, 'gm');
        html = html.replace(listBlockRegex, (match) => {
            const dedentedBlock = match.replace(new RegExp(`^${indent}`, 'gm'), '');
            const listType = dedentedBlock.match(/^\s*<li>\s*\d+\./) ? 'ol' : 'ul';
            const classes = i === 0
                ? `list-${listType === 'ol' ? 'decimal' : 'disc'} list-inside pl-4 my-4 space-y-1`
                : `list-${listType === 'ol' ? 'decimal' : 'disc'} list-inside pl-4 space-y-1`;
            return `${indent}<${listType} class="${classes}">${dedentedBlock}</${listType}>`;
        });
    }


    // Inline elements - order is important!
    html = html.replace(/`([^`]+)`/g, '<code class="bg-gray-200 dark:bg-gray-700 rounded px-1.5 py-1 font-mono text-sm text-sky-600 dark:text-sky-400 mx-0.5">$1</code>');
    html = html.replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer" class="text-sky-600 dark:text-sky-400 hover:underline">$1</a>');
    html = html.replace(/(?<!\w)\*\*([\s\S]+?)\*\*(?!\w)/g, '<strong class="font-bold text-gray-800 dark:text-gray-200">$1</strong>');
    html = html.replace(/(?<!\w)__([\s\S]+?)__(?!\w)/g, '<strong class="font-bold text-gray-800 dark:text-gray-200">$1</strong>');
    html = html.replace(/(?<!\w)\*([\s\S]+?)\*(?!\w)/g, '<em class="italic">$1</em>');
    html = html.replace(/(?<!\w)_([\s\S]+?)_(?!\w)/g, '<em class="italic">$1</em>');
    html = html.replace(/~~([\s\S]+?)~~/g, '<del class="line-through">$1</del>');

    // HR
    html = html.replace(/^\s*-{3,}\s*$/gm, '<hr class="my-6 border-gray-200 dark:border-gray-700"/>');
    
    // Paragraphs
    html = html.split(/\n\n+/).map(p => {
        if (!p.trim() || p.trim().startsWith('<')) return p;
        return `<p class="my-4 leading-relaxed">${p.trim().replace(/\n/g, '<br/>')}</p>`;
    }).join('');

    // Cleanup paragraphs around block elements
    html = html.replace(/<p>(\s*)<(ul|ol|li|blockquote|hr|pre|h[1-6])/g, '<$2');
    html = html.replace(/<\/(ul|ol|li|blockquote|hr|pre|h[1-6])>(\s*)<\/p>/g, '</$1>');
    
    // Cleanup <br> tags before lists inside other elements (like <li>)
    html = html.replace(/<br\s*\/?>(\s*<(ul|ol))/g, '$1');


    return html;
}

interface InfoProps {
    iconSet: IconSet;
    onOpenAboutDialog: () => void;
}

export const Info: React.FC<InfoProps> = ({ iconSet, onOpenAboutDialog }) => {
    const [activeTabIndex, setActiveTabIndex] = React.useState(0);
    const [markdownContent, setMarkdownContent] = React.useState<Record<string, string>>({});
    const [loading, setLoading] = React.useState(true);
    const [error, setError] = React.useState<string | null>(null);

    React.useEffect(() => {
        const fetchFiles = async () => {
            try {
                setLoading(true);
                const contents: Record<string, string> = {};
                const isElectron = !!window.electronAPI?.getMarkdownContent;

                for (const file of MARKDOWN_FILES) {
                    let fileContent: string;
                    if (isElectron) {
                        const result = await window.electronAPI.getMarkdownContent(file.name);
                        if (result.success && typeof result.content === 'string') {
                            fileContent = result.content;
                        } else {
                            throw new Error(`Could not load ${file.name} via Electron: ${result.error || 'Unknown error'}`);
                        }
                    } else {
                        // Fallback for web version
                        const response = await fetch(`./${file.name}`);
                        if (!response.ok) {
                            throw new Error(`Could not fetch ${file.name} (status: ${response.status})`);
                        }
                        fileContent = await response.text();
                    }
                    contents[file.name] = fileContent;
                }
                setMarkdownContent(contents);
                setError(null);
            } catch (err) {
                setError(err instanceof Error ? err.message : 'Failed to load info files.');
            } finally {
                setLoading(false);
            }
        };
        fetchFiles();
    }, []);

    const activeFile = MARKDOWN_FILES[activeTabIndex];
    const content = markdownContent[activeFile?.name] || '';
    const parsedHtml = React.useMemo(() => parseMarkdown(content), [content]);

    return (
        <div className="flex h-full bg-white dark:bg-gray-900 overflow-hidden">
            <aside className="w-64 flex-shrink-0 bg-gray-50 dark:bg-gray-800 border-r border-gray-200 dark:border-gray-700 flex flex-col">
                <div className="p-4 flex-shrink-0 border-b border-gray-200 dark:border-gray-700">
                    <h3 className="text-lg font-semibold text-gray-900 dark:text-white flex items-center gap-2">
                        <Icon name="BookOpen" iconSet={iconSet} className="w-6 h-6 text-sky-600 dark:text-sky-400" />
                        Info & Manuals
                    </h3>
                </div>
                <nav className="flex-1 p-2 space-y-1">
                    {MARKDOWN_FILES.map((file, index) => (
                        <button
                            key={file.name}
                            onClick={() => setActiveTabIndex(index)}
                            className={`w-full text-left px-3 py-2.5 text-sm font-medium rounded-md transition-colors duration-150 ${
                                activeTabIndex === index
                                ? 'bg-sky-100 dark:bg-sky-900/50 text-sky-700 dark:text-sky-300'
                                : 'text-gray-600 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700/50 hover:text-gray-900 dark:hover:text-gray-200'
                            }`}
                        >
                            {file.title}
                        </button>
                    ))}
                </nav>
                <div className="p-2 mt-auto border-t border-gray-200 dark:border-gray-700">
                    <button
                        onClick={onOpenAboutDialog}
                        className="w-full text-left px-3 py-2.5 text-sm font-medium rounded-md text-gray-600 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700/50 hover:text-gray-900 dark:hover:text-gray-200 flex items-center gap-3"
                    >
                        <Icon name="InformationCircle" iconSet={iconSet} className="w-5 h-5" />
                        About Log Analyser
                    </button>
                </div>
            </aside>
            <main className="flex-1 overflow-y-auto">
                <div className="max-w-4xl mx-auto p-6 sm:p-8 lg:p-10">
                    {loading && <p className="text-gray-500">Loading document...</p>}
                    {error && <div className="bg-red-100 text-red-700 p-4 rounded-lg">{error}</div>}
                    {!loading && !error && (
                        <div
                            dangerouslySetInnerHTML={{ __html: parsedHtml }}
                        />
                    )}
                </div>
            </main>
        </div>
    );
};
