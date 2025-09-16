import React from 'react';
import { Icon, IconName } from './icons/index.tsx';
import { IconSet } from '../types.ts';
import { Tooltip } from './Tooltip.tsx';

// Local types for this component
type ElementDebugInfo = {
    identity: {
        tag: string;
        id: string;
        name: string;
        ariaLabel: string;
        testId: string;
        classList: string;
        constructorName: string;
    };
    geometry: {
        x: number;
        y: number;
        width: number;
        height: number;
    };
    styles: {
        zIndex: string;
        position: string;
        display: string;
        overflow: string;
    };
    parents: string[];
};

interface FocusDebuggerProps {
    isVisible: boolean;
    iconSet: IconSet;
}

// Helper to get element details
const getElementInfo = (element: Element | null): ElementDebugInfo | null => {
    if (!element) return null;
    const el = element as HTMLElement;
    const computed = window.getComputedStyle(el);
    const rect = el.getBoundingClientRect();

    const parents: string[] = [];
    let parent = el.parentElement;
    for (let i = 0; i < 5 && parent; i++) {
        const parentTag = parent.tagName.toLowerCase();
        const parentId = parent.id ? `#${parent.id}` : '';
        parents.push(`${parentTag}${parentId}`);
        parent = parent.parentElement;
    }
    if (parent) parents.push('...');

    return {
        identity: {
            tag: el.tagName.toLowerCase(),
            id: el.id || '–',
            name: el.getAttribute('name') || '–',
            ariaLabel: el.getAttribute('aria-label') || el.title || '–',
            testId: el.dataset.testid || '–',
            classList: el.classList.toString() || '–',
            constructorName: el.constructor?.name || '–',
        },
        geometry: {
            x: Math.round(rect.x),
            y: Math.round(rect.y),
            width: Math.round(rect.width),
            height: Math.round(rect.height),
        },
        styles: {
            zIndex: computed.zIndex,
            position: computed.position,
            display: computed.display,
            overflow: computed.overflow,
        },
        parents: parents.reverse(),
    };
};

// Helper to format info for clipboard
const formatInfoForClipboard = (info: ElementDebugInfo | null, title: string): string => {
    if (!info) return `${title}: N/A`;
    return `
--- ${title} ---
[Identity]
Tag: ${info.identity.tag}
ID: ${info.identity.id}
Name: ${info.identity.name}
Class: ${info.identity.classList}
Constructor: ${info.identity.constructorName}
ARIA Label: ${info.identity.ariaLabel}
Test ID: ${info.identity.testId}

[Geometry]
Position: ${info.geometry.x}px, ${info.geometry.y}px
Size: ${info.geometry.width}px × ${info.geometry.height}px

[Computed Style]
z-index: ${info.styles.zIndex}
position: ${info.styles.position}
display: ${info.styles.display}
overflow: ${info.styles.overflow}

[Parent Tree]
${info.parents.join(' < ')}
    `.trim().replace(/^ +/gm, '');
};

const InfoRow: React.FC<{ label: string; value: string | number | null; isCode?: boolean }> = ({ label, value, isCode = false }) => (
    <div className="grid grid-cols-3 gap-2 items-start">
        <strong className="text-gray-400 col-span-1 text-right">{label}:</strong>
        <span className={`text-white col-span-2 break-all ${isCode ? 'font-mono' : ''}`} title={String(value ?? '')}>{String(value ?? '–')}</span>
    </div>
);

const DebugInfoDisplay: React.FC<{ title: string; info: ElementDebugInfo | null; icon: IconName; isFrozen: boolean; iconSet: IconSet }> = ({ title, info, icon, isFrozen, iconSet }) => {
    const handleCopy = () => {
        if (info) {
            navigator.clipboard.writeText(formatInfoForClipboard(info, title));
        }
    };

    return (
        <div className="flex-1 p-3 bg-gray-800/50 rounded-lg min-w-0">
            <div className="flex justify-between items-center mb-2">
                <div className="flex items-center gap-2">
                    <Icon name={icon} iconSet={iconSet} className="w-5 h-5 text-gray-400" />
                    <h4 className="font-bold text-gray-200">{title}</h4>
                </div>
                {isFrozen && info && (
                    <Tooltip content="Copy details">
                        <button onClick={handleCopy} className="p-1 text-gray-400 hover:text-white rounded-md hover:bg-gray-600">
                            <Icon name="ClipboardDocument" iconSet={iconSet} className="w-4 h-4" />
                        </button>
                    </Tooltip>
                )}
            </div>
            {info ? (
                <div className="space-y-3 text-xs">
                    <details open>
                        <summary className="font-semibold text-gray-300 cursor-pointer select-none">Identity</summary>
                        <div className="pl-2 pt-1 mt-1 border-l border-gray-600 space-y-1">
                            <InfoRow label="Tag" value={info.identity.tag} isCode />
                            <InfoRow label="ID" value={info.identity.id} isCode />
                            <InfoRow label="Class" value={info.identity.classList} isCode />
                            <InfoRow label="Instance" value={info.identity.constructorName} isCode />
                            <InfoRow label="ARIA Label" value={info.identity.ariaLabel} />
                        </div>
                    </details>
                    <details>
                        <summary className="font-semibold text-gray-300 cursor-pointer select-none">Geometry</summary>
                         <div className="pl-2 pt-1 mt-1 border-l border-gray-600 space-y-1">
                            <InfoRow label="Position" value={`${info.geometry.x}, ${info.geometry.y}`} isCode />
                            <InfoRow label="Size" value={`${info.geometry.width} × ${info.geometry.height}`} isCode />
                        </div>
                    </details>
                    <details>
                        <summary className="font-semibold text-gray-300 cursor-pointer select-none">Style</summary>
                         <div className="pl-2 pt-1 mt-1 border-l border-gray-600 space-y-1">
                            <InfoRow label="z-index" value={info.styles.zIndex} isCode />
                            <InfoRow label="position" value={info.styles.position} isCode />
                            <InfoRow label="display" value={info.styles.display} isCode />
                            <InfoRow label="overflow" value={info.styles.overflow} isCode />
                        </div>
                    </details>
                     <details>
                        <summary className="font-semibold text-gray-300 cursor-pointer select-none">Parent Tree</summary>
                         <div className="pl-2 pt-1 mt-1 border-l border-gray-600">
                            {/* FIX: The 'isCode' prop is not a valid attribute for a span element. Replaced with 'font-mono' class for monospace styling. */}
                            <span className="text-white break-all font-mono">{info.parents.join(' < ')}</span>
                        </div>
                    </details>
                </div>
            ) : (
                <span className="text-gray-500 text-sm">N/A</span>
            )}
        </div>
    );
};


export const FocusDebugger: React.FC<FocusDebuggerProps> = ({ isVisible, iconSet }) => {
    const [focusedInfo, setFocusedInfo] = React.useState<ElementDebugInfo | null>(null);
    const [hoveredInfo, setHoveredInfo] = React.useState<ElementDebugInfo | null>(null);
    const [isFrozen, setIsFrozen] = React.useState(false);

    React.useEffect(() => {
        if (!isVisible) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            if (e.key === 'F4') {
                e.preventDefault();
                setIsFrozen(prev => !prev);
            }
        };

        document.addEventListener('keydown', handleKeyDown);

        return () => {
            document.removeEventListener('keydown', handleKeyDown);
        };
    }, [isVisible]);

    React.useEffect(() => {
        if (!isVisible) return;

        const handleFocusIn = (e: FocusEvent) => !isFrozen && setFocusedInfo(getElementInfo(e.target as Element));
        const handleFocusOut = () => !isFrozen && setFocusedInfo(null);
        const handleMouseOver = (e: MouseEvent) => {
            if (!isFrozen) {
                // Prevent updating if hovering over the debugger itself
                const target = e.target as Element;
                if (target.closest('.focus-debugger-container')) return;
                setHoveredInfo(getElementInfo(target));
            }
        };
        const handleMouseOut = () => !isFrozen && setHoveredInfo(null);

        document.addEventListener('focusin', handleFocusIn, true);
        document.addEventListener('focusout', handleFocusOut, true);
        document.addEventListener('mouseover', handleMouseOver, true);
        document.addEventListener('mouseout', handleMouseOut, true);

        return () => {
            document.removeEventListener('focusin', handleFocusIn, true);
            document.removeEventListener('focusout', handleFocusOut, true);
            document.removeEventListener('mouseover', handleMouseOver, true);
            document.removeEventListener('mouseout', handleMouseOut, true);
        };
    }, [isVisible, isFrozen]);

    if (!isVisible) return null;

    const baseClasses = "focus-debugger-container fixed bottom-4 left-4 z-50 p-3 bg-gray-900/80 dark:bg-black/80 text-white rounded-lg shadow-2xl font-sans w-full max-w-xl backdrop-blur-sm transition-all";
    const frozenClasses = isFrozen ? "pointer-events-auto ring-2 ring-sky-500" : "pointer-events-none";

    return (
        <div className={`${baseClasses} ${frozenClasses}`}>
            <div className="flex justify-between items-center mb-3">
                <div className="flex items-center gap-2">
                    <Icon name="BugAnt" iconSet={iconSet} className="w-5 h-5 text-amber-400" />
                    <h3 className="font-bold text-lg text-amber-400">Debug Inspector</h3>
                </div>
                <Tooltip content={isFrozen ? 'Resume Inspector (F4)' : 'Freeze Inspector (F4)'}>
                    <button
                        onClick={() => setIsFrozen(!isFrozen)}
                        className="flex items-center gap-2 px-3 py-1.5 text-sm font-semibold rounded-lg transition-colors bg-sky-600 hover:bg-sky-700 text-white pointer-events-auto"
                    >
                        <Icon name={isFrozen ? 'Play' : 'Snowflake'} iconSet={iconSet} className="w-5 h-5" />
                        <span>{isFrozen ? 'Resume' : 'Freeze'}</span>
                    </button>
                </Tooltip>
            </div>
            <div className="flex items-start gap-3">
                <DebugInfoDisplay title="Focus Target" info={focusedInfo} icon="Eye" isFrozen={isFrozen} iconSet={iconSet} />
                <DebugInfoDisplay title="Hover Target" info={hoveredInfo} icon="CursorArrowRays" isFrozen={isFrozen} iconSet={iconSet} />
            </div>
        </div>
    );
};
