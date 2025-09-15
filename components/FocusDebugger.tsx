import React from 'react';
import { Icon } from './icons/index.tsx';
import { IconSet } from '../types.ts';

interface FocusDebuggerProps {
    isVisible: boolean;
    iconSet: IconSet;
}

const getElementInfo = (element: Element | null): string => {
    if (!element) return 'N/A';
    const el = element as HTMLElement;
    const name = el.getAttribute('aria-label') || el.title || el.id || el.getAttribute('name') || el.dataset.testid;
    const tag = el.tagName.toLowerCase();
    
    if (name) {
        return `${name} (${tag})`;
    }
    
    const classList = el.className && typeof el.className === 'string' 
        ? `.${el.className.split(' ').filter(Boolean).join('.')}` 
        : '';
    return `${tag}${classList}`;
};

export const FocusDebugger: React.FC<FocusDebuggerProps> = ({ isVisible, iconSet }) => {
    const [focusedInfo, setFocusedInfo] = React.useState('N/A');
    const [hoveredInfo, setHoveredInfo] = React.useState('N/A');

    React.useEffect(() => {
        if (!isVisible) {
            // Cleanup if the debugger is turned off
            setFocusedInfo('N/A');
            setHoveredInfo('N/A');
            return;
        }

        const handleFocusIn = (e: FocusEvent) => setFocusedInfo(getElementInfo(e.target as Element));
        const handleFocusOut = () => setFocusedInfo('N/A');
        const handleMouseOver = (e: MouseEvent) => setHoveredInfo(getElementInfo(e.target as Element));
        const handleMouseOut = () => setHoveredInfo('N/A');

        document.addEventListener('focusin', handleFocusIn);
        document.addEventListener('focusout', handleFocusOut);
        document.addEventListener('mouseover', handleMouseOver);
        document.addEventListener('mouseout', handleMouseOut);

        return () => {
            document.removeEventListener('focusin', handleFocusIn);
            document.removeEventListener('focusout', handleFocusOut);
            document.removeEventListener('mouseover', handleMouseOver);
            document.removeEventListener('mouseout', handleMouseOut);
        };
    }, [isVisible]);

    if (!isVisible) {
        return null;
    }

    return (
        <div className="fixed bottom-10 left-2 z-50 p-2 bg-gray-900/80 dark:bg-black/80 text-white rounded-lg shadow-lg font-mono text-xs max-w-sm pointer-events-none backdrop-blur-sm">
            <div className="flex items-start gap-2">
                <Icon name="BugAnt" iconSet={iconSet} className="w-4 h-4 flex-shrink-0 text-amber-400 mt-0.5" />
                <div>
                    <div className="flex items-center gap-1.5">
                        <span className="font-bold text-sky-400">Focus:</span>
                        <span className="truncate" title={focusedInfo}>{focusedInfo}</span>
                    </div>
                    <div className="flex items-center gap-1.5">
                        <span className="font-bold text-amber-400">Hover:</span>
                        <span className="truncate" title={hoveredInfo}>{hoveredInfo}</span>
                    </div>
                </div>
            </div>
        </div>
    );
};