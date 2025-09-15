import React from 'react';

interface SplitterProps {
    onResize: (delta: number) => void;
    direction?: 'horizontal' | 'vertical';
    className?: string;
}

export const Splitter: React.FC<SplitterProps> = ({ onResize, direction = 'vertical', className }) => {
    const handleMouseDown = React.useCallback((e: React.MouseEvent<HTMLDivElement>) => {
        e.preventDefault();

        const handleMouseMove = (moveEvent: MouseEvent) => {
            const delta = direction === 'vertical' ? moveEvent.movementX : moveEvent.movementY;
            onResize(delta);
        };

        const handleMouseUp = () => {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
        };

        document.addEventListener('mousemove', handleMouseMove);
        document.addEventListener('mouseup', handleMouseUp);
    }, [onResize, direction]);

    const baseClasses = "flex-shrink-0 bg-gray-200 dark:bg-gray-700 hover:bg-sky-500 dark:hover:bg-sky-500 transition-colors duration-200";
    const directionClasses = direction === 'vertical'
        ? "w-1.5 cursor-col-resize"
        : "h-1.5 cursor-row-resize";

    return (
        <div
            onMouseDown={handleMouseDown}
            className={`${baseClasses} ${directionClasses} ${className}`}
        />
    );
};
