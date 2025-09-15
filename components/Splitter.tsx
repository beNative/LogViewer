import React from 'react';

interface SplitterProps {
    onDrag: (deltaX: number) => void;
}

export const Splitter: React.FC<SplitterProps> = ({ onDrag }) => {
    
    const handleMouseDown = (downEvent: React.MouseEvent) => {
        downEvent.preventDefault();
        
        const startX = downEvent.clientX;
        let lastX = startX;

        const handleMouseMove = (moveEvent: MouseEvent) => {
            const deltaX = moveEvent.clientX - lastX;
            onDrag(deltaX);
            lastX = moveEvent.clientX;
        };

        const handleMouseUp = () => {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
            document.body.style.cursor = '';
        };
        
        document.body.style.cursor = 'col-resize';
        document.addEventListener('mousemove', handleMouseMove);
        document.addEventListener('mouseup', handleMouseUp);
    };

    return (
        <div
            onMouseDown={handleMouseDown}
            className="flex-shrink-0 w-1.5 cursor-col-resize bg-gray-300 dark:bg-gray-600 hover:bg-sky-500 active:bg-sky-600 transition-colors duration-150 z-10"
            style={{ touchAction: 'none' }}
        />
    );
};