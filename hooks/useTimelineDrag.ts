import React from 'react';
import { OverallTimeRange } from '../types';

export type DragState =
    | { type: 'select_left'; startX: number; initialStart: number; initialEnd: number }
    | { type: 'select_right'; startX: number; initialStart: number; initialEnd: number }
    | { type: 'cursor'; startX: number }
    | { type: 'new_selection'; startX: number; startTime: number };

interface UseTimelineDragProps {
    mainContainerRef: React.RefObject<HTMLDivElement>;
    uiScale: number;
    displayMinTime: number;
    displayMaxTime: number;
    selectedStartTime: number | null;
    selectedEndTime: number | null;
    cursorTime: number | null;
    mainPosToValue: (p: number) => number;
    onRangeChange: (start: number, end: number) => void;
    onCursorChange: (time: number) => void;
    zoomToSelectionEnabled: boolean;
    setOptimisticSelection: (sel: { start: number; end: number } | null) => void;
    setOptimisticViewRange: (range: { min: number; max: number } | null) => void;
    setViewRange: (range: OverallTimeRange | null) => void;
}

export const useTimelineDrag = ({
    mainContainerRef,
    uiScale,
    displayMinTime,
    displayMaxTime,
    selectedStartTime,
    selectedEndTime,
    cursorTime,
    mainPosToValue,
    onRangeChange,
    onCursorChange,
    zoomToSelectionEnabled,
    setOptimisticSelection,
    setOptimisticViewRange,
    setViewRange
}: UseTimelineDragProps) => {
    const [dragState, setDragState] = React.useState<DragState | null>(null);
    const [tempSelection, setTempSelection] = React.useState<{ start: number, end: number } | null>(null);
    const [tempCursorTime, setTempCursorTime] = React.useState<number | null>(null);

    // Clear temp cursor when actual cursor catches up
    React.useEffect(() => {
        if (tempCursorTime !== null && cursorTime !== null && Math.abs(tempCursorTime - cursorTime) < 1000) {
            setTempCursorTime(null);
        }
    }, [cursorTime, tempCursorTime]);

    const handleMouseDown = (
        e: React.MouseEvent,
        type: 'select_left' | 'select_right' | 'cursor' | 'new_selection'
    ) => {
        e.stopPropagation();
        e.preventDefault();

        const rect = mainContainerRef.current!.getBoundingClientRect();
        const clickPos = (e.clientX - rect.left) / uiScale;
        const clickTime = mainPosToValue(clickPos);

        if (type === 'cursor') {
            setDragState({ type: 'cursor', startX: e.clientX });
        } else if (type === 'new_selection') {
            setDragState({ type: 'new_selection', startX: e.clientX, startTime: clickTime });
        } else {
            setDragState({
                type: type,
                startX: e.clientX,
                initialStart: selectedStartTime || displayMinTime,
                initialEnd: selectedEndTime || displayMaxTime,
            });
        }
    };

    React.useEffect(() => {
        const handleMouseMove = (e: MouseEvent) => {
            if (!dragState || !mainContainerRef.current) return;
            e.preventDefault();
            const rect = mainContainerRef.current.getBoundingClientRect();
            const currentUnscaledPos = (e.clientX - rect.left) / uiScale;
            const currentTime = mainPosToValue(currentUnscaledPos);

            switch (dragState.type) {
                case 'new_selection': {
                    const start = Math.min(dragState.startTime, currentTime);
                    const end = Math.max(dragState.startTime, currentTime);
                    setTempSelection({ start, end });
                    break;
                }
                case 'select_left': {
                    const newStart = Math.min(dragState.initialEnd, Math.max(displayMinTime, currentTime));
                    setTempSelection({ start: newStart, end: dragState.initialEnd });
                    break;
                }
                case 'select_right': {
                    const newEnd = Math.max(dragState.initialStart, Math.min(displayMaxTime, currentTime));
                    setTempSelection({ start: dragState.initialStart, end: newEnd });
                    break;
                }
                case 'cursor': {
                    const newCursorTime = Math.max(displayMinTime, Math.min(displayMaxTime, currentTime));
                    setTempCursorTime(newCursorTime);
                    break;
                }
            }
        };

        const handleMouseUp = (e: MouseEvent) => {
            if (!dragState) return;

            if (dragState.type === 'new_selection') {
                if (Math.abs(e.clientX - dragState.startX) < 5) {
                    onCursorChange(dragState.startTime);
                } else if (tempSelection) {
                    // Optimistic update
                    setOptimisticSelection(tempSelection);
                    // If zoom enabled, optimistic view update
                    if (zoomToSelectionEnabled) {
                        setOptimisticViewRange({ min: tempSelection.start, max: tempSelection.end });
                        // DIRECT AUTO-ZOOM (User Request)
                        setViewRange({ min: tempSelection.start, max: tempSelection.end });
                    }
                    onRangeChange(tempSelection.start, tempSelection.end);
                }
            } else if (dragState.type === 'select_left' || dragState.type === 'select_right') {
                if (tempSelection) {
                    setOptimisticSelection(tempSelection);
                    if (zoomToSelectionEnabled) {
                        setOptimisticViewRange({ min: tempSelection.start, max: tempSelection.end });
                        // DIRECT AUTO-ZOOM
                        setViewRange({ min: tempSelection.start, max: tempSelection.end });
                    }
                    onRangeChange(tempSelection.start, tempSelection.end);
                }
            } else if (dragState.type === 'cursor' && tempCursorTime !== null) {
                onCursorChange(tempCursorTime);
            }

            setDragState(null);
            setTempSelection(null);
        };

        if (dragState) {
            document.body.style.cursor = 'col-resize';
            document.addEventListener('mousemove', handleMouseMove);
            document.addEventListener('mouseup', handleMouseUp);
        }
        return () => {
            document.body.style.cursor = 'default';
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
        };
    }, [dragState, mainPosToValue, displayMinTime, displayMaxTime, onRangeChange, onCursorChange, tempSelection, tempCursorTime, uiScale, zoomToSelectionEnabled, setViewRange, mainContainerRef, setOptimisticSelection, setOptimisticViewRange]);

    return {
        dragState,
        tempSelection,
        tempCursorTime,
        handleMouseDown
    };
};
