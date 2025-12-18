
import React, { useRef } from 'react';
import { PanelWidths } from '../types';

export const usePanelResizer = (
    panelWidths: PanelWidths,
    onPanelWidthsChange: (widths: PanelWidths) => void
) => {
    // Use ref to access latest widths inside event handlers without re-binding
    const panelWidthsRef = useRef(panelWidths);
    panelWidthsRef.current = panelWidths;

    const handleFilterResize = (deltaX: number) => {
        const newWidth = Math.max(240, Math.min(800, panelWidthsRef.current.filters + deltaX));
        onPanelWidthsChange({ ...panelWidthsRef.current, filters: newWidth });
    };

    const handleDetailsResize = (deltaX: number) => {
        // Delta is positive when moving right, which should decrease the details panel width
        // (Assuming right-aligned details panel logic from original code)
        const newWidth = Math.max(300, Math.min(1200, panelWidthsRef.current.details - deltaX));
        onPanelWidthsChange({ ...panelWidthsRef.current, details: newWidth });
    };

    return {
        handleFilterResize,
        handleDetailsResize
    };
};
