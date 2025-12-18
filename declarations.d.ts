declare module '*.svg' {
    const content: string;
    export default content;
}
declare module '*.png' {
    const content: string;
    export default content;
}

declare module 'sql.js';

// External library type declarations (loaded via CDN in index.html)

/**
 * Chart.js library - loaded globally via script tag.
 * We use unknown instead of 'any' for type safety.
 */
declare const Chart: {
    new(ctx: CanvasRenderingContext2D, config: unknown): { destroy: () => void; update: () => void };
    register: (...items: unknown[]) => void;
};

/**
 * Chart.js data labels plugin
 */
declare const ChartDataLabels: unknown;

/**
 * JSZip library for handling ZIP archives
 */
declare const JSZip: {
    loadAsync: (data: Blob | File | ArrayBuffer) => Promise<{
        files: Record<string, { async: (type: 'string') => Promise<string> }>;
    }>;
};
