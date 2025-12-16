import React from 'react';

/**
 * Custom hook that observes an element's width using ResizeObserver.
 * Returns 0 when the element is not visible or not mounted.
 * 
 * @param ref - React ref to the element to observe
 * @param isVisible - Whether the element should be observed (default: true)
 * @returns The current width of the element in pixels
 */
export function useResizeObserver(
    ref: React.RefObject<HTMLElement>,
    isVisible: boolean = true,
    debounceMs: number = 0
): number {
    const [width, setWidth] = React.useState(0);
    const timeoutRef = React.useRef<number | null>(null);

    React.useEffect(() => {
        // When not visible, reset width
        if (!isVisible) {
            setWidth(0);
            return;
        }

        // Store element in local variable for stable reference in cleanup
        const element = ref.current;
        if (!element) {
            // When element is not present (e.g., hidden), reset width.
            setWidth(0);
            return;
        }

        const resizeObserver = new ResizeObserver(entries => {
            if (entries[0]) {
                const newWidth = entries[0].contentRect.width;

                if (debounceMs > 0) {
                    if (timeoutRef.current) {
                        window.clearTimeout(timeoutRef.current);
                    }
                    timeoutRef.current = window.setTimeout(() => {
                        setWidth(newWidth);
                    }, debounceMs);
                } else {
                    setWidth(newWidth);
                }
            }
        });

        resizeObserver.observe(element);

        // Cleanup: Disconnect observer when component unmounts or ref changes.
        return () => {
            if (timeoutRef.current) {
                window.clearTimeout(timeoutRef.current);
            }
            resizeObserver.disconnect();
        };
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [ref, isVisible, debounceMs]); // Re-run when visibility changes

    return width;
}
