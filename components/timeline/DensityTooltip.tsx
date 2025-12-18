
import React from 'react';
import { LogDensityPointByLevel, LogDensityPoint } from '../../types';
import { Theme, getLevelColor } from '../../utils/timelineUtils';

type BucketData = LogDensityPointByLevel | LogDensityPoint;

interface DensityTooltipProps {
    x: number;
    y: number;
    bucketData: BucketData;
    theme: Theme;
}

export const DensityTooltip: React.FC<DensityTooltipProps> = ({ x, y, bucketData, theme }) => {
    const ref = React.useRef<HTMLDivElement>(null);
    const [style, setStyle] = React.useState<React.CSSProperties>({
        position: 'fixed',
        // Start off-screen and invisible to prevent flicker while calculating position
        top: -9999,
        left: -9999,
        opacity: 0,
    });

    React.useLayoutEffect(() => {
        if (ref.current) {
            const tooltip = ref.current;
            const { width, height } = tooltip.getBoundingClientRect();
            const GAP = 10; // 10px gap from viewport edges

            // Default position: centered above the cursor
            let newTop = y - height - GAP;
            let newLeft = x - (width / 2);

            // Check for vertical boundary collision
            if (newTop < GAP) {
                // Not enough space above, flip to below
                newTop = y + 20; // Some gap below the cursor
            }

            // Check for left boundary collision
            if (newLeft < GAP) {
                newLeft = GAP;
            }

            // Check for right boundary collision
            if (newLeft + width > window.innerWidth - GAP) {
                newLeft = window.innerWidth - width - GAP;
            }

            setStyle({
                position: 'fixed',
                top: newTop,
                left: newLeft,
                // The animate-fadeIn class will handle the opacity transition
            });
        }
    }, [x, y, bucketData]);

    const isLevelData = 'counts' in bucketData;
    const total = isLevelData ? Object.values(bucketData.counts).reduce((sum, count) => sum + count, 0) : bucketData.count;
    const sortedLevels = isLevelData ? Object.entries(bucketData.counts).sort((a, b) => b[1] - a[1]) : [];
    const unitLabel = isLevelData ? 'logs' : 'density';

    return (
        <div ref={ref} style={style} className="z-50 p-2.5 bg-gray-800/90 dark:bg-black/80 text-white rounded-lg shadow-2xl pointer-events-none w-56 backdrop-blur-sm animate-fadeIn">
            <div className="text-center mb-2">
                <p className="text-xs text-gray-400">Time Bucket</p>
                <p className="font-mono text-sm">{new Date(bucketData.time).toLocaleString()}</p>
                <p className="font-bold text-lg">{total.toLocaleString()} <span className="text-sm font-normal text-gray-300">{unitLabel}</span></p>
            </div>
            {isLevelData && (
                <div className="space-y-1">
                    {sortedLevels.map(([level, count]) => {
                        const percentage = total > 0 ? (count / total) * 100 : 0;
                        return (
                            <div key={level} className="text-xs">
                                <div className="flex justify-between items-center mb-0.5">
                                    <span className="font-semibold" style={{ color: getLevelColor(level, theme) }}>{level}</span>
                                    <span className="font-mono">{count.toLocaleString()} ({percentage.toFixed(1)}%)</span>
                                </div>
                                <div className="w-full bg-gray-600/50 rounded-full h-1.5">
                                    <div className="h-1.5 rounded-full" style={{ width: `${percentage}%`, backgroundColor: getLevelColor(level, theme) }} />
                                </div>
                            </div>
                        );
                    })}
                </div>
            )}
        </div>
    );
};
