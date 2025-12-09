import React from 'react';
import { CategoryDataPoint } from '../types.ts';

declare const Chart: any;

type Theme = 'light' | 'dark';

interface CategoryChartProps {
  data: CategoryDataPoint[];
  onSliceClick: (value: string) => void;
  theme: Theme;
}

// Pre-defined color scheme for consistency
const CHART_COLORS = [
  '#3B82F6', // blue-500
  '#F59E0B', // amber-500
  '#10B981', // emerald-500
  '#EF4444', // red-500
  '#8B5CF6', // violet-500
  '#EC4899', // pink-500
  '#6366F1', // indigo-500
  '#06B6D4', // cyan-500
];

/**
 * Lightens a hex color by a percentage.
 * Used for creating hover states on chart segments.
 * @param hex - Hex color string (e.g., '#3B82F6')
 * @param percent - Percentage to lighten (0-100)
 * @returns Lightened hex color string
 */
const lightenColor = (hex: string, percent: number) => {
  const num = parseInt(hex.replace("#", ""), 16),
    amt = Math.round(2.55 * percent),
    R = (num >> 16) + amt,
    G = (num >> 8 & 0x00FF) + amt,
    B = (num & 0x0000FF) + amt;
  return "#" + (0x1000000 + (R < 255 ? R < 1 ? 0 : R : 255) * 0x10000 + (G < 255 ? G < 1 ? 0 : G : 255) * 0x100 + (B < 255 ? B < 1 ? 0 : B : 255)).toString(16).slice(1);
};


/**
 * Renders an interactive doughnut chart for categorical data.
 * Supports click interactions to filter by category.
 * @param data - Array of category data points with name and count
 * @param onSliceClick - Callback when a chart slice is clicked
 * @param theme - Current theme ('light' or 'dark')
 */
export const CategoryChart: React.FC<CategoryChartProps> = ({ data, onSliceClick, theme }) => {
  const canvasRef = React.useRef<HTMLCanvasElement>(null);
  const chartRef = React.useRef<any>(null);

  React.useEffect(() => {
    if (!canvasRef.current || !data) return;

    const ctx = canvasRef.current.getContext('2d');
    if (!ctx) return;

    if (chartRef.current) {
      chartRef.current.destroy();
    }

    const labels = data.map(d => d.name);

    const legendColor = theme === 'dark' ? '#d1d5db' : '#374151'; // gray-300 vs gray-700
    const borderColor = theme === 'dark' ? '#1f2937' : '#ffffff'; // gray-800 vs white
    const tooltipBgColor = theme === 'dark' ? '#1f2937' : '#ffffff';
    const tooltipTitleColor = theme === 'dark' ? '#f3f4f6' : '#111827';
    const tooltipBodyColor = theme === 'dark' ? '#d1d5db' : '#374151';

    const chartData = {
      labels: labels,
      datasets: [{
        label: 'Log Count',
        data: data.map(d => d.count),
        backgroundColor: CHART_COLORS,
        borderColor: borderColor,
        borderWidth: 2,
        hoverOffset: 8,
        hoverBackgroundColor: CHART_COLORS.map(c => lightenColor(c, 10))
      }]
    };

    chartRef.current = new Chart(ctx, {
      type: 'doughnut',
      data: chartData,
      options: {
        responsive: true,
        maintainAspectRatio: false,
        cutout: '60%',
        onClick: (evt: any, elements: any[]) => {
          if (elements.length > 0) {
            const clickedIndex = elements[0].index;
            const categoryValue = labels[clickedIndex];
            onSliceClick(categoryValue);
          }
        },
        plugins: {
          legend: {
            position: 'right',
            labels: {
              color: legendColor,
              boxWidth: 20,
              padding: 15,
              font: {
                size: 12,
              }
            }
          },
          tooltip: {
            backgroundColor: tooltipBgColor,
            titleColor: tooltipTitleColor,
            bodyColor: tooltipBodyColor,
            titleFont: { size: 14 },
            bodyFont: { size: 12 },
            padding: 10,
            boxPadding: 5,
            borderColor: theme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)',
            borderWidth: 1,
            callbacks: {
              label: function (context: any) {
                let label = context.dataset.label || '';
                if (label) {
                  label += ': ';
                }
                if (context.parsed !== null) {
                  const total = context.dataset.data.reduce((acc: number, val: number) => acc + val, 0);
                  const percentage = ((context.parsed / total) * 100).toFixed(1);
                  label += `${context.raw.toLocaleString()} (${percentage}%)`;
                }
                return label;
              }
            }
          }
        }
      }
    });

  }, [data, onSliceClick, theme]);

  if (data.length === 0) {
    return <div className="flex items-center justify-center h-full text-gray-500 dark:text-gray-400">No data available for this chart.</div>
  }

  return <canvas ref={canvasRef} />;
};
