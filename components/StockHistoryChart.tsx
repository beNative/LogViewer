import React from 'react';
import { StockChartDataPoint, StockChartDataset } from '../types.ts';


type Theme = 'light' | 'dark';

interface StockHistoryChartProps {
  // data: StockChartDataPoint[]; // Deprecated
  datasets: StockChartDataset[];
  theme: Theme;
  selectedPoint: { articleId: string, timestamp: string } | null;
}

// Helper to generate colors if not provided
const generateColor = (index: number, theme: Theme) => {
  const seeds = [
    '16, 185, 129', // Emerald 500
    '59, 130, 246', // Blue 500
    '249, 115, 22', // Orange 500
    '139, 92, 246', // Violet 500
    '236, 72, 153', // Pink 500
    '14, 165, 233', // Sky 500
    '234, 179, 8',  // Yellow 500
    '239, 68, 68',  // Red 500
  ];
  const color = seeds[index % seeds.length];
  return {
    border: `rgba(${color}, 1)`,
    background: `rgba(${color}, 0.2)`,
    point: `rgba(${color}, 1)`,
  };
};

export const StockHistoryChart: React.FC<StockHistoryChartProps> = ({ datasets, theme, selectedPoint }) => {
  const canvasRef = React.useRef<HTMLCanvasElement>(null);
  const chartRef = React.useRef<any>(null); // Use any because Chart type might not be global or easily importable

  React.useEffect(() => {
    if (!canvasRef.current || !datasets || datasets.length === 0) return;

    const ctx = canvasRef.current.getContext('2d');
    if (!ctx) return;

    if (chartRef.current) {
      chartRef.current.destroy();
    }

    const chartDatasets = datasets.map((ds, index) => {
      const colors = generateColor(index, theme);
      const borderColor = ds.borderColor || colors.border;
      const backgroundColor = ds.backgroundColor || colors.background;

      return {
        label: ds.label,
        data: ds.data.map(d => ({ x: new Date(d.time), y: d.quantity })),
        borderColor: borderColor,
        backgroundColor: backgroundColor,
        borderWidth: 2,
        pointRadius: 3,
        pointHitRadius: 10,
        pointBackgroundColor: borderColor,
        tension: 0.1,
        fill: datasets.length === 1, // Only fill if single series
        // Metadata to help matching
        _articleId: ds.id,
      };
    });

    const gridColor = theme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)';
    const tickColor = theme === 'dark' ? '#9ca3af' : '#4b5563';
    const tooltipBgColor = theme === 'dark' ? '#1f2937' : '#ffffff';
    const tooltipTitleColor = theme === 'dark' ? '#f3f4f6' : '#111827';
    const tooltipBodyColor = theme === 'dark' ? '#d1d5db' : '#374151';

    chartRef.current = new Chart(ctx, {
      type: 'line',
      data: { datasets: chartDatasets },
      options: {
        animation: false,
        maintainAspectRatio: false,
        responsive: true,
        scales: {
          x: {
            type: 'time',
            time: {
              tooltipFormat: 'yyyy-MM-dd HH:mm:ss',
              displayFormats: {
                minute: 'MMM d, HH:mm:ss',
                hour: 'MMM d, HH:mm',
                day: 'MMM d, yyyy',
                week: 'MMM d, yyyy',
                month: 'MMM yyyy',
                year: 'yyyy',
              }
            },
            grid: { color: gridColor },
            ticks: {
              color: tickColor,
              maxRotation: 0,
              autoSkip: true,
              maxTicksLimit: 10,
            }
          },
          y: {
            beginAtZero: false,
            title: { display: true, text: 'Quantity', color: tickColor },
            grid: { color: gridColor },
            ticks: {
              color: tickColor,
              precision: 0
            }
          }
        },
        plugins: {
          legend: {
            display: datasets.length > 1,
            labels: { color: tickColor }
          },
          tooltip: {
            backgroundColor: theme === 'dark' ? 'rgba(17, 24, 39, 0.95)' : 'rgba(255, 255, 255, 0.95)',
            titleColor: theme === 'dark' ? '#f3f4f6' : '#111827',
            bodyColor: theme === 'dark' ? '#d1d5db' : '#374151',
            borderColor: theme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)',
            borderWidth: 1,
            padding: 10,
            cornerRadius: 6,
            titleFont: { size: 13, weight: 'bold' },
            bodyFont: { size: 12 },
            displayColors: true,
            boxPadding: 4,
            mode: 'nearest',
            intersect: false,
            callbacks: {
              title: (tooltipItems: any[]) => {
                if (!tooltipItems || tooltipItems.length === 0) return '';
                const date = new Date(tooltipItems[0].parsed.x);
                if (isNaN(date.getTime())) return '';

                const isoString = date.toISOString(); // YYYY-MM-DDTHH:mm:ss.sssZ
                const datePart = isoString.substring(0, 10);
                const timePart = isoString.substring(11, 23); // Includes milliseconds

                return [datePart, timePart];
              },
              label: (tooltipItem: any) => {
                let label = tooltipItem.dataset.label || '';
                if (label) {
                  label += ': ';
                }
                if (tooltipItem.parsed.y !== null) {
                  label += Math.round(tooltipItem.parsed.y).toLocaleString();
                }
                return label;
              }
            }
          },
        }
      }
    });

  }, [datasets, theme]); // Re-create chart only if datasets or theme change

  // Separate effect for selection highlighting to avoid full re-render
  React.useEffect(() => {
    const chart = chartRef.current;
    if (!chart || !selectedPoint) return;

    // Find the dataset corresponding to the selected article
    // Note: chartDatasets created in main effect don't persist extra properties directly reachable here easily 
    // without accessing chart.data.
    // But we know the order is preserving props. 
    // Wait, chart.js might strip custom props from dataset unless we put them in a specific place.
    // However, we know `datasets` prop corresponds index-to-index with `chart.data.datasets`.

    // 1. Find dataset index
    const datasetIndex = datasets.findIndex(ds => ds.id === selectedPoint.articleId);
    if (datasetIndex === -1) return;

    // 2. Find data point index
    // The chart data points are converted to {x: Date, y: number}. 
    // We need to match the timestamp.
    const targetTime = new Date(selectedPoint.timestamp).getTime();
    const dataIndex = datasets[datasetIndex].data.findIndex(d => new Date(d.time).getTime() === targetTime);

    if (dataIndex !== -1) {
      chart.setActiveElements([{ datasetIndex, index: dataIndex }]);
      chart.tooltip?.setActiveElements([{ datasetIndex, index: dataIndex }], { x: 0, y: 0 }); // Coords ignored usually
      chart.update();
    } else {
      chart.setActiveElements([]);
      chart.tooltip?.setActiveElements([], { x: 0, y: 0 });
      chart.update();
    }

  }, [selectedPoint, datasets]);

  return <canvas ref={canvasRef} />;
};