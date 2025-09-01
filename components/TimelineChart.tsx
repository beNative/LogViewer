import React from 'react';
import { TimelineDataPoint } from '../types.ts';

// Chart.js and plugins are loaded from script tags
declare const Chart: any;
declare const ChartDataLabels: any;

type Theme = 'light' | 'dark';

interface TimelineChartProps {
  data: TimelineDataPoint[];
  onTimeRangeSelect: (startTime: number, endTime: number) => void;
  theme: Theme;
}

export const TimelineChart: React.FC<TimelineChartProps> = ({ data, onTimeRangeSelect, theme }) => {
  const canvasRef = React.useRef<HTMLCanvasElement>(null);
  const chartRef = React.useRef<any>(null);

  React.useEffect(() => {
    if (!canvasRef.current || !data) return;

    const ctx = canvasRef.current.getContext('2d');
    if (!ctx) return;
    
    // Destroy previous chart instance if it exists
    if (chartRef.current) {
      chartRef.current.destroy();
    }

    const chartData = {
        labels: data.map(d => new Date(d.time)),
        datasets: [{
            label: 'Log Count',
            data: data.map(d => d.count),
            backgroundColor: 'rgba(59, 130, 246, 0.6)', // Adjusted opacity
            borderColor: 'rgba(59, 130, 246, 1)',
            borderWidth: 1,
            barPercentage: 1.0,
            categoryPercentage: 1.0,
        }]
    };

    const gridColor = theme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)';
    const tickColor = theme === 'dark' ? '#9ca3af' : '#4b5563';
    const tooltipBgColor = theme === 'dark' ? '#1f2937' : '#ffffff';
    const tooltipTitleColor = theme === 'dark' ? '#f3f4f6' : '#111827';
    const tooltipBodyColor = theme === 'dark' ? '#d1d5db' : '#374151';

    chartRef.current = new Chart(ctx, {
      type: 'bar',
      data: chartData,
      options: {
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
            grid: {
                color: gridColor,
            },
            ticks: {
                color: tickColor,
                maxRotation: 0,
                autoSkip: true,
            }
          },
          y: {
            beginAtZero: true,
            grid: {
                color: gridColor,
            },
            ticks: {
                color: tickColor
            }
          }
        },
        plugins: {
          legend: {
            display: false,
          },
          tooltip: {
            backgroundColor: tooltipBgColor,
            titleColor: tooltipTitleColor,
            bodyColor: tooltipBodyColor,
            titleFont: { size: 14 },
            bodyFont: { size: 12 },
            padding: 10,
            boxPadding: 5,
            borderColor: gridColor,
            borderWidth: 1,
          },
          zoom: {
            pan: {
                enabled: true,
                mode: 'x',
            },
            zoom: {
                drag: {
                    enabled: true,
                    backgroundColor: 'rgba(147, 197, 253, 0.3)'
                },
                mode: 'x',
                onZoomComplete: ({chart}: {chart: any}) => {
                    const min = chart.scales.x.min;
                    const max = chart.scales.x.max;
                    onTimeRangeSelect(min, max);
                    // Reset zoom after selection
                    chart.resetZoom();
                }
            }
          }
        }
      }
    });

  }, [data, onTimeRangeSelect, theme]);

  return <canvas ref={canvasRef} />;
};
