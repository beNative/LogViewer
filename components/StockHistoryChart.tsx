import React from 'react';
import { StockChartDataPoint } from '../types.ts';

declare const Chart: any;

type Theme = 'light' | 'dark';

interface StockHistoryChartProps {
  data: StockChartDataPoint[];
  theme: Theme;
}

export const StockHistoryChart: React.FC<StockHistoryChartProps> = ({ data, theme }) => {
  const canvasRef = React.useRef<HTMLCanvasElement>(null);
  const chartRef = React.useRef<any>(null);

  React.useEffect(() => {
    if (!canvasRef.current || !data) return;

    const ctx = canvasRef.current.getContext('2d');
    if (!ctx) return;
    
    if (chartRef.current) {
      chartRef.current.destroy();
    }

    const chartData = {
        labels: data.map(d => new Date(d.time)),
        datasets: [{
            label: 'Quantity',
            data: data.map(d => d.quantity),
            backgroundColor: 'rgba(16, 185, 129, 0.2)',
            borderColor: 'rgba(16, 185, 129, 1)',
            borderWidth: 2,
            pointRadius: 3,
            pointBackgroundColor: 'rgba(16, 185, 129, 1)',
            tension: 0.1,
            fill: true,
        }]
    };

    const gridColor = theme === 'dark' ? 'rgba(255, 255, 255, 0.1)' : 'rgba(0, 0, 0, 0.1)';
    const tickColor = theme === 'dark' ? '#9ca3af' : '#4b5563';
    const tooltipBgColor = theme === 'dark' ? '#1f2937' : '#ffffff';
    const tooltipTitleColor = theme === 'dark' ? '#f3f4f6' : '#111827';
    const tooltipBodyColor = theme === 'dark' ? '#d1d5db' : '#374151';

    chartRef.current = new Chart(ctx, {
      type: 'line',
      data: chartData,
      options: {
        maintainAspectRatio: false,
        responsive: true,
        scales: {
          x: {
            type: 'time',
            time: {
                tooltipFormat: 'yyyy-MM-dd HH:mm:ss',
            },
            grid: { color: gridColor },
            ticks: { color: tickColor, maxRotation: 25, autoSkip: true }
          },
          y: {
            beginAtZero: false,
            title: { display: true, text: 'Quantity', color: tickColor },
            grid: { color: gridColor },
            ticks: { color: tickColor }
          }
        },
        plugins: {
          legend: { display: false },
          tooltip: {
            backgroundColor: tooltipBgColor,
            titleColor: tooltipTitleColor,
            bodyColor: tooltipBodyColor,
          },
        }
      }
    });

  }, [data, theme]);

  return <canvas ref={canvasRef} />;
};
