import React from 'react';
import { DashboardData, IconSet } from '../types';
import { TimelineChart } from './TimelineChart';
import { CategoryChart } from './CategoryChart';
import { Icon } from './icons';

type Theme = 'light' | 'dark';

import { useAnalytics } from '../contexts/AnalyticsContext';
import { useSession } from '../contexts/SessionContext';
import { useData } from '../contexts/DataContext';
import { useSettings } from '../contexts/SettingsContext';

interface DashboardProps { }

const ChartCard: React.FC<{ title: string; children: React.ReactNode }> = ({ title, children }) => (
  <div className="bg-white dark:bg-gray-800/50 p-4 sm:p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
    <h3 className="text-lg font-semibold text-gray-800 dark:text-gray-200 mb-4">{title}</h3>
    <div className="h-64">{children}</div>
  </div>
);

export const Dashboard: React.FC<DashboardProps> = () => {
  const { dashboardData } = useAnalytics();
  const { hasData } = useSession();
  const { handleTimeRangeSelect, handleCategorySelect } = useData();
  const { theme, iconSet } = useSettings();
  if (!hasData) {
    return (
      <div className="flex-grow flex items-center justify-center p-8 text-center bg-gray-100 dark:bg-transparent">
        <div>
          <Icon name="ChartBar" iconSet={iconSet} className="w-24 h-24 text-gray-300 dark:text-gray-700 mx-auto mb-6" />
          <h2 className="text-2xl font-semibold text-gray-700 dark:text-gray-300">Dashboard is Empty</h2>
          <p className="text-gray-500 dark:text-gray-500 mt-2 max-w-md mx-auto">
            Load data in the <strong className="text-gray-800 dark:text-gray-400">Data Hub</strong> tab to see visualizations.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="flex-grow p-4 sm:p-6 lg:p-8 overflow-y-auto space-y-6 bg-gray-100 dark:bg-transparent">
      <div className="bg-white dark:bg-gray-800/50 p-6 rounded-xl ring-1 ring-gray-200 dark:ring-white/10 shadow-sm">
        <h3 className="text-xl font-semibold text-gray-800 dark:text-gray-200 mb-4">Log Volume Over Time</h3>
        <p className="text-sm text-gray-600 dark:text-gray-400 mb-4 -mt-2">Click and drag horizontally on the chart to select a time range and filter the log viewer.</p>
        <div className="h-80">
          <TimelineChart data={dashboardData.timeline} onTimeRangeSelect={handleTimeRangeSelect} theme={theme} />
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard title="Distribution by Log Level">
          <CategoryChart
            data={dashboardData.levels}
            onSliceClick={(value) => handleCategorySelect('level', value)}
            theme={theme}
          />
        </ChartCard>
        <ChartCard title="Distribution by Sender Type">
          <CategoryChart
            data={dashboardData.senderTypes}
            onSliceClick={(value) => handleCategorySelect('sndrtype', value)}
            theme={theme}
          />
        </ChartCard>
      </div>
    </div>
  );
};