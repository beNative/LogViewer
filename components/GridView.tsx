import React from 'react';
import { GridData } from '../types.ts';

interface GridViewProps {
  data: GridData;
}

export const GridView: React.FC<GridViewProps> = ({ data }) => {
  if (!data || !data.headers || !data.rows) {
    return null;
  }

  return (
    <div className="bg-white dark:bg-gray-900 rounded-lg ring-1 ring-gray-200 dark:ring-gray-700 overflow-hidden">
      <div className="overflow-x-auto">
        <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
          <thead className="bg-gray-50 dark:bg-gray-800/50 sticky top-0">
            <tr>
              {data.headers.map((header, index) => (
                <th 
                  key={index} 
                  scope="col" 
                  className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider whitespace-nowrap"
                >
                  {header}
                </th>
              ))}
            </tr>
          </thead>
          <tbody className="divide-y divide-gray-200 dark:divide-gray-800">
            {data.rows.map((row, rowIndex) => (
              <tr key={rowIndex} className="hover:bg-gray-50 dark:hover:bg-gray-800/70">
                {row.map((cell, cellIndex) => (
                  <td 
                    key={cellIndex} 
                    className="px-4 py-2 whitespace-nowrap text-sm text-gray-800 dark:text-gray-200 font-mono"
                  >
                    {cell}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};