import React from 'react';


interface KeyValueTableProps {
  data: { key: string; value: string }[];
}

export const KeyValueTableView: React.FC<KeyValueTableProps> = ({ data }) => {
  return (
    <div className="bg-white dark:bg-gray-900 rounded-lg ring-1 ring-gray-200 dark:ring-gray-700 overflow-hidden">
      <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
        <thead className="bg-gray-50 dark:bg-gray-800/50">
          <tr>
            <th scope="col" className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider w-1/3">
              Name
            </th>
            <th scope="col" className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
              Value
            </th>
          </tr>
        </thead>
        <tbody className="divide-y divide-gray-200 dark:divide-gray-800">
          {data.map(({ key, value }) => (
            <tr key={key}>
              <td className="px-4 py-2 whitespace-nowrap text-sm font-medium text-amber-600 dark:text-amber-400 font-mono">{key}</td>
              <td className="px-4 py-2 whitespace-pre-wrap break-all text-sm text-gray-800 dark:text-gray-200 font-mono">{value}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};