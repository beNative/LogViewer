import React from 'react';


export const TableIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    fill="none"
    viewBox="0 0 24 24"
    strokeWidth={1.5}
    stroke="currentColor"
    {...props}
  >
    <path
      strokeLinecap="round"
      strokeLinejoin="round"
      d="M3.375 19.5h17.25m-17.25 0a1.125 1.125 0 01-1.125-1.125v-12.75c0-.621.504-1.125 1.125-1.125h17.25c.621 0 1.125.504 1.125 1.125v12.75c0 .621-.504 1.125-1.125 1.125m-17.25 0h.008v.015h-.008V19.5zm.75-5.25h15.75m-15.75 0v.015h15.75V14.25m-15.75 0h.008v.015h-.008v-.015zm0-4.5h15.75m-15.75 0v.015h15.75V9.75m-15.75 0h.008v.015h-.008V9.75z"
    />
  </svg>
);