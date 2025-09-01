
import React from 'react';

export const DatabaseIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
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
      d="M3.75 6A2.25 2.25 0 016 3.75h12A2.25 2.25 0 0120.25 6v1.5a2.25 2.25 0 01-2.25 2.25H6A2.25 2.25 0 013.75 7.5V6zM3.75 15A2.25 2.25 0 016 12.75h12a2.25 2.25 0 012.25 2.25v1.5A2.25 2.25 0 0118 18.75H6A2.25 2.25 0 013.75 16.5V15z" 
    />
    <path 
      strokeLinecap="round" 
      strokeLinejoin="round" 
      d="M20.25 9v9A2.25 2.25 0 0118 20.25H6a2.25 2.25 0 01-2.25-2.25V9m16.5 0H3.75" 
    />
  </svg>
);