import React from 'react';


export const CogIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg 
    xmlns="http://www.w3.org/2000/svg" 
    fill="none" 
    viewBox="0 0 24 24" 
    strokeWidth={1.5} 
    stroke="currentColor" 
    {...props}>
    <path 
      strokeLinecap="round" 
      strokeLinejoin="round" 
      d="M4.5 12a7.5 7.5 0 0015 0m-15 0a7.5 7.5 0 1115 0m-15 0H3m18 0h-1.5m-15 0a7.5 7.5 0 1115 0m-15 0h-1.5m15 0h1.5m-16.5 5.25l-1.5 1.5M5.25 20.25l1.5-1.5M18.75 3.75l1.5 1.5M18.75 3.75l-1.5 1.5M3.75 18.75l1.5-1.5M3.75 18.75l1.5 1.5m13.5-13.5l1.5-1.5M20.25 5.25l-1.5 1.5M15 12a3 3 0 11-6 0 3 3 0 016 0z" 
    />
  </svg>
);