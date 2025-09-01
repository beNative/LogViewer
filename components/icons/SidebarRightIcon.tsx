import React from 'react';

export const SidebarRightIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
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
        d="M20.25 3.75H3.75A1.5 1.5 0 002.25 5.25v13.5A1.5 1.5 0 003.75 20.25h16.5a1.5 1.5 0 001.5-1.5V5.25a1.5 1.5 0 00-1.5-1.5z" 
    />
    <path 
        strokeLinecap="round" 
        strokeLinejoin="round" 
        d="M15.75 3.75v16.5" 
    />
  </svg>
);