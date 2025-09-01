import React from 'react';

export const SaveDiskIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
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
      d="M16.5 3.75V16.5a2.25 2.25 0 01-2.25 2.25H15v-1.5a.75.75 0 00-.75-.75h-3.75a.75.75 0 00-.75.75v1.5H6.75A2.25 2.25 0 014.5 16.5V6.75A2.25 2.25 0 016.75 4.5h9a2.25 2.25 0 012.25 2.25v.75" 
    />
    <path 
      strokeLinecap="round" 
      strokeLinejoin="round" 
      d="M3 12h18M3 12a9 9 0 019-9h-1.5a9 9 0 00-9 9zM3 12a9 9 0 009 9h1.5a9 9 0 019-9" 
    />
    <path 
      strokeLinecap="round" 
      strokeLinejoin="round" 
      d="M12 12h3.75a.75.75 0 01.75.75v3.75a.75.75 0 01-.75.75h-3.75a.75.75 0 01-.75-.75v-3.75a.75.75 0 01.75-.75z" 
    />
  </svg>
);
