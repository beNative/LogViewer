import React from 'react';

export const BugAntIcon: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
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
      d="M12 21a9.004 9.004 0 008.716-6.747M12 21a9.004 9.004 0 01-8.716-6.747M12 21c1.355 0 2.707-.156 4.008-.45m-8.016 0A9.004 9.004 0 0112 3c1.355 0 2.707.156 4.008.45m0 10.954c.366.191.71.405.998.648m-8.016-11.4a9.004 9.004 0 00-4.008.45m4.008-.45L9 3m3 18l3-2.672M9 3l-3 2.672m0 0a9.004 9.004 0 00-2.716 6.747M15 3l3 2.672m0 0a9.004 9.004 0 012.716 6.747m0-6.747c.366.191.71.405.998.648M12 6.75h.008v.008H12V6.75z"
    />
  </svg>
);