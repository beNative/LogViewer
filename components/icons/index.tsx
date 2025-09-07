import React from 'react';
import { IconSet } from '../types.ts';
import { MoonIcon } from './MoonIcon.tsx';
import { SunIcon } from './SunIcon.tsx';

// --- SHARP ICONS (Heroicons Outline) ---

const ArchiveBoxSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 7.5l-.625 10.632a2.25 2.25 0 01-2.247 2.118H6.622a2.25 2.25 0 01-2.247-2.118L3.75 7.5M10 11.25h4M3.375 7.5h17.25c.621 0 1.125-.504 1.125-1.125v-1.5c0-.621-.504-1.125-1.125-1.125H3.375c-.621 0-1.125.504-1.125 1.125v1.5c0 .621.504 1.125 1.125 1.125z" />
  </svg>
);
const ArrowPathSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0011.667 0l3.181-3.183m-4.991-2.692v-4.992m0 0h-4.992m4.992 0l-3.181-3.183a8.25 8.25 0 00-11.667 0L2.985 9.348z" />
  </svg>
);
const ArrowsPointingInSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M9 9V4.5M9 9H4.5M9 9L3.75 3.75M9 15v5.5M9 15H4.5M9 15l-5.25 5.25M15 9h5.5M15 9V4.5M15 9l5.25-5.25M15 15h5.5M15 15v5.5M15 15l5.25 5.25" />
  </svg>
);
const ArrowsPointingOutSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
        <path strokeLinecap="round" strokeLinejoin="round" d="M3.75 3.75v4.5m0-4.5h4.5m-4.5 0L9 9M3.75 20.25v-4.5m0 4.5h4.5m-4.5 0L9 15M20.25 3.75h-4.5m4.5 0v4.5m0-4.5L15 9M20.25 20.25h-4.5m4.5 0v-4.5m0 4.5L15 15" />
    </svg>
);
const ArrowUpTraySharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
    <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
        <path strokeLinecap="round" strokeLinejoin="round" d="M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5m-13.5-9L12 3m0 0l4.5 4.5M12 3v13.5" />
    </svg>
);
const BookOpenSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25" />
  </svg>
);
const BugAntSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 21a9.004 9.004 0 008.716-6.747M12 21a9.004 9.004 0 01-8.716-6.747M12 21c1.355 0 2.707-.156 4.008-.45m-8.016 0A9.004 9.004 0 0112 3c1.355 0 2.707.156 4.008.45m0 10.954c.366.191.71.405.998.648m-8.016-11.4a9.004 9.004 0 00-4.008.45m4.008-.45L9 3m3 18l3-2.672M9 3l-3 2.672m0 0a9.004 9.004 0 00-2.716 6.747M15 3l3 2.672m0 0a9.004 9.004 0 012.716 6.747m0-6.747c.366.191.71.405.998.648M12 6.75h.008v.008H12V6.75z" />
  </svg>
);
const ChartBarSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M3 13.125C3 12.504 3.504 12 4.125 12h2.25c.621 0 1.125.504 1.125 1.125v6.75C7.5 20.496 6.996 21 6.375 21h-2.25A1.125 1.125 0 013 19.875v-6.75zM9.75 8.625c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125v11.25c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V8.625zM16.5 4.125c0-.621.504-1.125 1.125-1.125h2.25C20.496 3 21 3.504 21 4.125v15.75c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V4.125z" />
  </svg>
);
const CheckCircleSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M9 12.75L11.25 15 15 9.75M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const ChevronDownSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 8.25l-7.5 7.5-7.5-7.5" />
  </svg>
);
const ChevronLeftSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 19.5L8.25 12l7.5-7.5" />
  </svg>
);
const ChevronRightSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M8.25 4.5l7.5 7.5-7.5 7.5" />
  </svg>
);
const ClipboardSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M15.666 3.888A2.25 2.25 0 0013.5 2.25h-3c-1.03 0-1.9.693-2.166 1.638m7.332 0c.055.194.084.4.084.612v0a2.25 2.25 0 01-2.25 2.25H9.75A2.25 2.25 0 017.5 4.5v0c0-.212.03-.418.084-.612m7.332 0c.646.049 1.288.11 1.927.184 1.1.128 1.907 1.077 1.907 2.185V19.5a2.25 2.25 0 01-2.25 2.25H6.75A2.25 2.25 0 014.5 19.5V6.257c0-1.108.806-2.057 1.907-2.185a48.208 48.208 0 011.927-.184" />
  </svg>
);
const ClockSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 6v6h4.5m4.5 0a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const CodeBracketSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M17.25 6.75L22.5 12l-5.25 5.25m-10.5 0L1.5 12l5.25-5.25" />
  </svg>
);
const CodeBracketSquareSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M14.25 9.75L16.5 12l-2.25 2.25m-4.5 0L7.5 12l2.25-2.25M6 20.25h12A2.25 2.25 0 0020.25 18V5.75A2.25 2.25 0 0018 3.5H6A2.25 2.25 0 003.75 5.75v12.5A2.25 2.25 0 006 20.25z" />
  </svg>
);
const CogSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M4.5 12a7.5 7.5 0 0015 0m-15 0a7.5 7.5 0 1115 0m-15 0H3m18 0h-1.5m-15 0a7.5 7.5 0 1115 0m-15 0h-1.5m15 0h1.5m-16.5 5.25l-1.5 1.5M5.25 20.25l1.5-1.5M18.75 3.75l1.5 1.5M18.75 3.75l-1.5 1.5M3.75 18.75l1.5-1.5M3.75 18.75l1.5 1.5m13.5-13.5l1.5-1.5M20.25 5.25l-1.5 1.5M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
  </svg>
);
const DatabaseSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M3.75 6A2.25 2.25 0 016 3.75h12A2.25 2.25 0 0120.25 6v1.5a2.25 2.25 0 01-2.25 2.25H6A2.25 2.25 0 013.75 7.5V6zM3.75 15A2.25 2.25 0 016 12.75h12a2.25 2.25 0 012.25 2.25v1.5A2.25 2.25 0 0118 18.75H6A2.25 2.25 0 013.75 16.5V15z" />
    <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 9v9A2.25 2.25 0 0118 20.25H6a2.25 2.25 0 01-2.25-2.25V9m16.5 0H3.75" />
  </svg>
);
const DocumentPlusSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 14.25v-2.625a3.375 3.375 0 00-3.375-3.375h-1.5A1.125 1.125 0 0113.5 7.125v-1.5a3.375 3.375 0 00-3.375-3.375H8.25m3.75 9h6m-6 0h-6m6 0v6m0-6V9.75M12 3H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V9.75M12 3L8.25 6.75" />
  </svg>
);
const DownloadSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5M16.5 12L12 16.5m0 0L7.5 12m4.5 4.5V3" />
  </svg>
);
const ExclamationTriangleSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 9v3.75m-9.303 3.376c-.866 1.5.217 3.374 1.948 3.374h14.71c1.73 0 2.813-1.874 1.948-3.374L13.949 3.378c-.866-1.5-3.032-1.5-3.898 0L2.697 16.126zM12 15.75h.007v.008H12v-.008z" />
  </svg>
);
const FileSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 14.25v-2.625a3.375 3.375 0 00-3.375-3.375h-1.5A1.125 1.125 0 0113.5 7.125v-1.5a3.375 3.375 0 00-3.375-3.375H8.25m.75 12l3 3m0 0l3-3m-3 3v-6m-1.5-9H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V11.25a9 9 0 00-9-9z" />
  </svg>
);
const FilterSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 3c2.755 0 5.455.232 8.083.678.533.09.917.556.917 1.096v1.044a2.25 2.25 0 01-.659 1.591l-5.432 5.432a2.25 2.25 0 00-.659 1.591v2.927a2.25 2.25 0 01-1.244 2.013L9.75 21v-6.572a2.25 2.25 0 00-.659-1.591L3.659 7.409A2.25 2.25 0 013 5.818V4.774c0-.54.384-1.006.917-1.096A48.32 48.32 0 0112 3z" />
  </svg>
);
const FolderSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M2.25 12.75V12A2.25 2.25 0 014.5 9.75h15A2.25 2.25 0 0121.75 12v.75m-8.69-6.44l-2.12-2.12a1.5 1.5 0 00-1.061-.44H4.5A2.25 2.25 0 002.25 6v12a2.25 2.25 0 002.25 2.25h15A2.25 2.25 0 0021.75 18V9a2.25 2.25 0 00-2.25-2.25h-5.379a1.5 1.5 0 01-1.06-.44z" />
  </svg>
);
const InformationCircleSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M11.25 11.25l.041-.02a.75.75 0 011.063.852l-.708 2.836a.75.75 0 001.063.853l.041-.021M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const PencilSquareSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M16.862 4.487l1.687-1.688a1.875 1.875 0 112.652 2.652L10.582 16.07a4.5 4.5 0 01-1.897 1.13L6 18l.8-2.685a4.5 4.5 0 011.13-1.897l8.932-8.931zm0 0L19.5 7.125M18 14v4.75A2.25 2.25 0 0115.75 21H5.25A2.25 2.25 0 013 18.75V8.25A2.25 2.25 0 015.25 6H10" />
  </svg>
);
const PlusCircleSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const SaveDiskSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M9 3.75H6.75A2.25 2.25 0 004.5 6v12a2.25 2.25 0 002.25 2.25h10.5A2.25 2.25 0 0019.5 18v-7.5a2.25 2.25 0 00-2.25-2.25h-3.75m-3.75 0V3.75m0 0h3.75m-3.75 0a2.25 2.25 0 012.25-2.25h1.5a2.25 2.25 0 012.25 2.25m-6 12v-6.75a.75.75 0 01.75-.75h3.75a.75.75 0 01.75.75v6.75" />
  </svg>
);
const SidebarRightSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 3.75H3.75A1.5 1.5 0 002.25 5.25v13.5A1.5 1.5 0 003.75 20.25h16.5a1.5 1.5 0 001.5-1.5V5.25a1.5 1.5 0 00-1.5-1.5z" />
    <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 3.75v16.5" />
  </svg>
);
const TableSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M3.375 19.5h17.25m-17.25 0a1.125 1.125 0 01-1.125-1.125v-12.75c0-.621.504-1.125 1.125-1.125h17.25c.621 0 1.125.504 1.125 1.125v12.75c0 .621-.504 1.125-1.125 1.125m-17.25 0h.008v.015h-.008V19.5zm.75-5.25h15.75m-15.75 0v.015h15.75V14.25m-15.75 0h.008v.015h-.008v-.015zm0-4.5h15.75m-15.75 0v.015h15.75V9.75m-15.75 0h.008v.015h-.008V9.75z" />
  </svg>
);
const TerminalSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M6.429 9.75L2.25 12l4.179 2.25m0-4.5l5.571 3 5.571-3m-11.142 0L12 15.25l5.571-3" />
    <path strokeLinecap="round" strokeLinejoin="round" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const TrashSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M14.74 9l-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 01-2.244 2.077H8.084a2.25 2.25 0 01-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 00-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 013.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 00-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 00-7.5 0" />
  </svg>
);
const ViewColumnsSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M9 4.5v15m6-15v15m-10.875 0h15.75c.621 0 1.125-.504 1.125-1.125V5.625c0-.621-.504-1.125-1.125-1.125H4.125C3.504 4.5 3 5.004 3 5.625v12.75c0 .621.504 1.125 1.125 1.125z" />
  </svg>
);
const XCircleSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M9.75 9.75l4.5 4.5m0-4.5l-4.5 4.5M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
  </svg>
);
const XMarkSharp: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
    <path strokeLinecap="round" strokeLinejoin="round" d="M6 18L18 6M6 6l12 12" />
  </svg>
);

// --- SOLID ICONS (Heroicons Solid) ---

const ArchiveBoxSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M2.25 2.25a.75.75 0 00-.75.75v12c0 .414.336.75.75.75h19.5a.75.75 0 00.75-.75v-12a.75.75 0 00-.75-.75H2.25zm1.5 1.5a.75.75 0 01.75-.75h15a.75.75 0 01.75.75v10.5a.75.75 0 01-.75.75h-15a.75.75 0 01-.75-.75V3.75z" clipRule="evenodd" />
    <path d="M8.25 7.5a.75.75 0 01.75.75h6a.75.75 0 010 1.5h-6a.75.75 0 01-.75-.75V8.25z" />
  </svg>
);
const ArrowPathSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M15.312 11.438c.422-.25.562-.812.312-1.234a.75.75 0 011.25-1.25c.813.813.938 2.063.313 2.813l-4.5 4.5c-.812.812-2.062.687-2.812-.312a.75.75 0 011.25-1.25c.422.422 1.062.562 1.5.312l4.688-2.563zm-12 2.812c-.422.25-.562.812-.312 1.234a.75.75 0 01-1.25 1.25c-.812-.812-.937-2.062-.312-2.812l4.5-4.5c.813-.813 2.063-.688 2.813.313a.75.75 0 01-1.25 1.25c-.422-.422-1.063-.562-1.5-.312L3.312 14.25zm12.188-9.75c.422.25.562.812.312 1.234a.75.75 0 01-1.25 1.25c-.812-.812-.937-2.062-.312-2.812l4.5-4.5c.813-.813 2.063-.688 2.813.313a.75.75 0 01-1.25 1.25c-.422-.422-1.063-.562-1.5-.312l-4.688 2.562zM3.312 5.25a.75.75 0 01-1.25-1.25C2.875 3.188 4.125 3.063 4.875 3.813l4.5 4.5c.813.812.688 2.062-.312 2.812a.75.75 0 01-1.25-1.25c.422-.422.562-1.062.312-1.5L3.312 5.25z" clipRule="evenodd" />
  </svg>
);
const ArrowsPointingInSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M9 3.75a.75.75 0 01.75.75v4.5a.75.75 0 01-1.5 0V5.31L3.53 9.78a.75.75 0 01-1.06-1.06L6.94 4.25H4.5a.75.75 0 010-1.5h4.5zm.75 11.25v4.5a.75.75 0 01-1.5 0V15a.75.75 0 011.5 0zm-1.5-6.75a.75.75 0 000 1.5h4.5a.75.75 0 000-1.5h-4.5zm-3.53-2.47a.75.75 0 001.06-1.06L4.25 2.06H6.75a.75.75 0 000-1.5H3a.75.75 0 00-.75.75v3.75a.75.75 0 001.5 0V3.56l3.22 3.22zm10.03 1.06a.75.75 0 01-1.06 1.06L12.56 6.94v2.56a.75.75 0 01-1.5 0V5.25a.75.75 0 01.75-.75h4.25a.75.75 0 010 1.5h-1.94z" clipRule="evenodd" />
  </svg>
);
const ArrowsPointingOutSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M1.5 6.75a.75.75 0 01.75-.75h3.75a.75.75 0 010 1.5H3.56l3.22 3.22a.75.75 0 01-1.06 1.06L2.25 8.31V10.5a.75.75 0 01-1.5 0v-3.75zm17.25 3.75a.75.75 0 011.5 0v3.75a.75.75 0 01-.75.75h-3.75a.75.75 0 010-1.5h2.56l-3.22-3.22a.75.75 0 011.06-1.06l3.47 3.47zM6.75 1.5a.75.75 0 01.75.75v3.75a.75.75 0 01-1.5 0V3.56l-3.22 3.22a.75.75 0 01-1.06-1.06L5.19 2.25H3a.75.75 0 010-1.5h3.75zm10.5 0a.75.75 0 01.75.75v3.75a.75.75 0 01-1.5 0V3.56l-3.22 3.22a.75.75 0 11-1.06-1.06l3.47-3.47z" clipRule="evenodd" />
  </svg>
);
const ArrowUpTraySolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M8.25 3.75a.75.75 0 01.75.75v6.586l1.22-1.22a.75.75 0 111.06 1.06l-2.5 2.5a.75.75 0 01-1.06 0l-2.5-2.5a.75.75 0 111.06-1.06l1.22 1.22V4.5a.75.75 0 01.75-.75z" clipRule="evenodd" />
    <path fillRule="evenodd" d="M3.75 13.5a.75.75 0 00-.75.75v4.5c0 .414.336.75.75.75h14.5a.75.75 0 00.75-.75v-4.5a.75.75 0 00-1.5 0v3.75h-13V14.25a.75.75 0 00-.75-.75z" clipRule="evenodd" />
  </svg>
);
const BookOpenSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25a.75.75 0 00-.75.75v18a.75.75 0 001.5 0V3a.75.75 0 00-.75-.75zM12.75 4.137a24.49 24.49 0 015.625 2.152.75.75 0 01.375.64V18.25a.75.75 0 01-1.125.651A22.95 22.95 0 0012.75 16.5v-12.363zM5.625 6.289a24.49 24.49 0 015.625-2.152.75.75 0 00-.75-1.338A25.99 25.99 0 003 6.375a.75.75 0 00-.75.75v11.192a.75.75 0 001.125.65A22.95 22.95 0 0111.25 16.5v-12.363a.75.75 0 00-.375-.648z" clipRule="evenodd" />
  </svg>
);
const BugAntSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M9.75 12c0 .966.394 1.844 1.03 2.478V21a.75.75 0 001.5 0v-6.522A3.98 3.98 0 0013.25 12c0-.966-.394-1.844-1.03-2.478V3a.75.75 0 00-1.5 0v6.522A3.98 3.98 0 009.75 12zM3 9.75A2.25 2.25 0 005.25 12c0 .966.394 1.844 1.03 2.478a.75.75 0 101.299-.756A2.484 2.484 0 016.75 12c0-.966-.394-1.844-1.03-2.478A.75.75 0 004.42 10.28 2.235 2.235 0 003 9.75zm18 0a2.25 2.25 0 01-2.25 2.25c-.966 0-1.844-.394-2.478-1.03a.75.75 0 00-1.299.756A3.984 3.984 0 0017.25 12c.966 0 1.844.394 2.478 1.03a.75.75 0 101.299-.756A2.484 2.484 0 0117.25 12a2.236 2.236 0 012.53-2.22A.75.75 0 0021 9.75z" clipRule="evenodd" />
  </svg>
);
const ChartBarSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M4.5 19.5a3 3 0 01-3-3V7.5a3 3 0 013-3h.75a.75.75 0 01.75.75v15a.75.75 0 01-.75.75h-.75z" />
    <path d="M11.25 19.5a3 3 0 01-3-3V4.5a3 3 0 013-3h.75a.75.75 0 01.75.75v18a.75.75 0 01-.75.75h-.75z" />
    <path d="M18 19.5a3 3 0 01-3-3V1.5a.75.75 0 01.75-.75h1.5a.75.75 0 01.75.75v17.25a.75.75 0 01-.75.75h-1.5a3 3 0 01-3-3h3z" />
  </svg>
);
const CheckCircleSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm13.36-1.814a.75.75 0 10-1.22-.872l-3.236 4.53L9.53 12.22a.75.75 0 00-1.06 1.06l2.25 2.25a.75.75 0 001.14-.094l3.75-5.25z" clipRule="evenodd" />
  </svg>
);
const ChevronDownSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12.53 16.28a.75.75 0 01-1.06 0l-7.5-7.5a.75.75 0 011.06-1.06L12 14.69l6.97-6.97a.75.75 0 111.06 1.06l-7.5 7.5z" clipRule="evenodd" />
  </svg>
);
const ChevronLeftSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M7.72 12.53a.75.75 0 010-1.06l7.5-7.5a.75.75 0 111.06 1.06L9.31 12l6.97 6.97a.75.75 0 11-1.06 1.06l-7.5-7.5z" clipRule="evenodd" />
  </svg>
);
const ChevronRightSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M16.28 11.47a.75.75 0 010 1.06l-7.5 7.5a.75.75 0 01-1.06-1.06L14.69 12 7.72 5.03a.75.75 0 011.06-1.06l7.5 7.5z" clipRule="evenodd" />
  </svg>
);
const ClipboardSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M10.5 3.75a.75.75 0 00-1.5 0V4.5h-1.5a.75.75 0 000 1.5h1.5v1.5a.75.75 0 001.5 0V6h1.5a.75.75 0 000-1.5h-1.5V3.75z" clipRule="evenodd" />
    <path fillRule="evenodd" d="M6 3.75A2.25 2.25 0 003.75 6v12A2.25 2.25 0 006 20.25h12A2.25 2.25 0 0020.25 18V6A2.25 2.25 0 0018 3.75H6zm-1.5 2.25a.75.75 0 01.75-.75h12a.75.75 0 01.75.75v12a.75.75 0 01-.75.75H6a.75.75 0 01-.75-.75V6z" clipRule="evenodd" />
  </svg>
);
const ClockSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM12.75 6a.75.75 0 00-1.5 0v6c0 .414.336.75.75.75h4.5a.75.75 0 000-1.5h-3.75V6z" clipRule="evenodd" />
  </svg>
);
const MoonSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
        <path fillRule="evenodd" d="M9.528 1.718a.75.75 0 01.162.819A8.97 8.97 0 009 6a9 9 0 009 9 8.97 8.97 0 003.463-.69.75.75 0 01.981.98 10.503 10.503 0 01-9.694 6.46c-5.799 0-10.5-4.7-10.5-10.5 0-3.51 1.713-6.636 4.362-8.467a.75.75 0 01.819.162z" clipRule="evenodd" />
    </svg>
);
const SunSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
        <path d="M12 2.25a.75.75 0 01.75.75v2.25a.75.75 0 01-1.5 0V3a.75.75 0 01.75-.75zM7.5 12a4.5 4.5 0 119 0 4.5 4.5 0 01-9 0zM18.894 6.106a.75.75 0 010 1.06l-1.591 1.591a.75.75 0 11-1.06-1.06l1.591-1.591a.75.75 0 011.06 0zm-11.483 0a.75.75 0 011.06 0l1.591 1.591a.75.75 0 11-1.06 1.06L6.106 7.166a.75.75 0 010-1.06zM21.75 12a.75.75 0 01-.75.75h-2.25a.75.75 0 010-1.5h2.25a.75.75 0 01.75.75zM3.75 12a.75.75 0 01.75-.75h2.25a.75.75 0 010 1.5H4.5a.75.75 0 01-.75-.75zM17.303 17.303a.75.75 0 01-1.06 0l-1.591-1.591a.75.75 0 111.06-1.06l1.591 1.591a.75.75 0 010 1.06zM7.166 18.894a.75.75 0 010-1.06l1.591-1.591a.75.75 0 111.06 1.06l-1.591 1.591a.75.75 0 01-1.06 0zM12 18a.75.75 0 01.75.75v2.25a.75.75 0 01-1.5 0v-2.25A.75.75 0 0112 18z" />
    </svg>
);
const SaveSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
        <path d="M3.375 3C2.339 3 1.5 3.84 1.5 4.875v14.25C1.5 20.16 2.339 21 3.375 21h17.25c1.035 0 1.875-.84 1.875-1.875V4.875C22.5 3.839 21.66 3 20.625 3H3.375z" />
        <path fillRule="evenodd" d="M9 16.5a.75.75 0 00.75-.75V12a.75.75 0 00-1.5 0v3.75a.75.75 0 00.75.75z" clipRule="evenodd" />
        <path fillRule="evenodd" d="M12.75 16.5a.75.75 0 00.75-.75V12a.75.75 0 00-1.5 0v3.75a.75.75 0 00.75.75z" clipRule="evenodd" />
        <path fillRule="evenodd" d="M16.5 16.5a.75.75 0 00.75-.75V12a.75.75 0 00-1.5 0v3.75a.75.75 0 00.75.75z" clipRule="evenodd" />
    </svg>
);


// --- FEATHER ICONS (from feathericons.com) ---
// Not implemented

// --- TABLER ICONS (from tabler-icons.io) ---
// Not implemented

// --- LUCIDE ICONS (from lucide.dev) ---
// Not implemented


// --- Icon Maps ---

const sharpIcons = {
    ArchiveBox: ArchiveBoxSharp,
    ArrowPath: ArrowPathSharp,
    ArrowsPointingIn: ArrowsPointingInSharp,
    ArrowsPointingOut: ArrowsPointingOutSharp,
    ArrowUpTray: ArrowUpTraySharp,
    BookOpen: BookOpenSharp,
    BugAnt: BugAntSharp,
    ChartBar: ChartBarSharp,
    CheckCircle: CheckCircleSharp,
    ChevronDown: ChevronDownSharp,
    ChevronLeft: ChevronLeftSharp,
    ChevronRight: ChevronRightSharp,
    Clipboard: ClipboardSharp,
    Clock: ClockSharp,
    CodeBracket: CodeBracketSharp,
    CodeBracketSquare: CodeBracketSquareSharp,
    Cog: CogSharp,
    Database: DatabaseSharp,
    DocumentPlus: DocumentPlusSharp,
    Download: DownloadSharp,
    ExclamationTriangle: ExclamationTriangleSharp,
    File: FileSharp,
    Filter: FilterSharp,
    Folder: FolderSharp,
    InformationCircle: InformationCircleSharp,
    PencilSquare: PencilSquareSharp,
    PlusCircle: PlusCircleSharp,
    Save: SaveDiskSharp, // Alias
    SaveDisk: SaveDiskSharp,
    SidebarRight: SidebarRightSharp,
    Table: TableSharp,
    Terminal: TerminalSharp,
    Trash: TrashSharp,
    ViewColumns: ViewColumnsSharp,
    XCircle: XCircleSharp,
    XMark: XMarkSharp,
    Sun: SunIcon,
    Moon: MoonIcon,
};

const solidIcons = {
    ArchiveBox: ArchiveBoxSolid,
    ArrowPath: ArrowPathSolid,
    ArrowsPointingIn: ArrowsPointingInSolid,
    ArrowsPointingOut: ArrowsPointingOutSolid,
    ArrowUpTray: ArrowUpTraySolid,
    BookOpen: BookOpenSolid,
    BugAnt: BugAntSolid,
    ChartBar: ChartBarSolid,
    CheckCircle: CheckCircleSolid,
    ChevronDown: ChevronDownSolid,
    ChevronLeft: ChevronLeftSolid,
    ChevronRight: ChevronRightSolid,
    Clipboard: ClipboardSolid,
    Clock: ClockSolid,
    Sun: SunSolid,
    Moon: MoonSolid,
    Save: SaveSolid,
    SaveDisk: SaveSolid,
    // Other solid icons would go here... we fallback to sharp.
};

const iconMap = {
    sharp: sharpIcons,
    solid: solidIcons,
    feather: {}, // Not implemented
    tabler: {}, // Not implemented
    lucide: {}, // Not implemented
};

export type IconName = keyof typeof sharpIcons;

interface IconProps extends React.SVGProps<SVGSVGElement> {
    name: IconName;
    iconSet: IconSet;
}

export const Icon: React.FC<IconProps> = ({ name, iconSet, ...props }) => {
    // @ts-ignore - This is a safe way to handle dynamic component selection.
    const IconComponent = iconMap[iconSet]?.[name] || sharpIcons[name];
    if (!IconComponent) {
        // Fallback to a default icon or null if a name is somehow invalid
        const FallbackIcon = XCircleSharp;
        return <FallbackIcon {...props} />;
    }
    return <IconComponent {...props} />;
};