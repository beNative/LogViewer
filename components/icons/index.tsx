import React from 'react';
import { IconSet } from '../types.ts';

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
const ClockSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM12.75 6a.75.75 0 00-1.5 0v6c0 .414.336.75.75.75h4.5a.75.75 0 000-1.5h-3.75V6z" clipRule="evenodd" />
  </svg>
);
const CodeBracketSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M9.622 3.205a.75.75 0 01.259 1.038L5.27 10.5h13.46l-4.612-6.257a.75.75 0 011.298-.764l5.25 7.125a.75.75 0 010 .764l-5.25 7.125a.75.75 0 01-1.298-.764L18.73 13.5H5.27l4.611 6.257a.75.75 0 01-1.298.764l-5.25-7.125a.75.75 0 010-.764l5.25-7.125a.75.75 0 011.039-.258z" clipRule="evenodd" />
  </svg>
);
const CodeBracketSquareSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M3 3.75A.75.75 0 013.75 3h16.5a.75.75 0 01.75.75v16.5a.75.75 0 01-.75.75H3.75a.75.75 0 01-.75-.75V3.75zm5.78 12.97a.75.75 0 01-1.06-1.06L4.939 12l2.781-2.781a.75.75 0 111.06 1.06L7.061 12l2.78 2.72a.75.75 0 010 1.06zm8.69-1.06a.75.75 0 001.06-1.06L16.219 12l2.781-2.781a.75.75 0 00-1.06-1.06L15.159 12l-2.78 2.72a.75.75 0 101.06 1.06z" clipRule="evenodd" />
  </svg>
);
const CogSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M11.078 2.25c-.917 0-1.699.663-1.85 1.542A12.034 12.034 0 006.36 5.882c-.97.234-1.595 1.178-1.423 2.152a11.94 11.94 0 001.002 2.682c.28.95.996 1.623 1.944 1.766a11.94 11.94 0 003.116 1.018c.95.28 1.623.996 1.766 1.944a11.94 11.94 0 001.018 3.116c.143.948.816 1.674 1.766 1.944a11.94 11.94 0 002.682 1.002c.974.172 1.918-.452 2.152-1.423a12.034 12.034 0 001.638-3.32c.152-.85.663-1.699 1.542-1.85a11.94 11.94 0 003.32-1.638c.97-.234 1.595-1.178 1.423-2.152a11.94 11.94 0 00-1.002-2.682c-.28-.95-.996-1.623-1.944-1.766a11.94 11.94 0 00-3.116-1.018c-.95-.28-1.623-.996-1.766-1.944A11.94 11.94 0 0016.96 6.36c-.143-.948-.816-1.674-1.766-1.944a11.94 11.94 0 00-2.682-1.002c-.974-.172-1.918.452-2.152 1.423A12.034 12.034 0 009.12 6.36c-.152.85-.663 1.699-1.542 1.85a11.94 11.94 0 00-3.32 1.638c-.97.234-1.595 1.178-1.423 2.152a11.94 11.94 0 001.002 2.682c.28.95.996 1.623 1.944 1.766a11.94 11.94 0 003.116 1.018c.95.28 1.623.996 1.766 1.944a11.94 11.94 0 001.018 3.116c.143.948.816 1.674 1.766 1.944a11.94 11.94 0 002.682 1.002c.974.172 1.918-.452 2.152-1.423a12.034 12.034 0 001.638-3.32c.152-.85.663-1.699 1.542-1.85a11.94 11.94 0 003.32-1.638c.97-.234 1.595-1.178-1.423-2.152a11.94 11.94 0 00-1.002-2.682c-.28-.95-.996-1.623-1.944-1.766a11.94 11.94 0 00-3.116-1.018c-.95-.28-1.623-.996-1.766-1.944A11.94 11.94 0 0016.96 6.36c-.143-.948-.816-1.674-1.766-1.944a11.94 11.94 0 00-2.682-1.002c-.974-.172-1.918.452-2.152 1.423A12.034 12.034 0 009.12 6.36c-.85.152-1.699.663-1.85 1.542a11.94 11.94 0 00-1.638 3.32zM12 15.75a3.75 3.75 0 100-7.5 3.75 3.75 0 000 7.5z" clipRule="evenodd" />
  </svg>
);
const DatabaseSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M3.75 4.5A2.25 2.25 0 016 2.25h12A2.25 2.25 0 0120.25 4.5v15A2.25 2.25 0 0118 21.75H6A2.25 2.25 0 013.75 19.5v-15zM8.25 6a.75.75 0 000 1.5h7.5a.75.75 0 000-1.5h-7.5zM8.25 11.25a.75.75 0 000 1.5h7.5a.75.75 0 000-1.5h-7.5zM8.25 16.5a.75.75 0 000 1.5h4.5a.75.75 0 000-1.5h-4.5z" />
  </svg>
);
const DocumentPlusSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M3.75 2.25a.75.75 0 00-1.5 0v1.5c0 .414.336.75.75.75h1.5a.75.75 0 00.75-.75V3a.75.75 0 00-.75-.75zm3.75 0a.75.75 0 00-1.5 0v1.5c0 .414.336.75.75.75h1.5a.75.75 0 00.75-.75V3a.75.75 0 00-.75-.75h-1.5zm3.75 0a.75.75 0 00-1.5 0v1.5c0 .414.336.75.75.75h1.5a.75.75 0 00.75-.75V3a.75.75 0 00-.75-.75h-1.5zM3 7.5a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H3.75A.75.75 0 013 7.5zm4.5 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H8.25a.75.75 0 01-.75-.75zm4.5 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zm-9 3.75a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H3.75a.75.75 0 01-.75-.75zm3.75 0a.75.75 0 00-1.5 0v1.5c0 .414.336.75.75.75h1.5a.75.75 0 00.75-.75v-1.5a.75.75 0 00-.75-.75h-1.5z" clipRule="evenodd" />
    <path d="M14.25 11.25a.75.75 0 00-1.5 0v1.5a.75.75 0 001.5 0v-1.5zM15 9.75a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zm.75 3.75a.75.75 0 00-1.5 0v1.5a.75.75 0 001.5 0v-1.5zM18.75 9a.75.75 0 00-.75.75v1.5a.75.75 0 001.5 0v-1.5a.75.75 0 00-.75-.75zm-.75 3.75a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75z" />
    <path fillRule="evenodd" d="M3 15.75a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H3.75a.75.75 0 01-.75-.75zm3.75 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H8.25a.75.75 0 01-.75-.75zm3.75 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zm4.5 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zm-9 3.75a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H3.75a.75.75 0 01-.75-.75zm3.75 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5H8.25a.75.75 0 01-.75-.75zm3.75 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zm4.5 0a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75z" clipRule="evenodd" />
  </svg>
);
const DownloadSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25a.75.75 0 01.75.75v11.69l3.22-3.22a.75.75 0 111.06 1.06l-4.5 4.5a.75.75 0 01-1.06 0l-4.5-4.5a.75.75 0 111.06-1.06l3.22 3.22V3a.75.75 0 01.75-.75z" clipRule="evenodd" />
    <path fillRule="evenodd" d="M3.75 13.5a.75.75 0 00-.75.75v4.5c0 .414.336.75.75.75h14.5a.75.75 0 00.75-.75v-4.5a.75.75 0 00-1.5 0v3.75h-13V14.25a.75.75 0 00-.75-.75z" clipRule="evenodd" />
  </svg>
);
const ExclamationTriangleSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M9.401 3.003c1.155-2 4.043-2 5.197 0l7.355 12.748c1.154 2-.29 4.5-2.599 4.5H4.645c-2.309 0-3.752-2.5-2.598-4.5L9.4 3.003zM12 8.25a.75.75 0 01.75.75v3.75a.75.75 0 01-1.5 0V9a.75.75 0 01.75-.75zm0 8.25a.75.75 0 100-1.5.75.75 0 000 1.5z" clipRule="evenodd" />
  </svg>
);
const FileSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M5.625 1.5c-1.036 0-1.875.84-1.875 1.875v17.25c0 1.035.84 1.875 1.875 1.875h12.75c1.035 0 1.875-.84 1.875-1.875V12.75A3.75 3.75 0 0016.5 9h-1.875a.375.375 0 01-.375-.375V6.75A3.75 3.75 0 0010.5 3H5.625z" />
    <path d="M12.971 1.816A5.23 5.23 0 0114.25 1.5a.75.75 0 01.75.75v4.5c0 .414.336.75.75.75h4.5a.75.75 0 01.75.75 5.23 5.23 0 01-.316 1.471 1.5 1.5 0 01-2.122 1.06l-6.838-3.419a1.5 1.5 0 010-2.682l6.838-3.419a1.5 1.5 0 012.122 1.06z" />
  </svg>
);
const FilterSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M3.792 2.938A49.069 49.069 0 0112 2.25c2.797 0 5.54.236 8.209.688a1.857 1.857 0 011.541 1.836v1.044a3 3 0 01-.879 2.121l-6.182 6.182a1.5 1.5 0 00-.439 1.061v2.927a8.25 8.25 0 01-4.212 7.279l-1.588.794A.75.75 0 018.25 21v-4.212a1.5 1.5 0 00-.44-1.06L1.631 8.53a3 3 0 01-.879-2.121V5.378c0-.857.575-1.581 1.341-1.78a49.009 49.009 0 011.699-.452z" clipRule="evenodd" />
  </svg>
);
const FolderSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M19.5 21a3 3 0 003-3v-8.25a3 3 0 00-3-3h-13.5a3 3 0 00-3 3V18a3 3 0 003 3h13.5z" />
    <path d="M1.5 9.75a3 3 0 013-3h15a3 3 0 013 3v.25a3 3 0 01-3 3H4.5a3 3 0 01-3-3V9.75z" />
  </svg>
);
const InformationCircleSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm8.706-1.442c1.146-.573 2.437.463 2.126 1.706l-.709 2.836.042-.02a.75.75 0 01.671.928l-.247 2.471a.75.75 0 11-1.451-.29l.247-2.471a.75.75 0 11.794-1.012l-.709-2.836a.75.75 0 00-1.706-.442 4.5 4.5 0 00-1.722 5.23l-.163.543a.75.75 0 01-1.284-.57l.163-.543a6 6 0 012.28-6.97zM12 6a.75.75 0 01.75.75v.008a.75.75 0 01-1.5 0V6.75A.75.75 0 0112 6z" clipRule="evenodd" />
  </svg>
);
const PencilSquareSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M21.731 2.269a2.625 2.625 0 00-3.712 0l-1.157 1.157 3.712 3.712 1.157-1.157a2.625 2.625 0 000-3.712zM19.513 8.199l-3.712-3.712-12.15 12.15a5.25 5.25 0 00-1.32 2.214l-.8 2.685a.75.75 0 00.933.933l2.685-.8a5.25 5.25 0 002.214-1.32L19.513 8.2z" />
  </svg>
);
const PlusCircleSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM12.75 9a.75.75 0 00-1.5 0v2.25H9a.75.75 0 000 1.5h2.25V15a.75.75 0 001.5 0v-2.25H15a.75.75 0 000-1.5h-2.25V9z" clipRule="evenodd" />
  </svg>
);
const SaveDiskSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M3.375 3C2.339 3 1.5 3.84 1.5 4.875v14.25C1.5 20.16 2.34 21 3.375 21h17.25c1.035 0 1.875-.84 1.875-1.875V4.875C22.5 3.839 21.66 3 20.625 3H3.375z" />
    <path fillRule="evenodd" d="M9 12.75a.75.75 0 000 1.5h6a.75.75 0 000-1.5H9zM10.5 15a.75.75 0 01.75-.75h1.5a.75.75 0 010 1.5h-1.5a.75.75 0 01-.75-.75zM5.25 6a.75.75 0 01.75-.75h12a.75.75 0 010 1.5H6a.75.75 0 01-.75-.75z" clipRule="evenodd" />
  </svg>
);
const SidebarRightSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M2.25 3.75A1.5 1.5 0 013.75 2.25h16.5a1.5 1.5 0 011.5 1.5v16.5a1.5 1.5 0 01-1.5 1.5H3.75a1.5 1.5 0 01-1.5-1.5V3.75z" />
    <path d="M15.75 3.75a.75.75 0 00-1.5 0v16.5a.75.75 0 001.5 0V3.75z" />
  </svg>
);
const TableSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path d="M12.75 3.375a.75.75 0 00-1.5 0V4.5h-2.25a.75.75 0 000 1.5h2.25v2.25a.75.75 0 001.5 0V6h2.25a.75.75 0 000-1.5H12.75V3.375z" />
    <path fillRule="evenodd" d="M3 10.5a.75.75 0 01.75-.75h16.5a.75.75 0 010 1.5H3.75a.75.75 0 01-.75-.75zm0 6a.75.75 0 01.75-.75h16.5a.75.75 0 010 1.5H3.75a.75.75 0 01-.75-.75z" clipRule="evenodd" />
    <path d="M4.5 21a3 3 0 003-3V6a3 3 0 00-3-3H4.5a3 3 0 00-3 3v12a3 3 0 003 3h3zm12-18a3 3 0 013 3v12a3 3 0 01-3 3h-3a3 3 0 01-3-3V6a3 3 0 013-3h3z" />
  </svg>
);
const TerminalSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm8.26 2.39a.75.75 0 001.06.04l4.5-3.75a.75.75 0 000-1.18l-4.5-3.75a.75.75 0 00-1.06 1.18L13.19 12l-2.67 2.22a.75.75 0 00.04 1.18zM7.5 15.75a.75.75 0 01.75-.75h.008a.75.75 0 01.75.75v.008a.75.75 0 01-1.5 0V15.75z" clipRule="evenodd" />
  </svg>
);
const TrashSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M16.5 4.478v.227a48.816 48.816 0 013.878.512.75.75 0 11-.256 1.478l-.209-.035-1.005 13.07a3 3 0 01-2.991 2.77H8.084a3 3 0 01-2.991-2.77L4.087 6.66l-.209.035a.75.75 0 01-.256-1.478A48.567 48.567 0 017.5 4.705v-.227c0-1.564 1.213-2.9 2.816-2.951a52.662 52.662 0 013.369 0c1.603.051 2.815 1.387 2.815 2.951zm-6.136-1.452a51.196 51.196 0 013.273 0C14.39 3.05 15 3.684 15 4.478v.113a49.488 49.488 0 00-6 0v-.113c0-.794.609-1.428 1.364-1.452z" clipRule="evenodd" />
  </svg>
);
const ViewColumnsSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M8.25 3a.75.75 0 01.75.75v16.5a.75.75 0 01-1.5 0V3.75A.75.75 0 018.25 3zM15.75 3a.75.75 0 01.75.75v16.5a.75.75 0 01-1.5 0V3.75a.75.75 0 01.75-.75z" clipRule="evenodd" />
    <path d="M3 3.75A.75.75 0 013.75 3h16.5a.75.75 0 01.75.75v16.5a.75.75 0 01-.75.75H3.75a.75.75 0 01-.75-.75V3.75z" />
  </svg>
);
const XCircleSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zm-1.72 6.97a.75.75 0 10-1.06 1.06L10.94 12l-1.72 1.72a.75.75 0 101.06 1.06L12 13.06l1.72 1.72a.75.75 0 101.06-1.06L13.06 12l1.72-1.72a.75.75 0 10-1.06-1.06L12 10.94l-1.72-1.72z" clipRule="evenodd" />
  </svg>
);
const XMarkSolid: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" {...props}>
    <path fillRule="evenodd" d="M5.47 5.47a.75.75 0 011.06 0L12 10.94l5.47-5.47a.75.75 0 111.06 1.06L13.06 12l5.47 5.47a.75.75 0 11-1.06 1.06L12 13.06l-5.47 5.47a.75.75 0 01-1.06-1.06L10.94 12 5.47 6.53a.75.75 0 010-1.06z" clipRule="evenodd" />
  </svg>
);

// --- FEATHER ICONS (Feathericons.com) ---

const FeatherIconWrapper: React.FC<{ children: React.ReactNode } & React.SVGProps<SVGSVGElement>> = ({ children, ...props }) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" strokeWidth={1.5} stroke="currentColor" strokeLinecap="round" strokeLinejoin="round" {...props}>
    {children}
  </svg>
);

const ArchiveBoxFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="21 8 21 21 3 21 3 8"></polyline><rect x="1" y="3" width="22" height="5"></rect><line x1="10" y1="12" x2="14" y2="12"></line></FeatherIconWrapper>
);
const ArrowPathFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="23 4 23 10 17 10"></polyline><polyline points="1 20 1 14 7 14"></polyline><path d="M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15"></path></FeatherIconWrapper>
);
const ArrowsPointingInFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="4 14 10 14 10 20"></polyline><polyline points="20 10 14 10 14 4"></polyline><line x1="14" y1="10" x2="21" y2="3"></line><line x1="3" y1="21" x2="10" y2="14"></line></FeatherIconWrapper>
);
const ArrowsPointingOutFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="15 3 21 3 21 9"></polyline><polyline points="9 21 3 21 3 15"></polyline><line x1="21" y1="3" x2="14" y2="10"></line><line x1="3" y1="21" x2="10" y2="14"></line></FeatherIconWrapper>
);
const ArrowUpTrayFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="17 8 12 3 7 8"></polyline><line x1="12" y1="3" x2="12" y2="15"></line></FeatherIconWrapper>
);
const BookOpenFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z"></path><path d="M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"></path></FeatherIconWrapper>
);
const BugAntFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M12 20a8 8 0 0 0 8-8V7a4 4 0 0 0-8 0v5a4 4 0 0 0-8 0v1a8 8 0 0 0 8 8Z"></path><path d="m18 8-4.75 4.75"></path><path d="m6 8 4.75 4.75"></path><path d="M12 16.5V22"></path><path d="M12 2v2"></path><path d="M15 2.5 12 5 9 2.5"></path><path d="m20 7-3 5"></path><path d="m4 7 3 5"></path></FeatherIconWrapper>
);
const ChartBarFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><line x1="12" y1="20" x2="12" y2="10"></line><line x1="18" y1="20" x2="18" y2="4"></line><line x1="6" y1="20" x2="6" y2="16"></line></FeatherIconWrapper>
);
const CheckCircleFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"></path><polyline points="22 4 12 14.01 9 11.01"></polyline></FeatherIconWrapper>
);
const ChevronDownFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="6 9 12 15 18 9"></polyline></FeatherIconWrapper>
);
const ChevronLeftFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="15 18 9 12 15 6"></polyline></FeatherIconWrapper>
);
const ChevronRightFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="9 18 15 12 9 6"></polyline></FeatherIconWrapper>
);
const ClockFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><circle cx="12" cy="12" r="10"></circle><polyline points="12 6 12 12 16 14"></polyline></FeatherIconWrapper>
);
const CodeBracketFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></FeatherIconWrapper>
);
const CodeBracketSquareFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect><polyline points="10 8 14 12 10 16"></polyline></FeatherIconWrapper>
);
const CogFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><circle cx="12" cy="12" r="3"></circle><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z"></path></FeatherIconWrapper>
);
const DatabaseFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><ellipse cx="12" cy="5" rx="9" ry="3"></ellipse><path d="M21 12c0 1.66-4 3-9 3s-9-1.34-9-3"></path><path d="M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5"></path></FeatherIconWrapper>
);
const DocumentPlusFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path><polyline points="14 2 14 8 20 8"></polyline><line x1="12" y1="18" x2="12" y2="12"></line><line x1="9" y1="15" x2="15" y2="15"></line></FeatherIconWrapper>
);
const DownloadFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path><polyline points="7 10 12 15 17 10"></polyline><line x1="12" y1="15" x2="12" y2="3"></line></FeatherIconWrapper>
);
const ExclamationTriangleFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"></path><line x1="12" y1="9" x2="12" y2="13"></line><line x1="12" y1="17" x2="12.01" y2="17"></line></FeatherIconWrapper>
);
const FileFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M13 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V9z"></path><polyline points="13 2 13 9 20 9"></polyline></FeatherIconWrapper>
);
const FilterFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polygon points="22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3"></polygon></FeatherIconWrapper>
);
const FolderFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z"></path></FeatherIconWrapper>
);
const InformationCircleFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><circle cx="12" cy="12" r="10"></circle><line x1="12" y1="16" x2="12" y2="12"></line><line x1="12" y1="8" x2="12.01" y2="8"></line></FeatherIconWrapper>
);
const PencilSquareFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M12 20h9"></path><path d="M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path></FeatherIconWrapper>
);
const PlusCircleFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><circle cx="12" cy="12" r="10"></circle><line x1="12" y1="8" x2="12" y2="16"></line><line x1="8" y1="12" x2="16" y2="12"></line></FeatherIconWrapper>
);
const SaveDiskFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z"></path><polyline points="17 21 17 13 7 13 7 21"></polyline><polyline points="7 3 7 8 15 8"></polyline></FeatherIconWrapper>
);
const SidebarRightFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect><line x1="15" y1="3" x2="15" y2="21"></line></FeatherIconWrapper>
);
const TableFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><path d="M9 3H5a2 2 0 0 0-2 2v4m6-6h10a2 2 0 0 1 2 2v4M9 3v18m0 0h10a2 2 0 0 0 2-2V9M9 21H5a2 2 0 0 1-2-2V9m0 0h18"></path></FeatherIconWrapper>
);
const TerminalFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="4 17 10 11 4 5"></polyline><line x1="12" y1="19" x2="20" y2="19"></line></FeatherIconWrapper>
);
const TrashFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><polyline points="3 6 5 6 21 6"></polyline><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path><line x1="10" y1="11" x2="10" y2="17"></line><line x1="14" y1="11" x2="14" y2="17"></line></FeatherIconWrapper>
);
const ViewColumnsFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect><line x1="9" y1="3" x2="9" y2="21"></line><line x1="15" y1="3" x2="15" y2="21"></line></FeatherIconWrapper>
);
const XCircleFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><circle cx="12" cy="12" r="10"></circle><line x1="15" y1="9" x2="9" y2="15"></line><line x1="9" y1="9" x2="15" y2="15"></line></FeatherIconWrapper>
);
const XMarkFeather: React.FC<React.SVGProps<SVGSVGElement>> = (props) => (
  <FeatherIconWrapper {...props}><line x1="18" y1="6" x2="6" y2="18"></line><line x1="6" y1="6" x2="18" y2="18"></line></FeatherIconWrapper>
);

// --- TABLER ICONS (tabler-icons.io) ---

const TablerIconWrapper: React.FC<{ children: React.ReactNode } & React.SVGProps<SVGSVGElement>> = ({ children, ...props }) => (
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" fill="none" strokeLinecap="round" strokeLinejoin="round" {...props}>
    <path stroke="none" d="M0 0h24v24H0z" fill="none"/>
    {children}
  </svg>
);

const ArchiveBoxTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M8 8h8v2a2 2 0 0 1 -2 2h-4a2 2 0 0 1 -2 -2v-2z" /><path d="M4 8v10a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-10" /><path d="M10 12h4" /></TablerIconWrapper> );
const ArrowPathTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M19.933 13.041a8 8 0 1 1 -9.925 -8.788c3.899 -1 7.935 1.007 9.425 4.747" /><path d="M20 4v5h-5" /></TablerIconWrapper> );
const ArrowsPointingInTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 10h6v-6" /><path d="M20 14h-6v6" /><path d="M10 4l-6 6" /><path d="M14 20l6 -6" /></TablerIconWrapper> );
const ArrowsPointingOutTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 14h6v-6" /><path d="M20 10h-6v6" /><path d="M14 4l6 6" /><path d="M10 20l-6 -6" /></TablerIconWrapper> );
const ArrowUpTrayTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2" /><path d="M7 9l5 -5l5 5" /><path d="M12 4l0 12" /></TablerIconWrapper> );
const BookOpenTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M3 19a9 9 0 0 1 9 0a9 9 0 0 1 9 0" /><path d="M3 6a9 9 0 0 1 9 0a9 9 0 0 1 9 0" /><path d="M3 6l0 13" /><path d="M12 6l0 13" /><path d="M21 6l0 13" /></TablerIconWrapper> );
const BugAntTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M9 9v-1a3 3 0 0 1 6 0v1" /><path d="M8 9h8a6 6 0 0 1 6 6v3h-19v-3a6 6 0 0 1 6 -6" /><path d="M3 13v-1a3 3 0 0 1 3 -3" /><path d="M21 13v-1a3 3 0 0 0 -3 -3" /><path d="M12 20l0 -6" /><path d="M4 18l4 -2" /><path d="M20 18l-4 -2" /><path d="M8 7l1 -4" /><path d="M16 7l-1 -4" /></TablerIconWrapper> );
const ChartBarTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M3 12m0 1a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v6a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1z" /><path d="M9 8m0 1a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v10a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1z" /><path d="M15 4m0 1a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v14a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1z" /><path d="M4 20l14 0" /></TablerIconWrapper> );
const CheckCircleTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /><path d="M9 12l2 2l4 -4" /></TablerIconWrapper> );
const ChevronDownTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M6 9l6 6l6 -6" /></TablerIconWrapper> );
const ChevronLeftTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M15 6l-6 6l6 6" /></TablerIconWrapper> );
const ChevronRightTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M9 6l6 6l-6 6" /></TablerIconWrapper> );
const ClockTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /><path d="M12 12l0 -3.5" /><path d="M16.5 16.5l-4.5 -4.5" /></TablerIconWrapper> );
const CodeBracketTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M7 8l-4 4l4 4" /><path d="M17 8l4 4l-4 4" /><path d="M14 4l-4 16" /></TablerIconWrapper> );
const CodeBracketSquareTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M7 4a2 2 0 0 0 -2 2v3a2 3 0 0 1 -2 3a2 3 0 0 1 2 3v3a2 2 0 0 0 2 2" /><path d="M17 4a2 2 0 0 1 2 2v3a2 3 0 0 0 2 3a2 3 0 0 0 -2 3v3a2 2 0 0 1 -2 2" /></TablerIconWrapper> );
const CogTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M10.325 4.317c.426 -1.756 2.924 -1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543 -.94 3.31 .826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756 .426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543 -.826 3.31 -2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756 -2.924 1.756 -3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543 .94 -3.31 -.826 -2.37 -2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756 -.426 -1.756 -2.924 0 -3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94 -1.543 .826 -3.31 2.37 -2.37c1 .608 2.296 .07 2.572 -1.065z" /><path d="M12 12m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0" /></TablerIconWrapper> );
const DatabaseTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 6m-8 0a8 3 0 1 0 16 0a8 3 0 1 0 -16 0" /><path d="M4 6v6a8 3 0 0 0 16 0v-6" /><path d="M4 12v6a8 3 0 0 0 16 0v-6" /></TablerIconWrapper> );
const DocumentPlusTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M14 3v4a1 1 0 0 0 1 1h4" /><path d="M17 21h-10a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h7l5 5v11a2 2 0 0 1 -2 2z" /><path d="M12 11l0 6" /><path d="M9 14l6 0" /></TablerIconWrapper> );
const DownloadTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 17v2a2 2 0 0 0 2 2h12a2 2 0 0 0 2 -2v-2" /><path d="M7 11l5 5l5 -5" /><path d="M12 4l0 12" /></TablerIconWrapper> );
const ExclamationTriangleTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 9v4" /><path d="M10.24 3.957l-8.407 14.286a1.914 1.914 0 0 0 1.643 2.857h16.813a1.914 1.914 0 0 0 1.643 -2.857l-8.407 -14.286a1.914 1.914 0 0 0 -3.286 0z" /><path d="M12 17h.01" /></TablerIconWrapper> );
const FileTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M14 3v4a1 1 0 0 0 1 1h4" /><path d="M17 21h-10a2 2 0 0 1 -2 -2v-14a2 2 0 0 1 2 -2h7l5 5v11a2 2 0 0 1 -2 2z" /></TablerIconWrapper> );
const FilterTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 4h16v2.172a2 2 0 0 1 -.586 1.414l-4.414 4.414v7l-6 2v-8.5l-4.48 -4.928a2 2 0 0 1 -.52 -1.345v-2.217z" /></TablerIconWrapper> );
const FolderTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M5 4h4l3 3h7a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-11a2 2 0 0 1 2 -2" /></TablerIconWrapper> );
const InformationCircleTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /><path d="M12 9h.01" /><path d="M11 12h1v4h1" /></TablerIconWrapper> );
const PencilSquareTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M7 7h-1a2 2 0 0 0 -2 2v9a2 2 0 0 0 2 2h9a2 2 0 0 0 2 -2v-1" /><path d="M20.385 6.585a2.1 2.1 0 0 0 -2.97 -2.97l-8.415 8.385v3h3l8.385 -8.415z" /><path d="M16 5l3 3" /></TablerIconWrapper> );
const PlusCircleTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /><path d="M9 12l6 0" /><path d="M12 9l0 6" /></TablerIconWrapper> );
const SaveDiskTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M6 4h10l4 4v10a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2v-12a2 2 0 0 1 2 -2" /><path d="M12 14m-2 0a2 2 0 1 0 4 0a2 2 0 1 0 -4 0" /><path d="M14 4l0 4l-6 0l0 -4" /></TablerIconWrapper> );
const SidebarRightTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 4m0 2a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2z" /><path d="M15 4l0 16" /></TablerIconWrapper> );
const TableTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M3 5a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-14z" /><path d="M3 10h18" /><path d="M10 3v18" /></TablerIconWrapper> );
const TerminalTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M8 9l3 3l-3 3" /><path d="M13 15l0 -6" /><path d="M3 4m0 2a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2z" /></TablerIconWrapper> );
const TrashTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 7l16 0" /><path d="M10 11l0 6" /><path d="M14 11l0 6" /><path d="M5 7l1 12a2 2 0 0 0 2 2h8a2 2 0 0 0 2 -2l1 -12" /><path d="M9 7v-3a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v3" /></TablerIconWrapper> );
const ViewColumnsTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M4 4m0 2a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2z" /><path d="M12 4l0 16" /></TablerIconWrapper> );
const XCircleTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M12 12m-9 0a9 9 0 1 0 18 0a9 9 0 1 0 -18 0" /><path d="M10 10l4 4m0 -4l-4 4" /></TablerIconWrapper> );
const XMarkTabler: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <TablerIconWrapper {...props}><path d="M18 6l-12 12" /><path d="M6 6l12 12" /></TablerIconWrapper> );

// --- LUCIDE ICONS (lucide.dev) ---

const LucideIconWrapper: React.FC<{ children: React.ReactNode } & React.SVGProps<SVGSVGElement>> = ({ children, ...props }) => (
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth={1.5} strokeLinecap="round" strokeLinejoin="round" {...props}>
        {children}
    </svg>
);

const ArchiveBoxLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><rect width="20" height="5" x="2" y="3" rx="1"/><path d="M4 8v11a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8"/><path d="M10 12h4"/></LucideIconWrapper> );
const ArrowPathLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8"/><path d="M21 3v5h-5"/><path d="M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16"/><path d="M3 21v-5h5"/></LucideIconWrapper> );
const ArrowsPointingInLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><polyline points="4 14 10 14 10 20"/><polyline points="20 10 14 10 14 4"/><line x1="14" y1="10" x2="21" y2="3"/><line x1="3" y1="21" x2="10" y2="14"/></LucideIconWrapper> );
const ArrowsPointingOutLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><polyline points="15 3 21 3 21 9"/><polyline points="9 21 3 21 3 15"/><line x1="21" y1="3" x2="14" y2="10"/><line x1="3" y1="21" x2="10" y2="14"/></LucideIconWrapper> );
const ArrowUpTrayLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="17 8 12 3 7 8"/><line x1="12" y1="3" x2="12" y2="15"/></LucideIconWrapper> );
const BookOpenLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z"/><path d="M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"/></LucideIconWrapper> );
const BugAntLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="m8 2 1.88 1.88"/><path d="M14.12 3.88 16 2"/><path d="M9 7.13v-1a3.003 3.003 0 1 1 6 0v1"/><path d="M12 20c-3.3 0-6-2.7-6-6v-3a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v3c0 3.3-2.7 6-6 6"/><path d="M12 20v-4"/><path d="M6 13H2"/><path d="M18 13h4"/><path d="M6 7h2.5"/><path d="M15.5 7H18"/></LucideIconWrapper> );
const ChartBarLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><line x1="18" y1="20" x2="18" y2="10"/><line x1="12" y1="20" x2="12" y2="4"/><line x1="6" y1="20" x2="6" y2="14"/></LucideIconWrapper> );
const CheckCircleLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/><polyline points="22 4 12 14.01 9 11.01"/></LucideIconWrapper> );
const ChevronDownLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="m6 9 6 6 6-6"/></LucideIconWrapper> );
const ChevronLeftLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="m15 18-6-6 6-6"/></LucideIconWrapper> );
const ChevronRightLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="m9 18 6-6-6-6"/></LucideIconWrapper> );
const ClockLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><circle cx="12" cy="12" r="10"/><polyline points="12 6 12 12 16 14"/></LucideIconWrapper> );
const CodeBracketLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><polyline points="16 18 22 12 16 6"/><polyline points="8 6 2 12 8 18"/></LucideIconWrapper> );
const CodeBracketSquareLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><rect width="18" height="18" x="3" y="3" rx="2"/><path d="M10 9.5 8 12l2 2.5"/><path d="m14 9.5 2 2.5-2 2.5"/></LucideIconWrapper> );
const CogLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 0 2l-.15.08a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.38a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1 0-2l.15-.08a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"/><circle cx="12" cy="12" r="3"/></LucideIconWrapper> );
const DatabaseLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><ellipse cx="12" cy="5" rx="9" ry="3"/><path d="M3 5v14a9 3 0 0 0 18 0V5"/><path d="M3 12a9 3 0 0 0 18 0"/></LucideIconWrapper> );
const DocumentPlusLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z"/><polyline points="14 2 14 8 20 8"/><line x1="12" y1="18" x2="12" y2="12"/><line x1="9" y1="15" x2="15" y2="15"/></LucideIconWrapper> );
const DownloadLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></LucideIconWrapper> );
const ExclamationTriangleLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="m21.73 18-8-14a2 2 0 0 0-3.46 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3Z"/><path d="M12 9v4"/><path d="M12 17h.01"/></LucideIconWrapper> );
const FileLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z"/><polyline points="14 2 14 8 20 8"/></LucideIconWrapper> );
const FilterLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><polygon points="22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3"/></LucideIconWrapper> );
const FolderLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M4 20h16a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.93a2 2 0 0 1-1.66-.9l-.82-1.2a2 2 0 0 0-1.66-.9H4a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2Z"/></LucideIconWrapper> );
const InformationCircleLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><circle cx="12" cy="12" r="10"/><path d="M12 16v-4"/><path d="M12 8h.01"/></LucideIconWrapper> );
const PencilSquareLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z"/></LucideIconWrapper> );
const PlusCircleLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><circle cx="12" cy="12" r="10"/><path d="M8 12h8"/><path d="M12 8v8"/></LucideIconWrapper> );
const SaveDiskLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z"/><polyline points="17 21 17 13 7 13 7 21"/><polyline points="7 3 7 8 15 8"/></LucideIconWrapper> );
const SidebarRightLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><rect width="18" height="18" x="3" y="3" rx="2"/><path d="M16 3v18"/></LucideIconWrapper> );
const TableLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M12 3v18"/><rect width="18" height="18" x="3" y="3" rx="2"/><path d="M3 9h18"/><path d="M3 15h18"/></LucideIconWrapper> );
const TerminalLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><polyline points="4 17 10 11 4 5"/><line x1="12" y1="19" x2="20" y2="19"/></LucideIconWrapper> );
const TrashLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M3 6h18"/><path d="M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"/><line x1="10" y1="11" x2="10" y2="17"/><line x1="14" y1="11" x2="14" y2="17"/></LucideIconWrapper> );
const ViewColumnsLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><rect width="18" height="18" x="3" y="3" rx="2"/><path d="M12 3v18"/></LucideIconWrapper> );
const XCircleLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><circle cx="12" cy="12" r="10"/><path d="m15 9-6 6"/><path d="m9 9 6 6"/></LucideIconWrapper> );
const XMarkLucide: React.FC<React.SVGProps<SVGSVGElement>> = (props) => ( <LucideIconWrapper {...props}><path d="M18 6 6 18"/><path d="M6 6l12 12"/></LucideIconWrapper> );

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
    SaveDisk: SaveDiskSharp,
    Save: SaveDiskSharp,
    SidebarRight: SidebarRightSharp,
    Table: TableSharp,
    Terminal: TerminalSharp,
    Trash: TrashSharp,
    ViewColumns: ViewColumnsSharp,
    XCircle: XCircleSharp,
    XMark: XMarkSharp,
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
    Clock: ClockSolid,
    CodeBracket: CodeBracketSolid,
    CodeBracketSquare: CodeBracketSquareSolid,
    Cog: CogSolid,
    Database: DatabaseSolid,
    DocumentPlus: DocumentPlusSolid,
    Download: DownloadSolid,
    ExclamationTriangle: ExclamationTriangleSolid,
    File: FileSolid,
    Filter: FilterSolid,
    Folder: FolderSolid,
    InformationCircle: InformationCircleSolid,
    PencilSquare: PencilSquareSolid,
    PlusCircle: PlusCircleSolid,
    SaveDisk: SaveDiskSolid,
    Save: SaveDiskSolid,
    SidebarRight: SidebarRightSolid,
    Table: TableSolid,
    Terminal: TerminalSolid,
    Trash: TrashSolid,
    ViewColumns: ViewColumnsSolid,
    XCircle: XCircleSolid,
    XMark: XMarkSolid,
};

const featherIcons = {
    ArchiveBox: ArchiveBoxFeather,
    ArrowPath: ArrowPathFeather,
    ArrowsPointingIn: ArrowsPointingInFeather,
    ArrowsPointingOut: ArrowsPointingOutFeather,
    ArrowUpTray: ArrowUpTrayFeather,
    BookOpen: BookOpenFeather,
    BugAnt: BugAntFeather,
    ChartBar: ChartBarFeather,
    CheckCircle: CheckCircleFeather,
    ChevronDown: ChevronDownFeather,
    ChevronLeft: ChevronLeftFeather,
    ChevronRight: ChevronRightFeather,
    Clock: ClockFeather,
    CodeBracket: CodeBracketFeather,
    CodeBracketSquare: CodeBracketSquareFeather,
    Cog: CogFeather,
    Database: DatabaseFeather,
    DocumentPlus: DocumentPlusFeather,
    Download: DownloadFeather,
    ExclamationTriangle: ExclamationTriangleFeather,
    File: FileFeather,
    Filter: FilterFeather,
    Folder: FolderFeather,
    InformationCircle: InformationCircleFeather,
    PencilSquare: PencilSquareFeather,
    PlusCircle: PlusCircleFeather,
    SaveDisk: SaveDiskFeather,
    Save: SaveDiskFeather,
    SidebarRight: SidebarRightFeather,
    Table: TableFeather,
    Terminal: TerminalFeather,
    Trash: TrashFeather,
    ViewColumns: ViewColumnsFeather,
    XCircle: XCircleFeather,
    XMark: XMarkFeather,
};

const tablerIcons = {
    ArchiveBox: ArchiveBoxTabler,
    ArrowPath: ArrowPathTabler,
    ArrowsPointingIn: ArrowsPointingInTabler,
    ArrowsPointingOut: ArrowsPointingOutTabler,
    ArrowUpTray: ArrowUpTrayTabler,
    BookOpen: BookOpenTabler,
    BugAnt: BugAntTabler,
    ChartBar: ChartBarTabler,
    CheckCircle: CheckCircleTabler,
    ChevronDown: ChevronDownTabler,
    ChevronLeft: ChevronLeftTabler,
    ChevronRight: ChevronRightTabler,
    Clock: ClockTabler,
    CodeBracket: CodeBracketTabler,
    CodeBracketSquare: CodeBracketSquareTabler,
    Cog: CogTabler,
    Database: DatabaseTabler,
    DocumentPlus: DocumentPlusTabler,
    Download: DownloadTabler,
    ExclamationTriangle: ExclamationTriangleTabler,
    File: FileTabler,
    Filter: FilterTabler,
    Folder: FolderTabler,
    InformationCircle: InformationCircleTabler,
    PencilSquare: PencilSquareTabler,
    PlusCircle: PlusCircleTabler,
    SaveDisk: SaveDiskTabler,
    Save: SaveDiskTabler,
    SidebarRight: SidebarRightTabler,
    Table: TableTabler,
    Terminal: TerminalTabler,
    Trash: TrashTabler,
    ViewColumns: ViewColumnsTabler,
    XCircle: XCircleTabler,
    XMark: XMarkTabler,
};

const lucideIcons = {
    ArchiveBox: ArchiveBoxLucide,
    ArrowPath: ArrowPathLucide,
    ArrowsPointingIn: ArrowsPointingInLucide,
    ArrowsPointingOut: ArrowsPointingOutLucide,
    ArrowUpTray: ArrowUpTrayLucide,
    BookOpen: BookOpenLucide,
    BugAnt: BugAntLucide,
    ChartBar: ChartBarLucide,
    CheckCircle: CheckCircleLucide,
    ChevronDown: ChevronDownLucide,
    ChevronLeft: ChevronLeftLucide,
    ChevronRight: ChevronRightLucide,
    Clock: ClockLucide,
    CodeBracket: CodeBracketLucide,
    CodeBracketSquare: CodeBracketSquareLucide,
    Cog: CogLucide,
    Database: DatabaseLucide,
    DocumentPlus: DocumentPlusLucide,
    Download: DownloadLucide,
    ExclamationTriangle: ExclamationTriangleLucide,
    File: FileLucide,
    Filter: FilterLucide,
    Folder: FolderLucide,
    InformationCircle: InformationCircleLucide,
    PencilSquare: PencilSquareLucide,
    PlusCircle: PlusCircleLucide,
    SaveDisk: SaveDiskLucide,
    Save: SaveDiskLucide,
    SidebarRight: SidebarRightLucide,
    Table: TableLucide,
    Terminal: TerminalLucide,
    Trash: TrashLucide,
    ViewColumns: ViewColumnsLucide,
    XCircle: XCircleLucide,
    XMark: XMarkLucide,
};


const iconSets = {
  sharp: sharpIcons,
  solid: solidIcons,
  feather: featherIcons,
  tabler: tablerIcons,
  lucide: lucideIcons,
};

type IconName = keyof typeof sharpIcons;

interface IconProps extends React.SVGProps<SVGSVGElement> {
  name: IconName;
  iconSet: IconSet;
}

export const Icon: React.FC<IconProps> = ({ name, iconSet, ...props }) => {
    const IconComponent = iconSets[iconSet]?.[name] || sharpIcons[name];
    if (!IconComponent) {
        console.warn(`Icon "${name}" not found in set "${iconSet}".`);
        return null;
    }
    return <IconComponent {...props} />;
};