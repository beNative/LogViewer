import React from 'react';
import { IconSet } from '../../types.ts';

// Each icon has a 'sharp' (outline) and 'solid' variant.
// The Icon component below will select the correct one based on the iconSet prop.

const iconDefs = {
    ArchiveBox: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 7.5l-.625 10.632a2.25 2.25 0 01-2.247 2.118H6.622a2.25 2.25 0 01-2.247-2.118L3.75 7.5M10 11.25h4M3.375 7.5h17.25c.621 0 1.125-.504 1.125-1.125v-1.5c0-.621-.504-1.125-1.125-1.125H3.375c-.621 0-1.125.504-1.125 1.125v1.5c0 .621.504 1.125 1.125 1.125z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M2.25 5.625A2.25 2.25 0 014.5 3.375h15a2.25 2.25 0 012.25 2.25v1.5a2.25 2.25 0 01-2.25 2.25h-15A2.25 2.25 0 012.25 7.125v-1.5zM11.25 9.75a.75.75 0 000 1.5h1.5a.75.75 0 000-1.5h-1.5z" clipRule="evenodd" />
                <path d="M3.375 12.75a2.25 2.25 0 000 4.5h17.25a2.25 2.25 0 000-4.5H3.375z" />
                <path fillRule="evenodd" d="M4.093 19.553a.75.75 0 00-.525.934 2.25 2.25 0 002.079 1.763h11.706a2.25 2.25 0 002.079-1.763.75.75 0 00-.525-.934 10.45 10.45 0 00-15.362 0z" clipRule="evenodd" />
            </svg>
        )
    },
    ArrowLeftRight: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M7.5 21L3 16.5m0 0L7.5 12M3 16.5h18m-7.5-12L21 7.5m0 0L16.5 12M21 7.5H3" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M16.28 3.22a.75.75 0 010 1.06l-6.25 6.25a.75.75 0 000 1.06l6.25 6.25a.75.75 0 11-1.06 1.06l-6.25-6.25a2.25 2.25 0 010-3.18L15.22 3.22a.75.75 0 011.06 0z" clipRule="evenodd" />
                <path fillRule="evenodd" d="M7.72 3.22a.75.75 0 011.06 0l6.25 6.25a2.25 2.25 0 010 3.18l-6.25 6.25a.75.75 0 11-1.06-1.06l6.25-6.25a.75.75 0 000-1.06L7.72 4.28a.75.75 0 010-1.06z" clipRule="evenodd" />
            </svg>
        )
    },
    ArrowPath: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0l3.181 3.183a8.25 8.25 0 0011.667 0l3.181-3.183m-4.991-2.692v-4.992m0 0h-4.992m4.992 0l-3.181-3.183a8.25 8.25 0 00-11.667 0L2.985 9.348z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M4.755 10.059a7.5 7.5 0 0112.548-3.364l1.903 1.903h-4.5a.75.75 0 000 1.5h6a.75.75 0 00.75-.75v-6a.75.75 0 00-1.5 0v4.514l-1.657-1.657A9 9 0 103.302 16.113a.75.75 0 001.455-.389A7.5 7.5 0 014.755 10.059z" clipRule="evenodd" />
            </svg>
        )
    },
    ArrowsPointingIn: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M9 9V4.5M9 9H4.5M9 9L3.75 3.75M9 15v5.5M9 15H4.5M9 15l-5.25 5.25M15 9h5.5M15 9V4.5M15 9l5.25-5.25M15 15h5.5M15 15v5.5M15 15l5.25 5.25" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M9 4.5a.75.75 0 01.75.75v3.75h3.75a.75.75 0 010 1.5H9.75v3.75a.75.75 0 01-1.5 0V9.75H4.5a.75.75 0 010-1.5h3.75V4.5A.75.75 0 019 4.5zm6.75 0a.75.75 0 01.75.75v3.75h3.75a.75.75 0 010 1.5h-3.75v3.75a.75.75 0 01-1.5 0V9.75h-3.75a.75.75 0 010-1.5h3.75V4.5a.75.75 0 01.75-.75zM9 19.5a.75.75 0 01.75-.75v-3.75h3.75a.75.75 0 010-1.5H9.75v-3.75a.75.75 0 01-1.5 0v3.75H4.5a.75.75 0 010 1.5h3.75v3.75A.75.75 0 019 19.5zm6.75 0a.75.75 0 01.75-.75v-3.75h3.75a.75.75 0 010-1.5h-3.75v-3.75a.75.75 0 01-1.5 0v3.75h-3.75a.75.75 0 010 1.5h3.75v3.75a.75.75 0 01.75.75z" clipRule="evenodd" />
            </svg>
        )
    },
    ArrowsPointingOut: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3.75 3.75v4.5m0-4.5h4.5m-4.5 0L9 9M3.75 20.25v-4.5m0 4.5h4.5m-4.5 0L9 15M20.25 3.75h-4.5m4.5 0v4.5m0-4.5L15 9M20.25 20.25h-4.5m4.5 0v-4.5m0 4.5L15 15" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M3 3.75A.75.75 0 013.75 3h6a.75.75 0 010 1.5h-4.5v4.5a.75.75 0 01-1.5 0v-6zm18 0a.75.75 0 01.75.75v6a.75.75 0 01-1.5 0v-4.5h-4.5a.75.75 0 010-1.5h6zM3 20.25a.75.75 0 01.75-.75h4.5v-4.5a.75.75 0 011.5 0v6a.75.75 0 01-.75.75h-6a.75.75 0 01-.75-.75zm18 0a.75.75 0 01.75-.75v-6a.75.75 0 01-1.5 0v4.5h-4.5a.75.75 0 010-1.5h6a.75.75 0 01.75.75z" clipRule="evenodd" />
            </svg>
        )
    },
    ArrowUpTray: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5m-13.5-9L12 3m0 0l4.5 4.5M12 3v13.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M11.47 2.47a.75.75 0 011.06 0l4.5 4.5a.75.75 0 01-1.06 1.06l-3.22-3.22V16.5a.75.75 0 01-1.5 0V4.81L8.03 8.03a.75.75 0 01-1.06-1.06l4.5-4.5zM3 16.5a.75.75 0 01.75.75v2.25a2.25 2.25 0 002.25 2.25h13.5a2.25 2.25 0 002.25-2.25V17.25a.75.75 0 011.5 0v2.25a3.75 3.75 0 01-3.75 3.75H6a3.75 3.75 0 01-3.75-3.75V17.25A.75.75 0 013 16.5z" clipRule="evenodd" />
            </svg>
        )
    },
    BookOpen: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                 <path fillRule="evenodd" d="M2.25 2.25a.75.75 0 000 1.5H3v16.5a1.5 1.5 0 001.5 1.5h15a1.5 1.5 0 001.5-1.5V3.75h.75a.75.75 0 000-1.5h-18zM15.25 18a.75.75 0 000-1.5H8.75a.75.75 0 000 1.5h6.5zM15.25 13.5a.75.75 0 000-1.5H8.75a.75.75 0 000 1.5h6.5zM15.25 9a.75.75 0 000-1.5H8.75a.75.75 0 000 1.5h6.5z" clipRule="evenodd" />
            </svg>
        )
    },
    BugAnt: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 21a9.004 9.004 0 008.716-6.747M12 21a9.004 9.004 0 01-8.716-6.747M12 21c1.355 0 2.707-.156 4.008-.45m-8.016 0A9.004 9.004 0 0112 3c1.355 0 2.707.156 4.008.45m0 10.954c.366.191.71.405.998.648m-8.016-11.4a9.004 9.004 0 00-4.008.45m4.008-.45L9 3m3 18l3-2.672M9 3l-3 2.672m0 0a9.004 9.004 0 00-2.716 6.747M15 3l3 2.672m0 0a9.004 9.004 0 012.716 6.747m0-6.747c.366.191.71.405.998.648M12 6.75h.008v.008H12V6.75z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM8.25 12a.75.75 0 01.75-.75h6a.75.75 0 010 1.5h-6a.75.75 0 01-.75-.75z" clipRule="evenodd" />
            </svg>
        )
    },
    ChartBar: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3 13.125C3 12.504 3.504 12 4.125 12h2.25c.621 0 1.125.504 1.125 1.125v6.75C7.5 20.496 6.996 21 6.375 21h-2.25A1.125 1.125 0 013 19.875v-6.75zM9.75 8.625c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125v11.25c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V8.625zM16.5 4.125c0-.621.504-1.125 1.125-1.125h2.25C20.496 3 21 3.504 21 4.125v15.75c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V4.125z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M3 13.125C3 12.504 3.504 12 4.125 12h2.25c.621 0 1.125.504 1.125 1.125v6.75C7.5 20.496 6.996 21 6.375 21h-2.25A1.125 1.125 0 013 19.875v-6.75zM9.75 8.625c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125v11.25c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V8.625zM16.5 4.125c0-.621.504-1.125 1.125-1.125h2.25C20.496 3 21 3.504 21 4.125v15.75c0 .621-.504 1.125-1.125 1.125h-2.25a1.125 1.125 0 01-1.125-1.125V4.125z" />
            </svg>
        )
    },
    CheckCircle: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M9 12.75L11.25 15 15 9.75M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm13.36-1.814a.75.75 0 10-1.22-.872l-3.236 4.53L9.53 12.22a.75.75 0 00-1.06 1.06l2.25 2.25a.75.75 0 001.14-.094l3.75-5.25z" clipRule="evenodd" />
            </svg>
        )
    },
    ChevronDown: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 8.25l-7.5 7.5-7.5-7.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12.53 16.28a.75.75 0 01-1.06 0l-7.5-7.5a.75.75 0 011.06-1.06L12 14.69l6.97-6.97a.75.75 0 111.06 1.06l-7.5 7.5z" clipRule="evenodd" />
            </svg>
        )
    },
    ChevronLeft: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 19.5L8.25 12l7.5-7.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M7.72 12.53a.75.75 0 010-1.06l7.5-7.5a.75.75 0 111.06 1.06L9.31 12l6.97 6.97a.75.75 0 11-1.06 1.06l-7.5-7.5z" clipRule="evenodd" />
            </svg>
        )
    },
    ChevronRight: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M8.25 4.5l7.5 7.5-7.5 7.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M16.28 11.47a.75.75 0 010 1.06l-7.5 7.5a.75.75 0 01-1.06-1.06L14.69 12 7.72 5.03a.75.75 0 011.06-1.06l7.5 7.5z" clipRule="evenodd" />
            </svg>
        )
    },
    ClipboardDocument: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M15.75 17.25v3.375c0 .621-.504 1.125-1.125 1.125h-9.75a1.125 1.125 0 01-1.125-1.125V7.875c0-.621.504-1.125 1.125-1.125H6.75a9.06 9.06 0 011.5.124m7.5 10.376h3.375c.621 0 1.125-.504 1.125-1.125V11.25c0-4.46-3.243-8.161-7.5-8.876a9.06 9.06 0 00-1.5-.124H9.375c-.621 0-1.125.504-1.125 1.125v3.5m7.5 10.375H9.375a1.125 1.125 0 01-1.125-1.125v-9.25m12 6.625v-1.875a3.375 3.375 0 00-3.375-3.375h-1.5a1.125 1.125 0 01-1.125-1.125v-1.5a3.375 3.375 0 00-3.375-3.375H9.75" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M10.5 3A2.25 2.25 0 008.25 5.25v.041a2.25 2.25 0 01-1.841 2.208 9.06 9.06 0 001.15.124h.01a9.06 9.06 0 001.5-.124A2.25 2.25 0 019.05 5.291V5.25A2.25 2.25 0 006.75 3h3.75zm-3.75 9.75a.75.75 0 01.75-.75h.01a.75.75 0 01.75.75v.01a.75.75 0 01-.75.75h-.01a.75.75 0 01-.75-.75v-.01zM10.5 12a.75.75 0 00-1.5 0v.01a.75.75 0 001.5 0v-.01zM10.5 15a.75.75 0 01.75-.75h.01a.75.75 0 01.75.75v.01a.75.75 0 01-.75.75h-.01a.75.75 0 01-.75-.75v-.01zM13.5 15a.75.75 0 00-1.5 0v.01a.75.75 0 001.5 0v-.01z" clipRule="evenodd" />
                <path d="M3 9.375A2.625 2.625 0 015.625 6.75h12.75A2.625 2.625 0 0121 9.375v9A2.625 2.625 0 0118.375 21H5.625A2.625 2.625 0 013 18.375v-9zM7.5 12a.75.75 0 00-1.5 0v.01a.75.75 0 001.5 0v-.01zM13.5 12a.75.75 0 01.75-.75h.01a.75.75 0 01.75.75v.01a.75.75 0 01-.75.75h-.01a.75.75 0 01-.75-.75v-.01zM16.5 12a.75.75 0 00-1.5 0v.01a.75.75 0 001.5 0v-.01zM7.5 15a.75.75 0 01.75-.75h.01a.75.75 0 01.75.75v.01a.75.75 0 01-.75.75h-.01a.75.75 0 01-.75-.75v-.01z" />
            </svg>
        )
    },
    Clock: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 6v6h4.5m4.5 0a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM12.75 6a.75.75 0 00-1.5 0v6c0 .414.336.75.75.75h4.5a.75.75 0 000-1.5h-3.75V6z" clipRule="evenodd" />
            </svg>
        )
    },
    CodeBracket: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M17.25 6.75L22.5 12l-5.25 5.25m-10.5 0L1.5 12l5.25-5.25" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                 <path fillRule="evenodd" d="M10.276 3.862a.75.75 0 01.293 1.028l-4.148 7.375a.75.75 0 001.314.738l4.148-7.375a.75.75 0 011.028-.293l8.026 4.515a.75.75 0 010 1.328l-8.026 4.515a.75.75 0 01-1.028-.293l-4.148-7.375a.75.75 0 00-1.314.738l4.148 7.375a.75.75 0 01-.293 1.028l-8.026 4.515a.75.75 0 01-1.028-.664V4.526a.75.75 0 011.028-.664l8.026 4.515z" clipRule="evenodd" />
            </svg>
        )
    },
    CodeBracketSquare: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M14.25 9.75L16.5 12l-2.25 2.25m-4.5 0L7.5 12l2.25-2.25M6 20.25h12A2.25 2.25 0 0020.25 18V5.75A2.25 2.25 0 0018 3.5H6A2.25 2.25 0 003.75 5.75v12.5A2.25 2.25 0 006 20.25z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M3 3.75A.75.75 0 013.75 3h16.5a.75.75 0 01.75.75v16.5a.75.75 0 01-.75.75H3.75a.75.75 0 01-.75-.75V3.75zm6.22 8.28a.75.75 0 01.03-1.06l-3-3a.75.75 0 011.06-1.06l3.5 3.5a.75.75 0 010 1.06l-3.5 3.5a.75.75 0 11-1.06-1.06l3-3a.75.75 0 01.03-.03z" clipRule="evenodd" />
            </svg>
        )
    },
    Cog: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M4.5 12a7.5 7.5 0 0015 0m-15 0a7.5 7.5 0 1115 0m-15 0H3m18 0h-1.5m-15 0a7.5 7.5 0 1115 0m-15 0h-1.5m15 0h1.5m-16.5 5.25l-1.5 1.5M5.25 20.25l1.5-1.5M18.75 3.75l1.5 1.5M18.75 3.75l-1.5 1.5M3.75 18.75l1.5-1.5M3.75 18.75l1.5 1.5m13.5-13.5l1.5-1.5M20.25 5.25l-1.5 1.5M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M11.078 2.25c-.917 0-1.699.663-1.85 1.567L9.05 5.85a1.5 1.5 0 001.277 1.833l.41.104a3.75 3.75 0 014.51 4.51l-.104.41a1.5 1.5 0 00-1.833 1.277l-2.033.178a1.875 1.875 0 01-1.567 1.85l-2.033.178a1.5 1.5 0 00-1.833 1.277l-.104.41a3.75 3.75 0 01-4.51 4.51l-.41-.104a1.5 1.5 0 00-1.277-1.833l-.178-2.033a1.875 1.875 0 01-1.85-1.567l-.178-2.033a1.5 1.5 0 00-1.277-1.833l-.41-.104a3.75 3.75 0 01-4.51-4.51l.104-.41a1.5 1.5 0 001.833-1.277l2.033-.178A1.875 1.875 0 019.05 5.85l2.033-.178a1.5 1.5 0 001.833-1.277l.104-.41a3.75 3.75 0 014.51-4.51l.41.104a1.5 1.5 0 001.277 1.833l.178 2.033a1.875 1.875 0 011.85 1.567l.178 2.033a1.5 1.5 0 001.277 1.833l.41.104a3.75 3.75 0 014.51 4.51l-.104.41a1.5 1.5 0 00-1.833 1.277l-2.033.178a1.875 1.875 0 01-1.567 1.85l-2.033.178a1.5 1.5 0 00-1.833 1.277l-.104.41a3.75 3.75 0 01-4.51 4.51l-.41-.104a1.5 1.5 0 00-1.277-1.833L9.05 18.15a1.875 1.875 0 01-1.567-1.85l-.178-2.033a1.5 1.5 0 00-1.833-1.277l-.104-.41a3.75 3.75 0 01-4.51-4.51l.104-.41a1.5 1.5 0 001.833-1.277l2.033-.178A1.875 1.875 0 0111.078 2.25z" clipRule="evenodd" />
            </svg>
        )
    },
    Cube: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M21 7.5l-9-5.25L3 7.5m18 0l-9 5.25m9-5.25v9l-9 5.25M3 7.5l9 5.25M3 7.5v9l9 5.25m0-9.75l-9-5.25m9 5.25l9-5.25" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12.516 2.17a.75.75 0 00-1.032 0L2.942 7.785a.75.75 0 00-.429.646v6.138a.75.75 0 00.429.646l8.542 5.615a.75.75 0 001.032 0l8.542-5.615a.75.75 0 00.429-.646V8.43a.75.75 0 00-.429-.646L12.516 2.17z" clipRule="evenodd" />
            </svg>
        )
    },
    CursorArrowRays: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
             <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.223 5.223h5.282a.75.75 0 01.597 1.285l-5.224 5.224a.75.75 0 01-1.285-.597v-5.282zM15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.223 5.223h5.282a.75.75 0 01.597 1.285l-5.224 5.224a.75.75 0 01-1.285-.597v-5.282z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
             <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M12.352 4.416a.75.75 0 00-1.06-.02L7.02 8.25H4.5a.75.75 0 000 1.5h2.52l4.272 3.848a.75.75 0 001.082-.992L10.5 9.31l1.59-1.59v3.78a.75.75 0 001.5 0v-3.78l1.59 1.59-1.892 4.257a.75.75 0 00.992 1.082l4.272-3.848h2.52a.75.75 0 000-1.5h-2.52l-4.272-3.848a.75.75 0 00-1.06-.02z" />
                <path d="M5.25 12a.75.75 0 00-.75.75v6a.75.75 0 001.5 0v-6a.75.75 0 00-.75-.75zM18.75 12a.75.75 0 00-.75.75v6a.75.75 0 001.5 0v-6a.75.75 0 00-.75-.75zM12 15.75a.75.75 0 00-.75.75v3a.75.75 0 001.5 0v-3a.75.75 0 00-.75-.75z" />
            </svg>
        )
    },
    Database: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3.75 6A2.25 2.25 0 016 3.75h12A2.25 2.25 0 0120.25 6v1.5a2.25 2.25 0 01-2.25 2.25H6A2.25 2.25 0 013.75 7.5V6zM3.75 15A2.25 2.25 0 016 12.75h12a2.25 2.25 0 012.25 2.25v1.5A2.25 2.25 0 0118 18.75H6A2.25 2.25 0 013.75 16.5V15z" />
                <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 9v9A2.25 2.25 0 0118 20.25H6a2.25 2.25 0 01-2.25-2.25V9m16.5 0H3.75" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M4.5 3.75A2.25 2.25 0 016.75 1.5h10.5A2.25 2.25 0 0119.5 3.75v16.5A2.25 2.25 0 0117.25 22.5H6.75A2.25 2.25 0 014.5 20.25V3.75z" />
                <path fillRule="evenodd" d="M8.25 6a.75.75 0 01.75.75v1.5a.75.75 0 01-1.5 0v-1.5A.75.75 0 018.25 6zM9 12a.75.75 0 000 1.5h6a.75.75 0 000-1.5H9z" clipRule="evenodd" />
            </svg>
        )
    },
    DocumentPlus: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 14.25v-2.625a3.375 3.375 0 00-3.375-3.375h-1.5A1.125 1.125 0 0113.5 7.125v-1.5a3.375 3.375 0 00-3.375-3.375H8.25m3.75 9h6m-6 0h-6m6 0v6m0-6V9.75M12 3H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V9.75M12 3L8.25 6.75" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M5.625 1.5A3.375 3.375 0 002.25 4.875v14.25A3.375 3.375 0 005.625 22.5h12.75A3.375 3.375 0 0021.75 19.125V8.625a3.375 3.375 0 00-1.28-2.613L16.12 2.73A3.375 3.375 0 0013.488 1.5H5.625zM12 8.25a.75.75 0 01.75.75v3.75h3.75a.75.75 0 010 1.5h-3.75v3.75a.75.75 0 01-1.5 0v-3.75H7.5a.75.75 0 010-1.5h3.75V9a.75.75 0 01.75-.75z" clipRule="evenodd" />
            </svg>
        )
    },
    Download: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5M16.5 12L12 16.5m0 0L7.5 12m4.5 4.5V3" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12 2.25a.75.75 0 01.75.75v11.69l3.22-3.22a.75.75 0 111.06 1.06l-4.5 4.5a.75.75 0 01-1.06 0l-4.5-4.5a.75.75 0 111.06-1.06l3.22 3.22V3a.75.75 0 01.75-.75zM3 16.5a.75.75 0 01.75.75v2.25a2.25 2.25 0 002.25 2.25h13.5a2.25 2.25 0 002.25-2.25V17.25a.75.75 0 011.5 0v2.25a3.75 3.75 0 01-3.75 3.75H6a3.75 3.75 0 01-3.75-3.75V17.25A.75.75 0 013 16.5z" clipRule="evenodd" />
            </svg>
        )
    },
    Eye: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M2.036 12.322a1.012 1.012 0 010-.639C3.423 7.51 7.36 4.5 12 4.5c4.638 0 8.573 3.007 9.963 7.178.07.207.07.431 0 .639C20.577 16.49 16.64 19.5 12 19.5c-4.638 0-8.573-3.007-9.963-7.178z" />
                <path strokeLinecap="round" strokeLinejoin="round" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M12 15a3 3 0 100-6 3 3 0 000 6z" />
                <path fillRule="evenodd" d="M1.323 11.447C2.811 6.976 7.028 3.75 12.001 3.75c4.97 0 9.185 3.223 10.675 7.69.12.362.12.752 0 1.113-1.487 4.471-5.705 7.697-10.677 7.697-4.97 0-9.186-3.223-10.675-7.69a1.762 1.762 0 010-1.113zM17.25 12a5.25 5.25 0 11-10.5 0 5.25 5.25 0 0110.5 0z" clipRule="evenodd" />
            </svg>
        )
    },
     EyeSlash: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3.98 8.223A10.477 10.477 0 001.934 12C3.226 16.338 7.244 19.5 12 19.5c.993 0 1.953-.138 2.863-.395M6.228 6.228A10.45 10.45 0 0112 4.5c4.756 0 8.773 3.162 10.065 7.498a10.523 10.523 0 01-4.293 5.774M6.228 6.228L3 3m3.228 3.228l3.65 3.65m7.894 7.894L21 21m-3.228-3.228l-3.65-3.65m0 0a3 3 0 10-4.243-4.243m4.242 4.242L9.88 9.88" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M3.53 2.47a.75.75 0 00-1.06 1.06l18 18a.75.75 0 101.06-1.06l-18-18zM22.676 12.553a11.249 11.249 0 01-2.631 4.31l-3.099-3.099a5.25 5.25 0 00-6.71-6.71L7.759 4.577a11.217 11.217 0 014.242-.827c4.97 0 9.185 3.223 10.675 7.69.12.362.12.752 0 1.113z" />
                <path fillRule="evenodd" d="M1.323 11.447C2.811 6.976 7.028 3.75 12.001 3.75c.725 0 1.45.08 2.148.235l-2.44 2.44A5.25 5.25 0 006.75 12c0 1.562.54 3.024 1.464 4.193l-2.44 2.44A11.217 11.217 0 011.323 12.553a1.762 1.762 0 010-1.113z" clipRule="evenodd" />
            </svg>
        )
    },
    ExclamationTriangle: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 9v3.75m-9.303 3.376c-.866 1.5.217 3.374 1.948 3.374h14.71c1.73 0 2.813-1.874 1.948-3.374L13.949 3.378c-.866-1.5-3.032-1.5-3.898 0L2.697 16.126zM12 15.75h.007v.008H12v-.008z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M9.401 3.003c1.155-2 4.043-2 5.197 0l7.355 12.74a3.25 3.25 0 01-2.598 4.858H4.645a3.25 3.25 0 01-2.598-4.858L9.4 3.003zM12 8.25a.75.75 0 01.75.75v3.75a.75.75 0 01-1.5 0V9a.75.75 0 01.75-.75zm0 8.25a.75.75 0 100-1.5.75.75 0 000 1.5z" clipRule="evenodd" />
            </svg>
        )
    },
    File: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M19.5 14.25v-2.625a3.375 3.375 0 00-3.375-3.375h-1.5A1.125 1.125 0 0113.5 7.125v-1.5a3.375 3.375 0 00-3.375-3.375H8.25m.75 12l3 3m0 0l3-3m-3 3v-6m-1.5-9H5.625c-.621 0-1.125.504-1.125 1.125v17.25c0 .621.504 1.125 1.125 1.125h12.75c.621 0 1.125-.504 1.125-1.125V11.25a9 9 0 00-9-9z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M5.625 1.5A3.375 3.375 0 002.25 4.875v14.25A3.375 3.375 0 005.625 22.5h12.75A3.375 3.375 0 0021.75 19.125V8.625a3.375 3.375 0 00-1.28-2.613L16.12 2.73A3.375 3.375 0 0013.488 1.5H5.625z" />
            </svg>
        )
    },
    Filter: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 3c2.755 0 5.455.232 8.083.678.533.09.917.556.917 1.096v1.044a2.25 2.25 0 01-.659 1.591l-5.432 5.432a2.25 2.25 0 00-.659 1.591v2.927a2.25 2.25 0 01-1.244 2.013L9.75 21v-6.572a2.25 2.25 0 00-.659-1.591L3.659 7.409A2.25 2.25 0 013 5.818V4.774c0-.54.384-1.006.917-1.096A48.32 48.32 0 0112 3z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M3.792 2.938A49.069 49.069 0 0112 2.25c2.797 0 5.54.236 8.209.688a1.857 1.857 0 011.541 1.836v1.044a3 3 0 01-.879 2.121l-6.182 6.182a1.5 1.5 0 00-.439 1.061v2.927a1.5 1.5 0 01-.808 1.342l-3 1.5a1.5 1.5 0 01-1.992-1.342V15.5a1.5 1.5 0 00-.439-1.061l-6.182-6.182A3 3 0 012.25 6.502V5.459c0-.936.682-1.722 1.541-1.836z" clipRule="evenodd" />
            </svg>
        )
    },
    Folder: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M2.25 12.75V12A2.25 2.25 0 014.5 9.75h15A2.25 2.25 0 0121.75 12v.75m-8.69-6.44l-2.12-2.12a1.5 1.5 0 00-1.061-.44H4.5A2.25 2.25 0 002.25 6v12a2.25 2.25 0 002.25 2.25h15A2.25 2.25 0 0021.75 18V9a2.25 2.25 0 00-2.25-2.25h-5.379a1.5 1.5 0 01-1.06-.44z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M19.5 21a3 3 0 003-3v-4.5a3 3 0 00-3-3h-15a3 3 0 00-3 3V18a3 3 0 003 3h15z" />
                <path fillRule="evenodd" d="M1.5 6.75A2.25 2.25 0 013.75 4.5h3.443c1.013 0 1.95.5 2.549 1.332l.24.319a.75.75 0 00.51.249h4.958A2.25 2.25 0 0118 8.649v1.101A3.001 3.001 0 0015 9h-1.5a.75.75 0 01-.75-.75V6A.75.75 0 0012 5.25H7.5A.75.75 0 006.75 6v1.5a.75.75 0 01-.75.75H3.75A2.25 2.25 0 011.5 6.75z" clipRule="evenodd" />
            </svg>
        )
    },
    InformationCircle: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M11.25 11.25l.041-.02a.75.75 0 011.063.852l-.708 2.836a.75.75 0 001.063.853l.041-.021M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm8.706-1.442c1.146-.573 2.437.463 2.126 1.706l-.709 2.836.042-.02a.75.75 0 01.67 1.34l-.041.021l-1.293.922a.75.75 0 01-1.055-.832l.293-.728l-1.215.867a.75.75 0 01-1.03-.832l.301-.752l-1.293.922a.75.75 0 01-1.055-.832l.293-.728l-1.215.867a.75.75 0 01-1.03-.832l.301-.752 1.293-.922a.75.75 0 011.055.832l-.293.728l1.215-.867a.75.75 0 011.03.832l-.301.752 1.293-.922z" clipRule="evenodd" />
            </svg>
        )
    },
    Maximize: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <rect x="4.5" y="4.5" width="15" height="15" rx="1" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                 <path d="M3 3.75A.75.75 0 013.75 3h16.5a.75.75 0 01.75.75v16.5a.75.75 0 01-.75.75H3.75a.75.75 0 01-.75-.75V3.75z" />
            </svg>
        )
    },
    Minimize: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={2} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M20 12H4" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
             <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M4 12a1 1 0 011-1h14a1 1 0 110 2H5a1 1 0 01-1-1z" clipRule="evenodd" />
            </svg>
        )
    },
    Moon: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M21.752 15.002A9.718 9.718 0 0118 15.75c-5.385 0-9.75-4.365-9.75-9.75 0-1.33.266-2.597.748-3.752A9.753 9.753 0 003 11.25C3 16.635 7.365 21 12.75 21a9.753 9.753 0 009.002-5.998z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M9.528 1.718a.75.75 0 01.162.819A8.97 8.97 0 009 6a9 9 0 009 9 8.97 8.97 0 003.463-.69.75.75 0 01.981.98 10.503 10.503 0 01-9.694 6.46c-5.799 0-10.5-4.7-10.5-10.5 0-4.368 2.667-8.112 6.46-9.694a.75.75 0 01.818.162z" clipRule="evenodd" />
            </svg>
        )
    },
    PencilSquare: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M16.862 4.487l1.687-1.688a1.875 1.875 0 112.652 2.652L10.582 16.07a4.5 4.5 0 01-1.897 1.13L6 18l.8-2.685a4.5 4.5 0 011.13-1.897l8.932-8.931zm0 0L19.5 7.125M18 14v4.75A2.25 2.25 0 0115.75 21H5.25A2.25 2.25 0 013 18.75V8.25A2.25 2.25 0 015.25 6H10" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M21.731 2.269a2.625 2.625 0 00-3.712 0l-1.157 1.157 3.712 3.712 1.157-1.157a2.625 2.625 0 000-3.712zM17.581 6.182L14.47 3.072 6.182 11.36a3 3 0 00-.878 2.121v3.262a.75.75 0 00.75.75h3.262a3 3 0 002.121-.878l8.289-8.29z" />
                <path fillRule="evenodd" d="M2 4.75A2.75 2.75 0 014.75 2h5.5a.75.75 0 010 1.5h-5.5A1.25 1.25 0 003.5 4.75v14.5A1.25 1.25 0 004.75 20.5h14.5a1.25 1.25 0 001.25-1.25v-5.5a.75.75 0 011.5 0v5.5A2.75 2.75 0 0119.25 22H4.75A2.75 2.75 0 012 19.25V4.75z" clipRule="evenodd" />
            </svg>
        )
    },
    Play: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M5.25 5.653c0-.856.917-1.398 1.667-.986l11.54 6.348a1.125 1.125 0 010 1.971l-11.54 6.347a1.125 1.125 0 01-1.667-.985V5.653z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M4.5 5.653c0-1.426 1.529-2.33 2.779-1.643l11.54 6.348c1.295.712 1.295 2.573 0 3.285L7.279 19.99c-1.25.687-2.779-.217-2.779-1.643V5.653z" clipRule="evenodd" />
            </svg>
        )
    },
    PlusCircle: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zM12.75 9a.75.75 0 00-1.5 0v2.25H9a.75.75 0 000 1.5h2.25V15a.75.75 0 001.5 0v-2.25H15a.75.75 0 000-1.5h-2.25V9z" clipRule="evenodd" />
            </svg>
        )
    },
    Restore: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M16.5 8.25V6a2.25 2.25 0 00-2.25-2.25H6A2.25 2.25 0 003.75 6v8.25A2.25 2.25 0 006 16.5h2.25m8.25-8.25H18a2.25 2.25 0 012.25 2.25v8.25A2.25 2.25 0 0118 21H9.75A2.25 2.25 0 017.5 18.75V16.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M3 6.75A.75.75 0 013.75 6h6a.75.75 0 01.75.75v10.5a.75.75 0 01-.75.75h-6a.75.75 0 01-.75-.75V6.75zM14.25 6a.75.75 0 01.75-.75h3a.75.75 0 01.75.75v3a.75.75 0 01-1.5 0V7.5h-1.5a.75.75 0 01-.75-.75z" clipRule="evenodd" />
                <path d="M15 11.25a.75.75 0 01.75-.75h1.5a.75.75 0 01.75.75v7.5a.75.75 0 01-.75.75h-7.5a.75.75 0 01-.75-.75v-1.5a.75.75 0 011.5 0v.75h5.25v-6z" />
            </svg>
        )
    },
    SaveDisk: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M9 3.75H6.75A2.25 2.25 0 004.5 6v12a2.25 2.25 0 002.25 2.25h10.5A2.25 2.25 0 0019.5 18V9.75l-4.22-4.22A1.875 1.875 0 0014.25 4.5v-1.5m-3 0V9a.75.75 0 00.75.75h2.25M10.5 18.75a.75.75 0 001.5 0v-6a.75.75 0 00-1.5 0v6z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M6.75 2.25A2.25 2.25 0 004.5 4.5v15A2.25 2.25 0 006.75 21.75h10.5A2.25 2.25 0 0019.5 19.5V8.25l-4.5-4.5H6.75zm6-1.5a.75.75 0 00-.75.75V8.25a.75.75 0 00.75.75h3.75a.75.75 0 00.75-.75V3.313a.75.75 0 00-1.28-.53L12.75.75z" clipRule="evenodd" />
                <path d="M10.5 15a.75.75 0 000 1.5h3a.75.75 0 000-1.5h-3z" />
            </svg>
        )
    },
    SidebarRight: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M20.25 3.75H3.75A1.5 1.5 0 002.25 5.25v13.5A1.5 1.5 0 003.75 20.25h16.5a1.5 1.5 0 001.5-1.5V5.25a1.5 1.5 0 00-1.5-1.5zM15.75 3.75v16.5" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M2.25 3.75A1.5 1.5 0 013.75 2.25h16.5a1.5 1.5 0 011.5 1.5v16.5a1.5 1.5 0 01-1.5 1.5H3.75a1.5 1.5 0 01-1.5-1.5V3.75z" />
            </svg>
        )
    },
    Snowflake: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 21V3m0 0l-3.75 3.75M12 3l3.75 3.75M12 21l-3.75-3.75M12 21l3.75-3.75M3 12h18M3.75 6.75l3 3m-3-3l-3 3m19.5 0l-3 3m3-3l3 3M3.75 17.25l3-3m-3 3l-3-3m19.5 0l-3-3m3-3l3 3" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M12 2.99988L14.1213 5.12119L16.4999 4.75727L17.2426 7.00002L19.4999 8.24266L18.7572 10.5L19.4999 12.7573L17.2426 13.9999L16.4999 16.2426L14.1213 15.8787L12 17.9999L9.87864 15.8787L7.49998 16.2426L6.75723 13.9999L4.49998 12.7573L5.24263 10.5L4.49998 8.24266L6.75723 7.00002L7.49998 4.75727L9.87864 5.12119L12 2.99988ZM12 7.75725L10.5 9.25725L10.5 14.7426L12 16.2426L13.5 14.7426V9.25725L12 7.75725Z" />
            </svg>
        )
    },
    Sun: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M12 3v2.25m6.364.386l-1.591 1.591M21 12h-2.25m-.386 6.364l-1.591-1.591M12 18.75V21m-4.773-4.227l-1.591 1.591M5.25 12H3m4.227-4.773L5.636 5.636M15.75 12a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M12 2.25a.75.75 0 01.75.75v2.25a.75.75 0 01-1.5 0V3a.75.75 0 01.75-.75zM7.5 12a4.5 4.5 0 119 0 4.5 4.5 0 01-9 0zM18.894 6.106a.75.75 0 011.06-1.06l1.591 1.59a.75.75 0 01-1.06 1.061l-1.591-1.59zM21.75 12a.75.75 0 01-.75.75h-2.25a.75.75 0 010-1.5h2.25a.75.75 0 01.75.75zM17.836 17.836a.75.75 0 011.06 1.06l-1.59 1.591a.75.75 0 01-1.061-1.06l1.59-1.591zM12 18a.75.75 0 01.75.75v2.25a.75.75 0 01-1.5 0v-2.25A.75.75 0 0112 18zM7.939 17.836a.75.75 0 011.06-1.06l1.591 1.59a.75.75 0 01-1.06 1.061l-1.591-1.59zM3 12a.75.75 0 01.75-.75h2.25a.75.75 0 010 1.5H3.75A.75.75 0 013 12zM5.061 6.106a.75.75 0 011.06 1.06l-1.59 1.591a.75.75 0 01-1.061-1.06l1.59-1.591z" />
            </svg>
        )
    },
    Table: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M3.375 19.5h17.25m-17.25 0a1.125 1.125 0 01-1.125-1.125v-12.75c0-.621.504-1.125 1.125-1.125h17.25c.621 0 1.125.504 1.125 1.125v12.75c0 .621-.504 1.125-1.125 1.125m-17.25 0h.008v.015h-.008V19.5zm.75-5.25h15.75m-15.75 0v.015h15.75V14.25m-15.75 0h.008v.015h-.008v-.015zm0-4.5h15.75m-15.75 0v.015h15.75V9.75m-15.75 0h.008v.015h-.008V9.75z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M4.5 3.375A2.625 2.625 0 001.875 6v12A2.625 2.625 0 004.5 20.625h15A2.625 2.625 0 0022.125 18V6a2.625 2.625 0 00-2.625-2.625h-15z" />
            </svg>
        )
    },
    Terminal: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M6.429 9.75L2.25 12l4.179 2.25m0-4.5l5.571 3 5.571-3m-11.142 0L12 15.25l5.571-3" />
                <path strokeLinecap="round" strokeLinejoin="round" d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                 <path fillRule="evenodd" d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm6.02-2.47a.75.75 0 011.06 0L12 12.22l2.67-2.67a.75.75 0 111.06 1.06l-3.22 3.22a.75.75 0 01-1.06 0L8.27 10.58a.75.75 0 010-1.06z" clipRule="evenodd" />
            </svg>
        )
    },
    Trash: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M14.74 9l-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 01-2.244 2.077H8.084a2.25 2.25 0 01-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 00-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 013.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 00-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 00-7.5 0" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M16.5 4.5a.75.75 0 01.75.75V6h3.75a.75.75 0 010 1.5H3a.75.75 0 010-1.5h3.75V5.25a.75.75 0 01.75-.75h7.5zM10.5 8.25a.75.75 0 00-1.5 0v8.25a.75.75 0 001.5 0V8.25zm3.75 0a.75.75 0 00-1.5 0v8.25a.75.75 0 001.5 0V8.25z" clipRule="evenodd" />
            </svg>
        )
    },
    ViewColumns: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M9 4.5v15m6-15v15m-10.875 0h15.75c.621 0 1.125-.504 1.125-1.125V5.625c0-.621-.504-1.125-1.125-1.125H4.125C3.504 4.5 3 5.004 3 5.625v12.75c0 .621.504 1.125 1.125 1.125z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path d="M4.5 3.375A2.625 2.625 0 001.875 6v12A2.625 2.625 0 004.5 20.625h15A2.625 2.625 0 0022.125 18V6a2.625 2.625 0 00-2.625-2.625h-15z" />
            </svg>
        )
    },
    XCircle: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M9.75 9.75l4.5 4.5m0-4.5l-4.5 4.5M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M12 2.25c-5.385 0-9.75 4.365-9.75 9.75s4.365 9.75 9.75 9.75 9.75-4.365 9.75-9.75S17.385 2.25 12 2.25zm-1.72 6.97a.75.75 0 10-1.06 1.06L10.94 12l-1.72 1.72a.75.75 0 101.06 1.06L12 13.06l1.72 1.72a.75.75 0 101.06-1.06L13.06 12l1.72-1.72a.75.75 0 10-1.06-1.06L12 10.94l-1.72-1.72z" clipRule="evenodd" />
            </svg>
        )
    },
    XMark: {
        sharp: (props: React.SVGProps<SVGSVGElement>) => (
            <svg fill="none" viewBox="0 0 24 24" strokeWidth={1.5} stroke="currentColor" {...props}>
                <path strokeLinecap="round" strokeLinejoin="round" d="M6 18L18 6M6 6l12 12" />
            </svg>
        ),
        solid: (props: React.SVGProps<SVGSVGElement>) => (
            <svg viewBox="0 0 24 24" fill="currentColor" {...props}>
                <path fillRule="evenodd" d="M5.47 5.47a.75.75 0 011.06 0L12 10.94l5.47-5.47a.75.75 0 111.06 1.06L13.06 12l5.47 5.47a.75.75 0 11-1.06 1.06L12 13.06l-5.47 5.47a.75.75 0 01-1.06-1.06L10.94 12 5.47 6.53a.75.75 0 010-1.06z" clipRule="evenodd" />
            </svg>
        )
    },
};

export type IconName = keyof typeof iconDefs;

export interface IconProps extends React.SVGProps<SVGSVGElement> {
    name: IconName;
    iconSet: IconSet;
}

export const Icon: React.FC<IconProps> = ({ name, iconSet, ...props }) => {
    const iconSetKey = iconSet === 'solid' ? 'solid' : 'sharp';
    const IconComponent = iconDefs[name as keyof typeof iconDefs]?.[iconSetKey] || iconDefs[name as keyof typeof iconDefs]?.sharp;

    if (!IconComponent) {
        // Fallback for icon sets that are not explicitly defined (feather, tabler, lucide)
        const SharpIcon = iconDefs[name as keyof typeof iconDefs]?.sharp;
        if (SharpIcon) {
            return <SharpIcon {...props} />;
        }
        return null; // or a default fallback icon
    }

    return <IconComponent {...props} />;
};