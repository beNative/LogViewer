import React from 'react';
import { IconSet } from '../../types';

import { ArchiveBoxIcon } from './ArchiveBoxIcon';
import { ArrowLeftRightIcon } from './ArrowLeftRightIcon';
import { ArrowPathIcon } from './ArrowPathIcon';
import { ArrowsPointingInIcon } from './ArrowsPointingInIcon';
import { ArrowsPointingOutIcon } from './ArrowsPointingOutIcon';
import { ArrowUpTrayIcon } from './ArrowUpTrayIcon';
import { BookOpenIcon } from './BookOpenIcon';
import { BugAntIcon } from './BugAntIcon';
import { ChartBarIcon } from './ChartBarIcon';
import { CheckCircleIcon } from './CheckCircleIcon';
import { ChevronDownIcon } from './ChevronDownIcon';
import { ChevronLeftIcon } from './ChevronLeftIcon';
import { ChevronRightIcon } from './ChevronRightIcon';
import { ClockIcon } from './ClockIcon';
import { CodeBracketIcon } from './CodeBracketIcon';
import { CodeBracketSquareIcon } from './CodeBracketSquareIcon';
import { CogIcon } from './CogIcon';
import { DatabaseIcon } from './DatabaseIcon';
import { DocumentPlusIcon } from './DocumentPlusIcon';
import { DownloadIcon } from './DownloadIcon';
import { ExclamationTriangleIcon } from './ExclamationTriangleIcon';
import { FileIcon } from './FileIcon';
import { FilterIcon } from './FilterIcon';
import { FolderIcon } from './FolderIcon';
import { InformationCircleIcon } from './InformationCircleIcon';
import { MoonIcon } from './MoonIcon';
import { PencilSquareIcon } from './PencilSquareIcon';
import { PlusCircleIcon } from './PlusCircleIcon';
import { SaveDiskIcon } from './SaveDiskIcon';
import { SidebarRightIcon } from './SidebarRightIcon';
import { SunIcon } from './SunIcon';
import { TableIcon } from './TableIcon';
import { TerminalIcon } from './TerminalIcon';
import { TrashIcon } from './TrashIcon';
import { ViewColumnsIcon } from './ViewColumnsIcon';
import { XCircleIcon } from './XCircleIcon';
import { XMarkIcon } from './XMarkIcon';

// Note: An icon named "Clipboard" is used in `ContextMenu.tsx` but its component file is not provided.
// It will gracefully fail to render, logging a warning to the console.

const iconMap = {
    ArchiveBox: ArchiveBoxIcon,
    ArrowLeftRight: ArrowLeftRightIcon,
    ArrowPath: ArrowPathIcon,
    ArrowsPointingIn: ArrowsPointingInIcon,
    ArrowsPointingOut: ArrowsPointingOutIcon,
    ArrowUpTray: ArrowUpTrayIcon,
    BookOpen: BookOpenIcon,
    BugAnt: BugAntIcon,
    ChartBar: ChartBarIcon,
    CheckCircle: CheckCircleIcon,
    ChevronDown: ChevronDownIcon,
    ChevronLeft: ChevronLeftIcon,
    ChevronRight: ChevronRightIcon,
    Clock: ClockIcon,
    CodeBracket: CodeBracketIcon,
    CodeBracketSquare: CodeBracketSquareIcon,
    Cog: CogIcon,
    Database: DatabaseIcon,
    DocumentPlus: DocumentPlusIcon,
    Download: DownloadIcon,
    ExclamationTriangle: ExclamationTriangleIcon,
    File: FileIcon,
    Filter: FilterIcon,
    Folder: FolderIcon,
    InformationCircle: InformationCircleIcon,
    Moon: MoonIcon,
    PencilSquare: PencilSquareIcon,
    PlusCircle: PlusCircleIcon,
    SaveDisk: SaveDiskIcon,
    Save: SaveDiskIcon, // Alias for SaveDiskIcon used in SessionManager
    SidebarRight: SidebarRightIcon,
    Sun: SunIcon,
    Table: TableIcon,
    Terminal: TerminalIcon,
    Trash: TrashIcon,
    ViewColumns: ViewColumnsIcon,
    XCircle: XCircleIcon,
    XMark: XMarkIcon,
};

export type IconName = keyof typeof iconMap;

interface IconProps extends React.SVGProps<SVGSVGElement> {
  name: IconName;
  iconSet: IconSet;
}

export const Icon: React.FC<IconProps> = ({ name, iconSet, ...props }) => {
  const IconComponent = (iconMap as any)[name];
  
  if (!IconComponent) {
    // Gracefully handle missing icons (e.g., ClipboardIcon)
    console.warn(`Icon "${name}" not found.`);
    return null;
  }
  
  // The iconSet prop is passed but currently unused as there's only one icon style.
  // This is where logic for different icon sets would go.
  return <IconComponent {...props} />;
};
