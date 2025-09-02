import React from 'react';
import { IconSet } from '../types.ts';

// Sharp Icons
import { ArchiveBoxIcon } from './ArchiveBoxIcon.tsx';
import { ArrowPathIcon } from './ArrowPathIcon.tsx';
import { ArrowsPointingInIcon } from './ArrowsPointingInIcon.tsx';
import { ArrowsPointingOutIcon } from './ArrowsPointingOutIcon.tsx';
import { ArrowUpTrayIcon } from './ArrowUpTrayIcon.tsx';
import { BookOpenIcon } from './BookOpenIcon.tsx';
import { BugAntIcon } from './BugAntIcon.tsx';
import { ChartBarIcon } from './ChartBarIcon.tsx';
import { CheckCircleIcon } from './CheckCircleIcon.tsx';
import { ChevronDownIcon } from './ChevronDownIcon.tsx';
import { ChevronLeftIcon } from './ChevronLeftIcon.tsx';
import { ChevronRightIcon } from './ChevronRightIcon.tsx';
import { ClockIcon } from './ClockIcon.tsx';
import { CodeBracketIcon } from './CodeBracketIcon.tsx';
import { CodeBracketSquareIcon } from './CodeBracketSquareIcon.tsx';
import { CogIcon } from './CogIcon.tsx';
import { DatabaseIcon } from './DatabaseIcon.tsx';
import { DocumentPlusIcon } from './DocumentPlusIcon.tsx';
import { DownloadIcon } from './DownloadIcon.tsx';
import { ExclamationTriangleIcon } from './ExclamationTriangleIcon.tsx';
import { FileIcon } from './FileIcon.tsx';
import { FilterIcon } from './FilterIcon.tsx';
import { FolderIcon } from './FolderIcon.tsx';
import { InformationCircleIcon } from './InformationCircleIcon.tsx';
import { PencilSquareIcon } from './PencilSquareIcon.tsx';
import { PlusCircleIcon } from './PlusCircleIcon.tsx';
import { SaveDiskIcon } from './SaveDiskIcon.tsx';
import { SidebarRightIcon } from './SidebarRightIcon.tsx';
import { TableIcon } from './TableIcon.tsx';
import { TerminalIcon } from './TerminalIcon.tsx';
import { TrashIcon } from './TrashIcon.tsx';
import { ViewColumnsIcon } from './ViewColumnsIcon.tsx';
import { XCircleIcon } from './XCircleIcon.tsx';
import { XMarkIcon } from './XMarkIcon.tsx';

const sharpIcons = {
    ArchiveBox: ArchiveBoxIcon,
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
    PencilSquare: PencilSquareIcon,
    PlusCircle: PlusCircleIcon,
    SaveDisk: SaveDiskIcon,
    Save: SaveDiskIcon, // Map "Save" to the correct icon to fix a bug
    SidebarRight: SidebarRightIcon,
    Table: TableIcon,
    Terminal: TerminalIcon,
    Trash: TrashIcon,
    ViewColumns: ViewColumnsIcon,
    XCircle: XCircleIcon,
    XMark: XMarkIcon,
};

// Fallback to sharp icons if solid or feather sets are not available
const solidIcons = sharpIcons;
const featherIcons = sharpIcons;

const iconSets = {
  sharp: sharpIcons,
  solid: solidIcons,
  feather: featherIcons,
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
