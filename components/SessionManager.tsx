import React from 'react';
import { SessionFile, IconSet } from '../types.ts';
import { formatBytes } from '../utils.ts';
import { Icon } from './icons/index.tsx';
import { Tooltip } from './Tooltip.tsx';


const SessionItem: React.FC<{
    session: SessionFile;
    isActive: boolean;
    isDirty: boolean;
    onLoad: (name: string) => void;
    onRename: (oldName: string, newName: string) => Promise<boolean>;
    onDelete: (name: string) => void;
    iconSet: IconSet;
}> = ({ session, isActive, isDirty, onLoad, onRename, onDelete, iconSet }) => {
    const [isRenaming, setIsRenaming] = React.useState(false);
    const [newName, setNewName] = React.useState(session.name);
    const inputRef = React.useRef<HTMLInputElement>(null);

    React.useEffect(() => {
        if (isRenaming) {
            inputRef.current?.focus();
            inputRef.current?.select();
        }
    }, [isRenaming]);
    
    const handleRename = async () => {
        if (newName && newName !== session.name) {
            const success = await onRename(session.name, newName);
            if (success) {
                setIsRenaming(false);
            }
        } else {
            setNewName(session.name);
            setIsRenaming(false);
        }
    };
    
    const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'Enter') handleRename();
        if (e.key === 'Escape') {
            setNewName(session.name);
            setIsRenaming(false);
        }
    };

    const handleDelete = () => {
        if(window.confirm(`Are you sure you want to permanently delete '${session.name}'?`)) {
            onDelete(session.name);
        }
    }

    const handleRenameClick = (e: React.MouseEvent) => {
        e.stopPropagation();
        setIsRenaming(true);
    };

    const handleDeleteClick = (e: React.MouseEvent) => {
        e.stopPropagation();
        handleDelete();
    };

    return (
        <li 
            className={`relative rounded-lg transition-all duration-150 group ${isActive ? 'bg-sky-50 dark:bg-sky-900/40 ring-2 ring-sky-500' : 'hover:bg-gray-100/70 dark:hover:bg-gray-700/40'}`}
        >
            <span className={`absolute left-0 top-2 bottom-2 w-1 rounded-r-full transition-colors ${isActive ? 'bg-sky-500' : 'bg-transparent'}`} />
            <button 
                onClick={() => onLoad(session.name)}
                disabled={isRenaming}
                className="w-full text-left pl-4 pr-16 py-3 flex items-center gap-4 cursor-pointer disabled:cursor-default"
            >
                <div className={`flex-shrink-0 w-10 h-10 rounded-lg flex items-center justify-center ${isActive ? 'bg-sky-200/80 dark:bg-sky-800/60' : 'bg-gray-200 dark:bg-gray-700/60'}`}>
                    <Icon name="Database" iconSet={iconSet} className={`w-6 h-6 transition-colors ${isActive ? 'text-sky-600 dark:text-sky-300' : 'text-gray-500 dark:text-gray-400'}`} />
                </div>
                <div className="flex-grow min-w-0">
                    {isRenaming ? (
                        <input
                            ref={inputRef}
                            type="text"
                            value={newName}
                            onChange={(e) => setNewName(e.target.value)}
                            onBlur={handleRename}
                            onKeyDown={handleKeyDown}
                            onClick={(e) => e.stopPropagation()}
                            className="w-full bg-white dark:bg-gray-700 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition"
                        />
                    ) : (
                        <Tooltip content={session.name}>
                            <p className={`font-semibold truncate ${isActive ? 'text-sky-800 dark:text-sky-200' : 'text-gray-800 dark:text-gray-200'}`}>
                                {session.name}{isActive && isDirty ? '*' : ''}
                            </p>
                        </Tooltip>
                    )}
                    <p className="text-xs text-gray-500 dark:text-gray-400 mt-1">
                        {formatBytes(session.size)} &middot; {new Date(session.mtime).toLocaleString()}
                    </p>
                </div>
            </button>
            {!isRenaming && (
                <div className="absolute right-2 top-1/2 -translate-y-1/2 flex-shrink-0 flex items-center gap-1 opacity-0 group-hover:opacity-100 focus-within:opacity-100 transition-opacity">
                    <Tooltip content="Rename">
                        <button onClick={handleRenameClick} className="p-2 text-gray-500 hover:text-gray-900 dark:hover:text-white rounded-full hover:bg-gray-200 dark:hover:bg-gray-600/60 transition-colors">
                            <Icon name="PencilSquare" iconSet={iconSet} className="w-5 h-5" />
                        </button>
                    </Tooltip>
                    <Tooltip content="Delete">
                        <button onClick={handleDeleteClick} className="p-2 text-gray-500 hover:text-red-500 rounded-full hover:bg-gray-200 dark:hover:bg-gray-600/60 transition-colors">
                            <Icon name="Trash" iconSet={iconSet} className="w-5 h-5" />
                        </button>
                    </Tooltip>
                </div>
            )}
        </li>
    );
};

interface SessionManagerProps {
    sessions: SessionFile[];
    activeSessionName: string | null;
    isDirty: boolean;
    onLoad: (name: string) => void;
    onRename: (oldName: string, newName: string) => Promise<boolean>;
    onDelete: (name: string) => void;
    iconSet: IconSet;
}

export const SessionManager: React.FC<SessionManagerProps> = (props) => {
    return (
        <>
            {props.sessions.length > 0 ? (
                <ul className="space-y-2">
                    {props.sessions.map(session => (
                        <SessionItem
                            key={session.path}
                            session={session}
                            isActive={session.name === props.activeSessionName}
                            isDirty={props.isDirty}
                            onLoad={props.onLoad}
                            onRename={props.onRename}
                            onDelete={props.onDelete}
                            iconSet={props.iconSet}
                        />
                    ))}
                </ul>
            ) : (
                <div className="text-center text-gray-500 dark:text-gray-400 py-10 h-full flex flex-col items-center justify-center">
                    <Icon name="Folder" iconSet={props.iconSet} className="w-16 h-16 text-gray-300 dark:text-gray-600 mx-auto mb-4" />
                    <h3 className="text-lg font-semibold mb-1 text-gray-700 dark:text-gray-300">No Saved Sessions</h3>
                    <p className="text-sm">Create a new session using the panel on the right.</p>
                </div>
            )}
        </>
    );
};