import React from 'react';
import { SessionFile, IconSet } from '../types.ts';
import { formatBytes } from '../utils.ts';
import { Icon } from './icons/index.tsx';


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

    return (
        <li className={`p-3 rounded-lg flex items-center gap-4 transition-colors duration-150 ${isActive ? 'bg-sky-100 dark:bg-sky-900/50 ring-1 ring-sky-500/50' : 'bg-white dark:bg-gray-800'}`}>
            <div className="flex-grow min-w-0">
                {isRenaming ? (
                     <div className="flex items-center gap-2">
                        <input
                            ref={inputRef}
                            type="text"
                            value={newName}
                            onChange={(e) => setNewName(e.target.value)}
                            onBlur={handleRename}
                            onKeyDown={handleKeyDown}
                            className="w-full bg-white dark:bg-gray-700 border-gray-300 dark:border-gray-600 text-gray-900 dark:text-white sm:text-sm rounded-md shadow-sm focus:ring-sky-500 focus:border-sky-500 transition"
                        />
                         <button onClick={() => setIsRenaming(false)} className="p-1 text-gray-500 hover:text-red-500"><Icon name="XCircle" iconSet={iconSet} className="w-5 h-5"/></button>
                         <button onClick={handleRename} className="p-1 text-gray-500 hover:text-green-500"><Icon name="CheckCircle" iconSet={iconSet} className="w-5 h-5"/></button>
                     </div>
                ) : (
                    <p className="font-semibold text-gray-800 dark:text-gray-200 truncate" title={session.name}>
                        {session.name}{isActive && isDirty ? '*' : ''}
                    </p>
                )}
                <p className="text-xs text-gray-500 dark:text-gray-400 mt-1">
                    {formatBytes(session.size)} &middot; {new Date(session.mtime).toLocaleString()}
                </p>
            </div>
            <div className="flex-shrink-0 flex items-center gap-1">
                 <button onClick={() => setIsRenaming(true)} title="Rename" className="p-2 text-gray-500 hover:text-gray-900 dark:hover:text-white rounded-full hover:bg-gray-200 dark:hover:bg-gray-700/60 transition-colors">
                    <Icon name="PencilSquare" iconSet={iconSet} className="w-5 h-5" />
                </button>
                <button onClick={handleDelete} title="Delete" className="p-2 text-gray-500 hover:text-red-500 rounded-full hover:bg-gray-200 dark:hover:bg-gray-700/60 transition-colors">
                    <Icon name="Trash" iconSet={iconSet} className="w-5 h-5" />
                </button>
                <button onClick={() => onLoad(session.name)} title="Load Session" className="p-2 text-sky-600 dark:text-sky-400 hover:text-sky-800 dark:hover:text-sky-300 rounded-full hover:bg-sky-100 dark:hover:bg-sky-900/60 transition-colors">
                    <Icon name="ArrowPath" iconSet={iconSet} className="w-5 h-5" />
                </button>
            </div>
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
                <div className="text-center text-gray-500 dark:text-gray-400 py-10">
                        <p>No saved sessions found.</p>
                        <p className="text-sm mt-1">Create a new session by dropping files above.</p>
                </div>
            )}
        </>
    );
};