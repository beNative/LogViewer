import React from 'react';
import { Icon } from './icons';
import { IconSet } from '../types';

interface DropzoneProps {
  onFileDrop: (files: FileList) => void;
  error?: string | null;
  iconSet: IconSet;
  title?: string;
  subtitle?: React.ReactNode;
  details?: string;
}

export const Dropzone: React.FC<DropzoneProps> = ({ onFileDrop, error, iconSet, title, subtitle, details }) => {
  const [isDragging, setIsDragging] = React.useState(false);
  const inputRef = React.useRef<HTMLInputElement>(null);

  const handleDragEnter = React.useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragging(true);
  }, []);

  const handleDragLeave = React.useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragging(false);
  }, []);

  const handleDragOver = React.useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
  }, []);

  const handleDrop = React.useCallback((e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    setIsDragging(false);
    onFileDrop(e.dataTransfer.files);
  }, [onFileDrop]);
  
  const handleClick = () => {
    inputRef.current?.click();
  };

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files) {
      onFileDrop(e.target.files);
    }
  };

  return (
    <div
      onClick={handleClick}
      onDragEnter={handleDragEnter}
      onDragLeave={handleDragLeave}
      onDragOver={handleDragOver}
      onDrop={handleDrop}
      className={`relative w-full h-full flex flex-col items-center justify-center p-6 border-2 border-gray-200 dark:border-gray-700 rounded-xl cursor-pointer transition-all duration-200 
      ${isDragging 
        ? 'border-sky-500 bg-sky-50 dark:bg-sky-900/40 ring-4 ring-sky-500/20' 
        : 'hover:border-sky-400 dark:hover:border-sky-500 hover:bg-gray-50 dark:hover:bg-gray-800'
      }`}
    >
        <input 
            ref={inputRef}
            type="file" 
            className="hidden" 
            multiple 
            onChange={handleFileChange}
            accept=".xml,.zip"
        />
        <div className="flex flex-col items-center justify-center text-center text-gray-500 dark:text-gray-400 pointer-events-none">
            <Icon name="ArchiveBox" iconSet={iconSet} className={`w-12 h-12 mb-4 transition-colors duration-200 ${isDragging ? 'text-sky-500 dark:text-sky-400' : 'text-gray-400 dark:text-gray-500'}`} />
            <p className="font-semibold text-lg text-gray-700 dark:text-gray-300">
                {title || 'Drop files here'}
            </p>
            <p className="text-sm">{subtitle || <React.Fragment>or <span className="text-sky-600 dark:text-sky-400 font-semibold">click to browse</span></React.Fragment>}</p>
            <p className="text-xs mt-1">{details || '(XML or ZIP)'}</p>
            {error && <p className="mt-4 text-red-700 dark:text-red-400 bg-red-100 dark:bg-red-900/50 p-3 rounded-lg text-sm">{error}</p>}
        </div>
    </div>
  );
};