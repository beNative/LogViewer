import React from 'react';
import { FileIcon } from './icons/FileIcon.tsx';

interface DropzoneProps {
  onFileDrop: (files: FileList) => void;
  error: string | null;
}

export const Dropzone: React.FC<DropzoneProps> = ({ onFileDrop, error }) => {
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
      className={`relative p-8 border-2 border-dashed rounded-lg cursor-pointer transition-colors duration-200 
      ${isDragging 
        ? 'border-sky-500 bg-sky-50 dark:bg-sky-900/30' 
        : 'border-gray-300 dark:border-gray-600 hover:border-gray-400 dark:hover:border-gray-500 bg-gray-50/50 hover:bg-gray-100 dark:bg-transparent dark:hover:bg-gray-700/30'
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
            <FileIcon className={`w-16 h-16 mb-4 transition-colors duration-200 ${isDragging ? 'text-sky-500 dark:text-sky-400' : 'text-gray-400 dark:text-gray-500'}`} />
            <p className="font-semibold text-lg text-gray-700 dark:text-gray-300">
                <span className="text-sky-600 dark:text-sky-400">Click to upload</span> or drag and drop
            </p>
            <p className="text-sm">XML or ZIP files</p>
            {error && <p className="mt-4 text-red-700 dark:text-red-400 bg-red-100 dark:bg-red-900/50 p-3 rounded-lg">{error}</p>}
        </div>
    </div>
  );
};
