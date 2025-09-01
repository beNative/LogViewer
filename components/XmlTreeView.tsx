import React from 'react';
import { ChevronRightIcon } from './icons/ChevronRightIcon.tsx';

interface ExpansionState {
    version: number;
    expanded: boolean;
}

interface TreeNodeProps {
  node: Node;
  expansion: ExpansionState;
}

// Helper to ignore text nodes that only contain whitespace
const isWhitespaceNode = (node: Node): boolean => {
  return node.nodeType === Node.TEXT_NODE && !node.nodeValue?.trim();
};

// A dedicated component for rendering attributes as a collapsible node
const AttributesNode: React.FC<{ attributes: Attr[]; expansion: ExpansionState }> = ({ attributes, expansion }) => {
    const [isExpanded, setIsExpanded] = React.useState(expansion.expanded);

    React.useEffect(() => {
        setIsExpanded(expansion.expanded);
    }, [expansion.version]);

    if (attributes.length === 0) {
        return null;
    }

    const toggleExpand = (e: React.MouseEvent) => {
        e.stopPropagation();
        setIsExpanded(!isExpanded);
    };

    return (
        <div className="font-mono text-sm">
            <div className="flex items-center cursor-pointer hover:bg-gray-200/70 dark:hover:bg-gray-700/50 rounded py-0.5" onClick={toggleExpand}>
                <ChevronRightIcon className={`w-4 h-4 mr-1 flex-shrink-0 transition-transform ${isExpanded ? 'rotate-90' : ''}`} />
                <span className="text-purple-600 dark:text-purple-400">@[attributes]</span>
            </div>
            {isExpanded && (
                <div className="pl-5 border-l border-dashed border-gray-300 dark:border-gray-600">
                    {attributes.map(attr => (
                        <div key={attr.name} className="flex items-center py-0.5">
                           <div className="w-4 mr-1 flex-shrink-0" /> {/* Spacer for alignment */}
                           <span className="text-amber-600 dark:text-amber-400">{attr.name}</span>
                           <span className="text-gray-500 dark:text-gray-500">="</span>
                           <span className="text-green-600 dark:text-green-400">{attr.value}</span>
                           <span className="text-gray-500 dark:text-gray-500">"</span>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
};


// This component recursively renders a single node in the XML tree
const XmlTreeNode: React.FC<TreeNodeProps> = ({ node, expansion }) => {
  const [isExpanded, setIsExpanded] = React.useState(expansion.expanded);

  React.useEffect(() => {
    setIsExpanded(expansion.expanded);
  }, [expansion.version]);

  if (node.nodeType === Node.TEXT_NODE && node.nodeValue?.trim()) {
    return (
      <div className="pl-5 text-gray-700 dark:text-gray-300 whitespace-pre-wrap">
        {node.nodeValue.trim()}
      </div>
    );
  }
  
  if (node.nodeType !== Node.ELEMENT_NODE) {
    return null;
  }

  const element = node as Element;
  const childNodes = Array.from(element.childNodes).filter(n => !isWhitespaceNode(n));
  const attributes = Array.from(element.attributes);

  const hasChildren = childNodes.length > 0;
  const hasAttributes = attributes.length > 0;
  const isExpandable = hasChildren || hasAttributes;

  const toggleExpand = (e: React.MouseEvent) => {
      e.stopPropagation();
      if (isExpandable) {
        setIsExpanded(!isExpanded);
      }
  }

  return (
    <div className="font-mono text-sm">
      <div className={`flex items-center rounded py-0.5 ${isExpandable ? 'cursor-pointer hover:bg-gray-200/70 dark:hover:bg-gray-700/50' : ''}`} onClick={toggleExpand}>
        {isExpandable ? (
            <ChevronRightIcon className={`w-4 h-4 mr-1 flex-shrink-0 transition-transform ${isExpanded ? 'rotate-90' : ''}`} />
        ) : (
            <div className="w-4 mr-1 flex-shrink-0" />
        )}
        <span className="text-gray-500 dark:text-gray-500">&lt;</span>
        <span className="text-sky-600 dark:text-sky-400">{element.tagName}</span>
        {/* Attributes are no longer rendered inline */}
        <span className="text-gray-500 dark:text-gray-500">{hasChildren ? '>' : ' />'}</span>
      </div>

      {isExpanded && isExpandable && (
        <div className="pl-5 border-l border-gray-300 dark:border-gray-600">
          {hasAttributes && <AttributesNode attributes={attributes} expansion={expansion} />}
          {hasChildren && childNodes.map((child, index) => (
            <XmlTreeNode key={index} node={child} expansion={expansion} />
          ))}
          {hasChildren && <div className="text-gray-500 dark:text-gray-500">&lt;/{element.tagName}&gt;</div>}
        </div>
      )}
    </div>
  );
};


interface XmlTreeViewProps {
    xmlString: string;
}

// This is the main component that takes the raw XML string and renders the tree
export const XmlTreeView: React.FC<XmlTreeViewProps> = ({ xmlString }) => {
    const [expansion, setExpansion] = React.useState<ExpansionState>({ version: 0, expanded: true });

    const collapseAll = () => setExpansion(s => ({ version: s.version + 1, expanded: false }));
    const expandAll = () => setExpansion(s => ({ version: s.version + 1, expanded: true }));
    
    // We use useMemo to avoid re-parsing the XML on every render
    const [doc, error] = React.useMemo(() => {
        try {
            // Remove XML declaration (e.g., <?xml version="1.0"?>) as it can interfere with parsing
            const cleanedXml = xmlString.replace(/<\?xml[^>]*\?>/i, '').trim();
            if (!cleanedXml) {
                return [null, 'Empty XML string.'];
            }
            
            const parser = new DOMParser();
            const parsedDoc = parser.parseFromString(cleanedXml, "application/xml");
            
            // Check for a parsererror element, which indicates an XML parsing failure
            const parserError = parsedDoc.getElementsByTagName("parsererror");
            if (parserError.length > 0) {
                return [null, "Failed to parse XML content."];
            }
            
            return [parsedDoc, null];
        } catch (e) {
            return [null, "An error occurred while parsing XML."];
        }
    }, [xmlString]);

    if (error || !doc?.documentElement) {
        // This should not happen if the parent component checks for validity, but it's a safe fallback
        return (
            <div className="text-red-600 dark:text-red-400 font-mono text-sm p-2">
                Could not display XML tree. {error}
            </div>
        );
    }

    return (
        <div className="bg-gray-100 dark:bg-gray-900 rounded-lg ring-1 ring-gray-200 dark:ring-gray-700">
             <div className="p-2 border-b border-gray-200 dark:border-gray-700 flex items-center gap-2">
                <button
                    onClick={expandAll}
                    className="px-2 py-1 text-xs font-medium text-gray-700 dark:text-gray-300 rounded hover:bg-gray-200 dark:hover:bg-gray-700 transition-colors"
                >
                    Expand All
                </button>
                <button
                    onClick={collapseAll}
                    className="px-2 py-1 text-xs font-medium text-gray-700 dark:text-gray-300 rounded hover:bg-gray-200 dark:hover:bg-gray-700 transition-colors"
                >
                    Collapse All
                </button>
            </div>
            <div className="p-3 overflow-x-auto">
                <XmlTreeNode node={doc.documentElement} expansion={expansion} />
            </div>
        </div>
    );
};