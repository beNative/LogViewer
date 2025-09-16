import React from 'react';
import ReactDOM from 'react-dom';

interface TooltipProps {
  content: React.ReactNode;
  children: React.ReactElement;
  disabled?: boolean;
}

export const Tooltip: React.FC<TooltipProps> = ({ content, children, disabled = false }) => {
  const [isVisible, setIsVisible] = React.useState(false);
  const [position, setPosition] = React.useState({ top: 0, left: 0 });
  const triggerRef = React.useRef<HTMLElement>(null);
  const tooltipRef = React.useRef<HTMLDivElement>(null);

  const showTooltip = React.useCallback(() => {
    if (disabled || !triggerRef.current) return;
    
    // Position calculation will happen in useLayoutEffect
    setIsVisible(true);
  }, [disabled]);

  const hideTooltip = React.useCallback(() => {
    setIsVisible(false);
  }, []);

  React.useLayoutEffect(() => {
    if (isVisible && triggerRef.current && tooltipRef.current) {
      const triggerRect = triggerRef.current.getBoundingClientRect();
      const tooltipRect = tooltipRef.current.getBoundingClientRect();
      const GAP = 8; // 8px gap

      let top = triggerRect.top - tooltipRect.height - GAP;
      let left = triggerRect.left + (triggerRect.width / 2) - (tooltipRect.width / 2);

      // Check for vertical overflow
      if (top < GAP) {
        // Not enough space above, position below
        top = triggerRect.bottom + GAP;
      }
      
      // Check for horizontal overflow
      if (left < GAP) {
        left = GAP;
      } else if (left + tooltipRect.width > window.innerWidth - GAP) {
        left = window.innerWidth - tooltipRect.width - GAP;
      }

      setPosition({ top: top + window.scrollY, left: left + window.scrollX });
    }
  }, [isVisible]);

  // Clone the child to attach our own ref and event handlers
  // FIX: Cast children to `React.ReactElement<any>` to fix type inference issue with cloneElement
  const triggerElement = React.cloneElement(children as React.ReactElement<any>, {
    ref: triggerRef,
    onMouseEnter: showTooltip,
    onMouseLeave: hideTooltip,
    onFocus: showTooltip,
    onBlur: hideTooltip,
    'aria-describedby': isVisible ? 'custom-tooltip' : undefined,
  });

  const TooltipPortal = isVisible && !disabled && content ? ReactDOM.createPortal(
    <div
      id="custom-tooltip"
      ref={tooltipRef}
      role="tooltip"
      style={{ top: position.top, left: position.left }}
      className="absolute z-50 px-2.5 py-1.5 text-sm text-white bg-gray-900/90 dark:bg-black/80 rounded-md shadow-lg pointer-events-none animate-fadeIn"
    >
      {content}
    </div>,
    document.body
  ) : null;

  return (
    <>
      {triggerElement}
      {TooltipPortal}
    </>
  );
};