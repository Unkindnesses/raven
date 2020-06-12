import React, {useEffect, useState, useRef} from 'react';
import MonacoEditor from 'react-monaco-editor';
import {CopyIcon,CheckCircleFillIcon,ChevronRightIcon,XIcon} from '@primer/octicons-react'
import Clipboard from 'clipboard'

export default ({children, className: languageClassName, metastring}) => {
  let code = children.replace(/\n$/, '');
  let language = languageClassName;
  if (language) {
    language = languageClassName.slice(9);
  }

  const [lineCount, setLineCount] = useState(code.split('\n').length);

  const editor = useRef();

  function layout() {
    const newLineCount = editor.current.getModel().getLineCount();
    setLineCount(newLineCount);
    editor.current.layout();
  }

  function onDidMount(ed, monaco) {
    editor.current = ed;
    editor.current.onDidChangeModelContent(layout);
  }

  async function reset(e) {
    e.preventDefault();
    await editor.current.setValue(code);
    layout();
    editor.current.focus();
  }

  const [showCopied, setShowCopied] = useState(false);
  const copyButton = useRef();

  useEffect(() => {
    let clipboard;

    if (copyButton.current) {
      clipboard = new Clipboard(copyButton.current, {
        text: () => editor.current.getValue()
      });
    }

    return () => {
      if (clipboard) {
        clipboard.destroy();
      }
    };
  }, [copyButton.current]);

  function copy(e) {
    e.preventDefault();
    setShowCopied(true);
    setTimeout(() => setShowCopied(false), 2000);
    editor.current.focus();
  }

  let options = {
    minimap: {enabled: false},
    lineNumbers: false,
    fontSize: "15px",
    lineHeight: "23px",
    scrollBeyondLastLine: false,
    scrollbar: {alwaysConsumeMouseWheel: false}
  }

  let header;
  if (languageClassName) {
    header =
      <div className='code-header'>
        {language}
      </div>;
  }

  return (
    <>
      <div className='code-header'>
        {language ? language : 'Code'}
        <span className='buttons'>
          {language == 'raven' ?
            <a href="#" onClick={e=>e.preventDefault()}><ChevronRightIcon /></a> :
            null}
          <a href="#" ref={copyButton} onClick={copy}>
            {showCopied ? <CheckCircleFillIcon /> : <CopyIcon />}
          </a>
          <a href="#" onClick={reset}><XIcon /></a>
        </span>
      </div>
      <div className='code-wrapper'>
        <div style={{height: (lineCount*23+10) + 'px'}}>
          <MonacoEditor
            height="100%"
            width="100%"
            language="python"
            defaultValue={code}
            options={options}
            theme="vs-dark"
            editorDidMount={onDidMount}
            />
          </div>
        </div>
      </>
  );
};
