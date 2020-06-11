import React, {useEffect, useState, useRef} from 'react';
import MonacoEditor from 'react-monaco-editor';

export default ({children, className: languageClassName, metastring}) => {
  let code = children.replace(/\n$/, '');
  let language = languageClassName;
  if (language) {
    language = languageClassName.slice(9);
  }

  const [lineCount, setLineCount] = useState(code.split('\n').length);

  function onDidMount(editor, monaco) {
    editor.onDidChangeModelContent(function () {
      const newLineCount = editor.getModel().getLineCount();
      setLineCount(newLineCount);
      editor.layout();
    });
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
