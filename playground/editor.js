const parser = Elm.Compile.worker();

let result = '';

function resetLogBuffer() {
  result = '';
}

function log(s) {
  result = result + s + '\n';
  document.getElementById('result').textContent = result;
}

function compile(source) {
  resetLogBuffer()
  let start = Date.now();
  parser.ports.parse.send(source + '\n');
}

setTimeout(() => {
  const source = localStorage.getItem('toy.playground.source');
  const editorElement = document.getElementById('editor');
  editorElement.textContent = source || 'main : String\nmain = "hello, world"\n';
  var editor = ace.edit(editorElement);
  setTimeout(() => {
    editorElement.classList.add('ready');
    if (editor.getValue().trim() !== '') {
      compile(editor.getValue());
    }
  }, 50);

  editor.setTheme("ace/theme/monokai");
  editor.getSession().setMode("ace/mode/elm");
  editor.on('change', () => {
    compile(editor.getValue());
  });
  editor.getSession().on('change', () => {
    localStorage.setItem('toy.playground.source', editor.getValue());
  });
  parser.ports.parsed.subscribe(mes => {
    // log(mes);
  });
  parser.ports.checked.subscribe(mes => {
    const errors = mes[0];
    const interfaces = mes[1];
    resetLogBuffer();
    log([errors.join('\n'), interfaces.join('\n')].join('\n'));
    Object.keys(editor.getSession().getMarkers(true)).forEach(id => {
      editor.getSession().removeMarker(id);
    });
    console.log('interfaces');
    console.log(interfaces.join('\n'));

    const annotations = errors.map(e => {
      const splitted = e.split(' ');
      const startRow = +splitted[0].split(':')[0] - 1;
      const startCol = +splitted[0].split(':')[1] - 1;
      const endRow = +splitted[1].split(':')[0] - 1;
      const endCol = +splitted[1].split(':')[1] - 1;
      var Range = ace.require("ace/range").Range;
      editor.session.addMarker(new Range(startRow, startCol, endRow, endCol), "error_range", "background", true);
      return {
        row: startRow,
        column: startCol,
        text: splitted.slice(2).join(' '),
        type: 'error'
      };
    });
    editor.getSession().setAnnotations(annotations);
  });
  parser.ports.generated.subscribe(code => {
    resetLogBuffer();
    log(code);
  });
  parser.ports.err.subscribe(e => {
    log(e);
    const splitted = e.split(' ');
    const annotations = [{
      row: +splitted[0].split(':')[0] - 1,
      column: +splitted[0].split(':')[1] - 1,
      text: splitted.slice(1).join(' '),
      type: 'error'
    }];
    editor.getSession().setAnnotations(annotations);
  });
});
