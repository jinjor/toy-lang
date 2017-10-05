const fs = require('fs');
const argv = require('argv');
const Elm = require('../dest/elm.js');

const args = argv.option({
  name: 'input',
  short: 'i',
  type: 'string',
  description: 'pass code through stdin',
  example: `'toy compile -i'`
}).option({
  name: 'code',
  short: 'c',
  type: 'string',
  description: 'pass code directly',
  example: `'toy compile -c "a = 1"'`
}).run();

// console.log(args);

const fileName = args.targets[0];

function getSource() {
  return new Promise((resolve, reject) => {
    if (args.options.input) {
      let s = '';
      process.stdin.setEncoding('utf8');
      process.stdin.on('readable', () => {
        const chunk = process.stdin.read();
        if (chunk !== null) {
          s += chunk;
        }
      });
      process.stdin.on('end', () => {
        resolve(s);
      });
    } else if (args.options.code) {
      resolve(args.options.code + '\n');
    } else {
      try {
        resolve(fs.readFileSync(fileName, 'utf8'));
      } catch (e) {
        reject(e);
      }
    }
  });
}

function ensureLocalCacheDir() {
  if (!fs.existsSync('./.toy')) {
    fs.mkdirSync('./.toy');
  }
}

function saveAST(data) {
  if (!fileName) {
    return;
  }
  ensureLocalCacheDir();
  const name = 'tmp.ast';
  const file = './.toy/' + name;
  fs.writeFileSync(file, data);
}

function saveInterface(data) {
  if (!fileName) {
    return;
  }
  ensureLocalCacheDir();
  const name = 'tmp.interface';
  const file = './.toy/' + name;
  fs.writeFileSync(file, data);
}

getSource().then(source => {
  const parser = Elm.Compile.worker();
  let start = Date.now();
  parser.ports.parse.send(source);
  parser.ports.parsed.subscribe(mes => {
    // console.error(mes);
    console.log('took ' + (Date.now() - start) + '[ms] to parse');
    start = Date.now();
    saveAST();
  });
  parser.ports.checked.subscribe(mes => {
    mes[0].forEach(m => {
      console.log(m);
    });
    mes[1].forEach(m => {
      console.log(m);
    });
    console.log('took ' + (Date.now() - start) + '[ms] to check');
    saveInterface();
  });
  parser.ports.err.subscribe(mes => {
    console.log('error!');
    console.log(mes);
    process.exit(1);
  });
});
