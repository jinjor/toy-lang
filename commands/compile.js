const fs = require('fs');
const Elm = require('../dest/elm.js');

const fileName = process.argv[2];
const source = fs.readFileSync(fileName, 'utf8');

const parser = Elm.Compile.worker();
let start = Date.now();
parser.ports.parse.send(source);
parser.ports.parsed.subscribe(mes => {
  // console.error(mes);
  console.log('took ' + (Date.now() - start) + '[ms] to parse');
  start = Date.now();
  if (!fs.existsSync('./.toy')) {
    fs.mkdirSync('./.toy');
  }
  const name = fileName.replace('/', '$').replace('.toy', '.ast');
  const file = './.toy/' + name;
  // fs.writeFileSync(file, mes);
});
parser.ports.checked.subscribe(mes => {
  mes[0].forEach(m => {
    console.log(m);
  });
  mes[1].forEach(m => {
    console.log(m);
  });
  console.log('took ' + (Date.now() - start) + '[ms] to check');
  if (!fs.existsSync('./.toy')) {
    fs.mkdirSync('./.toy');
  }
  const name = fileName.replace('/', '$').replace('.toy', '.chk');
  const file = './.toy/' + name;
  // fs.writeFileSync(file, mes);
});
parser.ports.err.subscribe(mes => {
  console.log('error!');
  console.log(mes);
  process.exit(1);
});
