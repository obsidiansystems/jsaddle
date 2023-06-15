const http = require('http');
const ws = require('ws');
const XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;


let request = http.get('http://0.0.0.0:3709/jsaddle.js', (res) => {
  if (res.statusCode !== 200) {
    console.error(`Did not get an OK from the server. Code: ${res.statusCode}`);
    res.resume();
    return;
  }

  let data = '';

  res.on('data', (chunk) => {
    data += chunk;
  });

  res.on('close', () => {
    console.log('Retrieved all data');
    eval(data);
    console.log("connected");
  });
});

request.on('error', (err) => {
  console.error(`Encountered an error trying to make a request: ${err.message}`);
});
