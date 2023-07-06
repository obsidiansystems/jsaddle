const http = require('http');
const ws = require('ws');
const XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;

let port = 3709;
let warpMode = "websocket";

if (process.argv[2]) {
  port = Number(process.argv[2]);
}

if (process.argv[3]) {
  warpMode = process.argv[3];
}
console.log('Running client on port: ', port, ', in mode: ', warpMode);

const jsaddleRoot = 'http://0.0.0.0:' + port;

let request = http.get(jsaddleRoot + '/jsaddle.js', (res) => {
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
    var window = global;
    var result = null;
    var arg = { session_started: (v) => { result = v; } };

    if (warpMode == "websocket") {
      eval(data);
    } else { // "xhr" or "xhronly" modes
      var {connId, core, processReqsViaXHR, connectWebsocket} = function() {
        var dontAutoConnectWebsocket = true;
        eval(data);
        var vals = connectXHR(connId);
        return Object.assign(vals, {connectWebsocket});
      } ();

      while(result == null || warpMode == "xhronly") {
        processReqsViaXHR();
      }

      console.log("Switching from xhr to websocket mode");
      connectWebsocket({core, connId});
    }
  });
});

request.on('error', (err) => {
  console.error(`Encountered an error trying to make a request: ${err.message}`);
});
