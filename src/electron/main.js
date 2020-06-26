const { app, BrowserWindow, dialog } = require('electron');
const execFile = require('child_process').execFile;
const path = require('path');

const MAIN_URL = 'http://localhost:9876';

function createWindow () {
  // Create the browser window.
  let win = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
        nodeIntegration: true,
        enableRemoteModule: true
    }
  });

  // and load the index.html of the app.
    win.loadURL(MAIN_URL);
}

var lispProcess = null;

function runapp () {
    lispExePath = path.resolve(__dirname, "delectus_engine.exe");
    lispProcess = execFile(lispExePath, function(err, stdout, stderr) {
        if(err) {
            console.error(err);
            return;
        } else {
            console.log(stdout);
        }
    });
    createWindow();
}

function quit() {
    if (lispProcess !== null) {
        lispProcess.kill("SIGKILL");
        lispProcess = null;
    }
    app.exit(0);
}

process.on('uncaughtException', function (err) {
  console.log(err);
});

app.on("ready", runapp);
app.on("quit", quit);
