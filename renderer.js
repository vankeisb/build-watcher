// This file is required by the index.html file and will
// be executed in the renderer process for that window.
// All of the Node.js APIs are available in this process.

var {ipcRenderer, remote} = require('electron');
var path = require("path");

const dataFileName = path.join(remote.app.getPath("appData"), "app-name", "build-watcher.json");

const packageJson = require('./package.json');
const appVersion = packageJson.version;
const appName = packageJson.name;

var Elm = require('./dist/app');
var a = Elm.App.fullscreen({
    appName : appName,
    appVersion : appVersion,
    dataFileName: dataFileName
});

function sendToMain(data) {
    ipcRenderer.send('renderer-msg', data)
}

a.ports.saveData.subscribe(function(data) {
    sendToMain({
         kind: "save-data",
         data: data
     });
});

a.ports.loadData.subscribe(function() {
    sendToMain({
        kind: "load-data"
    })
});

a.ports.openURL.subscribe(function(url) {
    sendToMain({
        kind: "open-url",
        data: url
    })
});

a.ports.desktopNotification.subscribe(function(data) {
    let icon;
    if (data.isGreen) {
        icon = 'smile-green.png'
    } else {
        icon = 'smile-red.png'
    }
    const notif = {
        title: data.title,
        body: data.body,
        icon: path.join(__dirname, 'assets', 'notif-icon', icon)
    }
    const n = new Notification(data.title, notif)
    n.onclick = () => {
        sendToMain({
            kind: "notif-clicked"
        })
    }
})

ipcRenderer.on('main-msg', (event, arg) => {
    if (arg.kind === 'data-loaded') {
        if (arg.success) {
            if (arg.loadedFromFile) {
                a.ports.onDataLoadSuccess.send(arg.data);
            } else {
                a.ports.onDataLoadFileNotFound.send(arg.data);
            }
        } else {
            a.ports.onDataLoadError.send(arg.data);
        }
    } else if (arg.kind == 'data-saved') {
        if (arg.success) {
            a.ports.onDataSaveSuccess.send(arg.data);
        } else {
            a.ports.onDataSaveError.send(arg.data);
        }
    }
});
