Electron-based app for monitoring C.I. builds from various servers, in a single place.

# Features

* multi server (Bamboo and Travis for now)
* "live" build status list
* tags view (build groups)
* minimizes to sys tray
* desktop notifications
* share config with others

# Download

Binaries can be found at :

http://rvkb.com/build-watcher

Those are zipped apps, no installer, no auto-update.
Download, unzip, and run the executable.

## Mac

Just toss the .app into applications folder, as usual.

## Windows

The exe needs to be along all other files in the expanded dir.
Best create a link to it.

# Development

The app is written in Elm with ports/JS for the Electron APIs.

    npm install
    npm run build
    npm run start/debug

Useful environment vars when developing :

    ELECTRON_ENABLE_LOGGING=1
    BW_DEV=true

## Executables

`release-*` npm scripts are provided in `package.json` for releasing the executable.
