const { app, BrowserWindow, dialog } = require("electron");
const { spawn } = require("child_process");
const path = require("path");
const fs = require("fs");
const http = require("http");

let mainWindow = null;
let rProcess = null;

const port = 3850;
const host = "127.0.0.1";

function waitForServer(url, retries = 60) {
  return new Promise((resolve, reject) => {
    const attempt = (remaining) => {
      http
        .get(url, (res) => {
          res.resume();
          resolve();
        })
        .on("error", () => {
          if (remaining <= 0) {
            reject(new Error("Timed out while waiting for local Shiny service."));
            return;
          }
          setTimeout(() => attempt(remaining - 1), 1000);
        });
    };
    attempt(retries);
  });
}

function resolveAppRoot() {
  if (app.isPackaged) {
    return path.join(process.resourcesPath, "app-bundle");
  }
  return path.resolve(__dirname, "..");
}

function resolveRuntimeRoot(appRoot) {
  if (app.isPackaged) {
    return path.join(process.resourcesPath, "runtime");
  }
  return path.join(appRoot, "runtime");
}

function locateRscript(runtimeRoot) {
  const embeddedRscript = path.join(runtimeRoot, "R", "bin", "Rscript.exe");
  if (fs.existsSync(embeddedRscript)) {
    return embeddedRscript;
  }
  if (process.env.RSCRIPT_PATH) {
    return process.env.RSCRIPT_PATH;
  }
  return "Rscript";
}

async function createWindow() {
  const appRoot = resolveAppRoot();
  const runtimeRoot = resolveRuntimeRoot(appRoot);
  const runtimeLib = path.join(runtimeRoot, "library");
  const launchScript = path.join(appRoot, "scripts", "launch_app.R");
  const rscript = locateRscript(runtimeRoot);

  if (!fs.existsSync(launchScript)) {
    throw new Error(`Launch script not found: ${launchScript}`);
  }

  const childEnv = {
    ...process.env,
    BULKSEQ_RUNTIME_ROOT: runtimeRoot
  };
  if (fs.existsSync(runtimeLib)) {
    childEnv.BULKSEQ_R_LIBS_USER = runtimeLib;
  }

  rProcess = spawn(rscript, [launchScript, String(port), host], {
    cwd: appRoot,
    windowsHide: true,
    env: childEnv
  });

  rProcess.on("error", async (err) => {
    await dialog.showMessageBox({
      type: "error",
      title: "Startup Error",
      message: "Failed to start local R/Shiny service.",
      detail: String(err)
    });
    app.quit();
  });

  await waitForServer(`http://${host}:${port}`);

  mainWindow = new BrowserWindow({
    width: 1440,
    height: 960,
    minWidth: 1200,
    minHeight: 800,
    autoHideMenuBar: true,
    webPreferences: {
      contextIsolation: true
    }
  });

  await mainWindow.loadURL(`http://${host}:${port}`);
}

app
  .whenReady()
  .then(createWindow)
  .catch(async (err) => {
    await dialog.showMessageBox({
      type: "error",
      title: "Desktop App Error",
      message: "Electron startup failed.",
      detail: String(err)
    });
    app.quit();
  });

app.on("window-all-closed", () => {
  if (rProcess) {
    rProcess.kill();
    rProcess = null;
  }
  if (process.platform !== "darwin") {
    app.quit();
  }
});
