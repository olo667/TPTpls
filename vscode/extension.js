const fs = require('fs');
const path = require('path');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

exports.activate = ctx => {
  // the packaged extension bundles the jar; a checkout uses the one sbt server/assembly builds
  const bundled = path.join(ctx.extensionPath, 'server', 'tptp-lsp.jar');
  const jar = fs.existsSync(bundled)
    ? bundled
    : path.join(ctx.extensionPath, '..', 'server', 'target', 'scala-3.3.8', 'tptp-lsp.jar');
  client = new LanguageClient(
    'tptp',
    'TPTP LSP',
    { command: 'java', args: ['-jar', jar] },
    { documentSelector: [{ scheme: 'file', language: 'tptp' }] }
  );
  client.start();
  ctx.subscriptions.push(client);
};

exports.deactivate = () => client?.stop();
