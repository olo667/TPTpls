const path = require('path');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

exports.activate = ctx => {
  const jar = path.join(ctx.extensionPath, '..', 'server', 'target', 'scala-3.3.8', 'tptp-lsp.jar');
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
