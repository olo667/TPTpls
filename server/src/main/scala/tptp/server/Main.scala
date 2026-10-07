package tptp.server

import org.eclipse.lsp4j.launch.LSPLauncher
import tptp.server.lsp.TptpLanguageServer

object Main {
  def main(args: Array[String]): Unit = {
    val protocolOut = System.out
    System.setOut(System.err) // stray prints must never corrupt the protocol stream
    val server = TptpLanguageServer()
    val launcher = LSPLauncher.createServerLauncher(server, System.in, protocolOut)
    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
  }
}
