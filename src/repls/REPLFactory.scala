package repls

object REPLFactory {

    def makeIntREPL() : REPL = new IntREPL()
    def makeMultiSetREPL() : REPL = new MultiSetREPL()

}
