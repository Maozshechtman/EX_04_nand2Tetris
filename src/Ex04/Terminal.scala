package Ex04

class Terminal extends Rule {
  var terminalName = ""

  //C-TOR
  def this(rule: String, terminal: String) {
    this()
    this.terminalName = terminal
    this.ruleName = rule
  }

  override def RuletoXml: String = {
    return "<" + ruleName + ">" + terminalName + "</" + ruleName + ">\n"
  }

}