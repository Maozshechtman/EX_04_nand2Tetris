package Ex04


class NonTerminal extends Rule {
  private var subRules: List[Rule] = List()

  def this(rule: String) {
    this()
    this.ruleName = rule
  }

  def addSubrule(rule: Rule): Unit = {
    subRules = this.subRules :+ rule
  }

  override def RuletoXml: String = {
    return "<" + ruleName + ">\n" + subRules.map(rule => rule.RuletoXml).mkString + "</" + ruleName + ">\n"
  }

}
