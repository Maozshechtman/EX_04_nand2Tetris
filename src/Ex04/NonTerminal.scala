package Ex04

class NonTerminal extends Rule {
  var subRules: Seq[Rule] = Seq()

  def this(rule: String) {
    this()
    this.ruleName = rule
  }

  def addSubrule(rule: Rule): Unit = {
    subRules +: Seq(rule)
  }

  override def RuletoXml: String =
    return "<" + ruleName + ">\n" + subRules.map(rule => rule.RuletoXml) + "</" + ruleName + ">\n"

}
