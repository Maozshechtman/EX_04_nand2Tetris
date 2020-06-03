package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source

case class Parsing(TokensFile: File = null) {
  private val targetFileName = TokensFile.getName.replaceAll("TMaG.xml", "AST.xml")
  private val targetASTFile = new File(targetFileName)
  private val listTokens = Source.fromFile(TokensFile).getLines().map(node => new Token().XMLNodeToToken(node)).toSeq
  var AstTree = new NonTerminal()
  private var tokensPointer = 1

  // Non Terminal rules
  def ParseClass(): Unit = {
    AstTree = new NonTerminal("class")
    nextToken //class name
    AstTree.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken //{
    AstTree addSubrule (new Terminal("symbol", "{"))
    nextToken
    while (Seq("static", "filed").contains(currentToken().getValue))
      ParseClassVarDec()

    while (Seq("constructor", "method", "function").contains(currentToken().getValue))
      ParseSubroutineDec()
    AstTree.addSubrule(new Terminal("symbol", "}"))


  }

  private def currentToken(): Token = return listTokens(tokensPointer)

  def ParseClassVarDec() {
    var classVarDec = new NonTerminal("classVaeDec")
    nextToken // class var type (id or keyword)
    classVarDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken // Var Name
    classVarDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken // comma or semi-colom
    while (currentToken().getValue.equals(",")) {
      classVarDec.addSubrule(new Terminal("symbol", ","))
      nextToken // Var Name
      classVarDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
      nextToken // semi colom or comma
      AstTree.addSubrule(classVarDec)
    }
    AstTree.addSubrule(new Terminal("symbol", ";")) //endOfrule
    nextToken //continue parsing

  }

  private def nextToken {
    if (tokensPointer < listTokens.length - 1)
      tokensPointer += 1
  }

  def ParseSubroutineDec(): Unit = {
    var subroutineDec = new NonTerminal("subroutineDec")
    subroutineDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue)) // method|function|constarctor
    nextToken // return type (keyword or id )
    subroutineDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken //subroutineDec
    subroutineDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken //(
    subroutineDec.addSubrule(new Terminal("symbol", "("))
    parsePrametersList()
    nextToken //)
    subroutineDec.addSubrule(new Terminal("symbol", ")"))
    parseSubroutineBody()
    AstTree.addSubrule(subroutineDec)
    nextToken // continue parsing
  }

  def parsePrametersList() = {
    var parameterList = new NonTerminal("parameterList")
    nextToken // if the subroutine have prameters
    if (!currentToken().getPattern.equals("symbol")) {
      parameterList.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue)) //var type
      nextToken // var name
      parameterList.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
      nextToken // comma or RPRAN
      while (currentToken().getValue.equals(",")) {
        parameterList.addSubrule(new Terminal("symbol", ","))
        nextToken //var type
        parameterList.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
        nextToken
        AstTree.addSubrule(parameterList)
      }
    }
    AstTree += "</parameterList>"
  }

  def parseSubroutineBody() = {
    var subroutineBody = new NonTerminal("subroutineBody")
    nextToken //{
    subroutineBody.addSubrule(new Terminal("symbol", "{"))
    nextToken
    while (currentToken().getValue.equals("var"))
      parseVarDec()
    parseStatements()
    subroutineBody.addSubrule(new Terminal("symbol", "}"))
    AstTree.addSubrule(subroutineBody)
    nextToken

  }

  def parseVarDec(): Unit = {
    var varDec = new NonTerminal("varDec")
    // keyword var
    varDec.addSubrule(new Terminal("keyword", "var"))
    nextToken //var type (id or keyword)
    varDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken // var name
    varDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken // comma or semicolon
    while (currentToken().getValue.equals(",")) {
      varDec.addSubrule(new Terminal("symbol", ","))
      nextToken // var name
      varDec.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
      nextToken
    }
    varDec.addSubrule(new Terminal("symbol", ";"))
    AstTree.addSubrule(varDec)
    nextToken // continue parsing 
  }

  def parseStatements(): Unit = {
    AstTree += "<statements>\n"
    while (Seq("do", "let", "if", "return").contains(currentToken().getValue))
      parseStatment()
    AstTree += "<statements>\n"
  }

  def parseStatment() {
    currentToken().getValue
    match {
      case "let" => parseLetStament()
      case "if" => parseIfStatement()
      case "while" => parseWhileStatement()
      case "do" => parseDoStatement()
      case "return" => paresReturnStatement()
    }
  }

  def parseLetStament() {}

  def parseIfStatement() {}

  def parseWhileStatement() {}

  def parseDoStatement() {}

  def paresReturnStatement() {}

  def parseExpression(): Unit = {

  }

  def writeASTTofile(): Unit = {
    val writer = new PrintWriter(new FileOutputStream(targetFileName, true))
    writer.append(AstTree.RuletoXml)
    writer.close()
  }

  class Token(pattern: String = null, value: String = null) {
    def getPattern = pattern

    def getValue = value

    def XMLNodeToToken(xmlNode: String): Token = {
      val spilled = xmlNode.split("<_>")
      return new Token(spilled(1), spilled(2))
    }

    override def toString: String = "<" + pattern + "> " + value + " </" + pattern + ">" + "/n"


  }


}
