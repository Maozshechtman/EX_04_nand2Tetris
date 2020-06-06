package Ex04

import java.io.{File, FileOutputStream, PrintWriter}

import scala.io.Source

//statement:               ifStatement | whileStatement  | letStatement
//statements:              statements*
//ifStatement:             keyword('if') symbol('(') expression symbol(')') symbol('{') statements symbol('}')
//whileStatement:          keyword('while') symbol('(') expression symbol(')') symbol('{') statements symbol('}')
//letStatement:            varName symbol('=') expression symbol(';')
//expression:              term (op term)*
//term:                    identifier | integerConstant
//op:                      symbol('+' | '-' | '=' | '>' | '<')


case class Parsing(TokensFile: File = null) {
  private val targetFileName = TokensFile.getName.replaceAll("TMaG.xml", "AST.xml")
  private val targetASTFile = new File(targetFileName)
  private val listTokens = Source.fromFile(TokensFile).getLines().map(node => new Token().XMLNodeToToken(node)).toSeq
  var AstTree = new NonTerminal()
  private var tokensPointer = 1
  val operators = Seq("+", "-", "=", "&lt", "&gt", "&amp")
  val terms = Seq("identifier", "integerConstant", "StringConstant")
  val constKeyword = Seq("null", "this", "true", "false")

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
    subroutineBody.addSubrule(parseStatements())
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

  def parseStatements(): NonTerminal = {
    var statments = new NonTerminal("statements")
    while (Seq("do", "let", "if", "return").contains(currentToken().getValue))
      parseStatment(statments)
    return statments
  }

  def parseStatment(root: NonTerminal) {
    root.addSubrule(currentToken().getValue
    match {
      case "let" => parseLetStament()
      case "if" => parseIfStatement()
      case "while" => parseWhileStatement()
      case "do" => parseDoStatement()
      case "return" => paresReturnStatement()
    })
  }

  def parseLetStament(): NonTerminal = {
    var letStatement = new NonTerminal("letStatement")
    letStatement.addSubrule(new Terminal("keyword", "let"))
    nextToken // var name
    letStatement.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken // equal or [
    if (currentToken().getValue.equals("[")) {
      letStatement.addSubrule(new Terminal("symbol", "["))
      letStatement.addSubrule(parseExpression())
      nextToken
      letStatement.addSubrule(new Terminal("symbol", "]"))
      nextToken //equal
    }
    letStatement.addSubrule(new Terminal("symbol", "="))
    nextToken // expression
    letStatement.addSubrule(parseExpression())
    nextToken //;
    letStatement.addSubrule(new Terminal("symbol", ";"))
    return letStatement
  }

  def parseIfStatement(): NonTerminal = {
    var ifstatement = new NonTerminal("ifStatement")
    ifstatement.addSubrule(new Terminal("keyword", "if"))
    nextToken //(
    ifstatement.addSubrule(new Terminal("symbol", "("))
    nextToken //boolean expression
    ifstatement.addSubrule(parseExpression())
    ifstatement.addSubrule(new Terminal("symbol", ")"))
    nextToken
    ifstatement.addSubrule(new Terminal("symbol", "{"))
    nextToken
    //TODO: change return type of Statments to NonTerminal
    ifstatement.addSubrule(parseStatements())
    ifstatement.addSubrule(new Terminal("symbol", "}"))
    nextToken
    if (currentToken().getValue.equals("else")) {
      ifstatement.addSubrule(new Terminal("keyword", "else"))
      nextToken
      ifstatement.addSubrule(new Terminal("symbol", "{"))
      nextToken
      ifstatement.addSubrule(parseStatements())
      ifstatement.addSubrule(new Terminal("symbol", "}"))
    }

    return ifstatement
  }

  def parseWhileStatement(): NonTerminal = {
    var whileStatement = new NonTerminal("whileStatement")
    nextToken
    whileStatement.addSubrule(new Terminal("symbol", "("))
    nextToken
    whileStatement.addSubrule(parseExpression())
    whileStatement.addSubrule(new Terminal("symbol", ")"))
    nextToken
    whileStatement.addSubrule(new Terminal("symbol", "{"))
    whileStatement.addSubrule(parseStatements())
    whileStatement.addSubrule(new Terminal("symbol", "}"))


    return whileStatement
  }


  def parseExpressionList(): NonTerminal = {
    //TODO:complete this rule
    var expressionList = new NonTerminal("expressionList")
    expressionList.addSubrule(parseExpression())
    while (currentToken().getValue.equals(",")) {
      expressionList.addSubrule(new Terminal("symbol", ","))
      nextToken
      expressionList.addSubrule(parseExpression())
      nextToken // comma or RPRAN
    }
    return expressionList
  }

  def parseSubRoutineCall(doStatement: NonTerminal) = {
    // subroutine name or class or var name (all are identifiers)
    doStatement.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
    nextToken //Dot or (
    if (currentToken().getValue.equals(".")) {
      doStatement.addSubrule(new Terminal("symbol", "."))
      nextToken //subroutineName <id>
      doStatement.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
      nextToken //(
    }
    doStatement.addSubrule(new Terminal("symbol", "("))
    nextToken
    if (terms.contains(currentToken().getPattern) || constKeyword.contains(currentToken().getValue))
      doStatement.addSubrule(parseExpressionList())
    doStatement.addSubrule(new Terminal("symbol", ")"))
    nextToken // continue parsing


  }

  def parseDoStatement(): NonTerminal = {
    var doStatement = new NonTerminal("doStatement")
    nextToken //subroutine call (name+parametmeter list)
    parseSubRoutineCall(doStatement)
    doStatement.addSubrule(new Terminal("symbol", ";"))
    return doStatement
  }

  def paresReturnStatement(): NonTerminal = {
    var returnStatment = new NonTerminal("returnStatement")
    returnStatment.addSubrule(new Terminal("keyword", "return"))
    nextToken // ; or experssion
    if (!currentToken().getValue.equals(";"))
      returnStatment.addSubrule(parseExpression())
    returnStatment.addSubrule(new Terminal("symbol", ";"))
    return returnStatment
  }

  def parseExpression(): NonTerminal = {
    //expression:              term (op term)*
    var Expression = new NonTerminal()
    Expression.addSubrule(parseTerm)
    nextToken //operator
    while (operators.contains(currentToken().getValue)) {
      Expression.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
      nextToken //term
      Expression.addSubrule(parseTerm)
    }
    return Expression
  }

  //TODO: return/check/fix error nonTerm
  def parseTerm(): NonTerminal = {
    var Term = new NonTerminal("term")
    if (terms.contains(currentToken().getPattern)) {
      if (listTokens(tokensPointer + 1).getValue.equals("("))
        parseSubRoutineCall(Term)
      else {
        Term.addSubrule(new Terminal(currentToken().getPattern, currentToken().getValue))
        nextToken //
      }

      if (currentToken().getValue.equals("[")) {
        Term.addSubrule(new Terminal("symbol", "["))
        Term.addSubrule(parseExpression())
        Term.addSubrule(new Terminal("symbol", "]"))
      }
    }
    else //'(' or unaryOp
    {
      if (currentToken().getValue.equals("(")) {
        Term.addSubrule(new Terminal("symbol", "("))
        nextToken
        Term.addSubrule(parseExpression())
        nextToken
        Term.addSubrule(new Terminal("symbol", ")"))
      }
      else //UnaryOP
      {
        Term.addSubrule(new Terminal(currentToken().getValue, currentToken().getValue))
      }
    }
    return Term
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
