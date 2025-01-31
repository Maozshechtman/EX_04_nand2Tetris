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
  private val targetFileName = TokensFile.getPath.replaceAll("TMaG.xml", "AST.xml")
  private val targetASTFile = new File(targetFileName)
  private val listTokens = Source.fromFile(TokensFile).getLines().map(node => new Token().XMLNodeToToken(node)).toSeq
  private val operators = Seq("+", "-", "=", "&lt;", "&gt;", "&amp;", "/", "|", "*")
  private val terms = Seq("identifier", "integerConstant", "stringConstant")
  private val constKeyword = Seq("null", "this", "true", "false")
  private val unaryOperators = Seq("~", "-")
  private var AstTree = new NonTerminal()
  private var tokensPointer = 1

  //Parsing tools
  def lookAhead(): Token = {
    if (tokensPointer + 1 < listTokens.length - 1)
      return listTokens(tokensPointer + 1)
    else
      return currentToken()
  }

  // Non Terminal rules
  def Parse(): Unit = {
    ParseClass()
    writeASTTofile()
  }

  def parseStatements(): NonTerminal = {
    var statments = new NonTerminal("statements")
    while (Seq("do", "let", "if", "return", "while").contains(currentToken().getValue)) {
      parseStatment(statments)
    }
    return statments
  }

  //terminal rules
  private def parseClassName(className: String): Terminal = {
    nextToken
    return new Terminal("identifier", className)
  }

  private def parseVarName(name: String): Terminal = {
    nextToken
    return new Terminal("identifier", name)
  }

  private def parseSunbRoutineName(subRoutineName: String): Terminal = {
    nextToken
    return new Terminal("identifier", subRoutineName)

  }

  private def parseType(varType: Token): Terminal = {
    nextToken
    return new Terminal(varType.getPattern, varType.getValue)
  }

  private def parseKeyword(keyword: String): Terminal = {
    nextToken
    return new Terminal("keyword", keyword)
  }

  private def parseSymbol(symbol: String): Terminal = {
    nextToken
    return new Terminal("symbol", symbol)
  }

  private def parseStringConstant(str: String): Terminal = {
    nextToken
    return new Terminal("stringConstant", str)
  }

  private def parseintegerConstant(number: String): Terminal = {
    nextToken
    return new Terminal("integerConstant", number)
  }

  new Terminal()

  private def currentToken(): Token = return listTokens(tokensPointer)

  private def nextToken {
    if (tokensPointer < listTokens.length - 1)
      tokensPointer += 1
  }

  private def ParseClass(): Unit = {
    AstTree = new NonTerminal("class")
    AstTree.addSubrule(parseKeyword("class"))
    AstTree.addSubrule(parseClassName(currentToken().getValue))
    AstTree addSubrule (parseSymbol("{"))
    while (Seq("static", "field").contains(currentToken().getValue)) {
      AstTree.addSubrule(ParseClassVarDec())
    }
    while (Seq("constructor", "method", "function").contains(currentToken().getValue)) {
      ParseSubroutineDec()
    }
    AstTree.addSubrule(parseSymbol("}"))


  }

  private def ParseClassVarDec(): NonTerminal = {
    var classVarDec = new NonTerminal("classVarDec")
    classVarDec.addSubrule(parseKeyword(currentToken().getValue))
    classVarDec.addSubrule(parseType(currentToken()))
    classVarDec.addSubrule(parseVarName(currentToken().getValue))
    while (currentToken().getValue.equals(",")) {
      classVarDec.addSubrule(parseSymbol(","))
      classVarDec.addSubrule(parseVarName(currentToken().getValue))
    }
    classVarDec.addSubrule(parseSymbol(";"))
    //endOfrule
    return classVarDec

  }

  private def ParseSubroutineDec(): Unit = {
    var subroutineDec = new NonTerminal("subroutineDec")
    // method|function|constarctor
    subroutineDec.addSubrule(parseKeyword(currentToken().getValue))
    //return type
    subroutineDec.addSubrule(parseType(currentToken()))
    //subroutine name
    subroutineDec.addSubrule(parseSunbRoutineName(currentToken().getValue))
    //(
    subroutineDec.addSubrule(parseSymbol("("))
    //Parameter List
    subroutineDec.addSubrule(parsePrametersList())
    subroutineDec.addSubrule(parseSymbol(")"))
    subroutineDec.addSubrule(parseSubroutineBody())
    AstTree.addSubrule(subroutineDec)
  }

  private def parsePrametersList(): NonTerminal = {
    var parameterList = new NonTerminal("parameterList")
    if (!currentToken().getValue.equals(")")) {
      //var type
      parameterList.addSubrule(parseType(currentToken()))
      // var name
      parameterList.addSubrule(parseVarName(currentToken().getValue))
      // comma or RPRAN
      while (currentToken().getValue.equals(",")) {
        parameterList.addSubrule(parseSymbol(","))
        //var type
        parameterList.addSubrule(parseType(currentToken()))
        //varName
        parameterList.addSubrule(parseVarName(currentToken().getValue))


      }

    }
    return parameterList
  }

  private def parseSubroutineBody(): NonTerminal = {
    var subroutineBody = new NonTerminal("subroutineBody")
    //{
    subroutineBody.addSubrule(parseSymbol("{"))
    while (currentToken().getValue.equals("var"))
      subroutineBody.addSubrule(parseVarDec())
    subroutineBody.addSubrule(parseStatements())
    subroutineBody.addSubrule(parseSymbol("}"))
    return subroutineBody


  }

  private def parseVarDec(): NonTerminal = {
    var varDec = new NonTerminal("varDec")
    // keyword var
    varDec.addSubrule(parseKeyword("var"))
    //var type (id or keyword)
    varDec.addSubrule(parseType(currentToken()))
    // var name
    varDec.addSubrule(parseVarName(currentToken().getValue))
    // comma or semicolon
    while (currentToken().getValue.equals(",")) {
      varDec.addSubrule(parseSymbol(","))
      varDec.addSubrule(parseVarName(currentToken().getValue))
    }
    varDec.addSubrule(parseSymbol(";"))
    return varDec

  }

  private def parseStatment(root: NonTerminal) {
    root.addSubrule(currentToken().getValue
    match {
      case "let" => parseLetStament()
      case "if" => parseIfStatement()
      case "while" => parseWhileStatement()
      case "do" => parseDoStatement()
      case "return" => paresReturnStatement()
    })
  }

  private def parseLetStament(): NonTerminal = {
    var letStatement = new NonTerminal("letStatement")
    letStatement.addSubrule(parseKeyword("let"))
    // var name
    letStatement.addSubrule(parseVarName(currentToken().getValue))
    // equal or [
    if (currentToken().getValue.equals("[")) {
      letStatement.addSubrule(parseSymbol("["))
      letStatement.addSubrule(parseExpression())

      letStatement.addSubrule(parseSymbol("]"))

    }
    letStatement.addSubrule(parseSymbol("="))
    letStatement.addSubrule(parseExpression())
    letStatement.addSubrule(parseSymbol(";"))
    return letStatement
  }

  private def parseIfStatement(): NonTerminal = {
    var ifstatement = new NonTerminal("ifStatement")
    ifstatement.addSubrule(parseKeyword("if"))
    //(
    ifstatement.addSubrule(parseSymbol("("))
    //boolean expression
    ifstatement.addSubrule(parseExpression())
    //)
    ifstatement.addSubrule(parseSymbol(")"))
    ifstatement.addSubrule(parseSymbol("{"))
    ifstatement.addSubrule(parseStatements())
    ifstatement.addSubrule(parseSymbol("}"))
    if (currentToken().getValue.equals("else")) {
      ifstatement.addSubrule(parseKeyword("else"))
      ifstatement.addSubrule(parseSymbol("{"))
      ifstatement.addSubrule(parseStatements())
      ifstatement.addSubrule(parseSymbol("}"))
    }

    return ifstatement
  }

  private def parseWhileStatement(): NonTerminal = {
    var whileStatement = new NonTerminal("whileStatement")
    whileStatement.addSubrule(parseKeyword("while"))
    whileStatement.addSubrule(parseSymbol("("))
    whileStatement.addSubrule(parseExpression())
    whileStatement.addSubrule(parseSymbol(")"))
    whileStatement.addSubrule(parseSymbol("{"))
    // add Statments
    whileStatement.addSubrule(parseStatements())
    whileStatement.addSubrule(parseSymbol("}"))
    return whileStatement
  }


  private def parseExpressionList(): NonTerminal = {
    var expressionList = new NonTerminal("expressionList")
    if (currentToken().getValue.equals(")"))
      return expressionList
    expressionList.addSubrule(parseExpression())
    if (currentToken().getValue.equals(")"))
      return expressionList
    while (currentToken().getValue.equals(",")) {
      expressionList.addSubrule(parseSymbol(","))
      expressionList.addSubrule(parseExpression())

    }
    return expressionList
  }

  private def parseSubRoutineCall(doStatement: NonTerminal) = {
    // SubroutineName| classNmae | Varname (all are identifiers)
    doStatement.addSubrule(parseVarName(currentToken().getValue))
    //Dot or (
    if (currentToken().getValue.equals(".")) {
      doStatement.addSubrule(parseSymbol("."))
      doStatement.addSubrule(parseSunbRoutineName(currentToken().getValue))
    }
    doStatement.addSubrule(parseSymbol("("))
    doStatement.addSubrule(parseExpressionList())
    doStatement.addSubrule(parseSymbol(")"))

  }

  private def parseDoStatement(): NonTerminal = {
    var doStatement = new NonTerminal("doStatement")
    doStatement.addSubrule(parseKeyword("do"))
    //subroutine call (name+parametmeter list)
    parseSubRoutineCall(doStatement)
    doStatement.addSubrule(parseSymbol(";"))
    return doStatement
  }

  private def paresReturnStatement(): NonTerminal = {
    var returnStatment = new NonTerminal("returnStatement")
    returnStatment.addSubrule(parseKeyword("return"))
    // ; or experssion
    if (!currentToken().getValue.equals(";"))
      returnStatment.addSubrule(parseExpression())
    returnStatment.addSubrule(parseSymbol(";"))
    return returnStatment
  }

  private def parseExpression(): NonTerminal = {
    //expression:              term (op term)*
    var Expression = new NonTerminal("expression")
    Expression.addSubrule(parseTerm)
    if (operators.contains(currentToken().getValue)) {
      while (operators.contains(currentToken().getValue)) {
        //op=symbol
        Expression.addSubrule(parseSymbol(currentToken().getValue))
        //term
        Expression.addSubrule(parseTerm)
      }
    }
    return Expression
  }


  private def parseTerm(): NonTerminal = {
    var Term = new NonTerminal("term")
    //KeywordCase={null,true,false,this}=keyword
    if (constKeyword.contains(currentToken().getValue)) {
      Term.addSubrule(parseKeyword(currentToken().getValue))
      return Term
    }
    //String constant
    if (currentToken().getPattern.equals("stringConstant")) {
      Term.addSubrule(parseStringConstant(currentToken().getValue))
      return Term
    }
    //integerConstant
    if (currentToken().getPattern.equals("integerConstant")) {
      Term.addSubrule(parseintegerConstant(currentToken().getValue))
      return Term
    }
    //varName|VarNameExpression|.SubRoutineName
    if (currentToken().getPattern.equals("identifier")) {
      //subRoutineCall
      if (lookAhead().getValue.equals(".") || lookAhead().getValue.equals("(")) {
        parseSubRoutineCall(Term)
        return Term
      }
      Term.addSubrule(parseVarName(currentToken().getValue))
      if (currentToken().getValue.equals("[")) {
        Term.addSubrule(parseSymbol("["))
        Term.addSubrule(parseExpression())
        Term.addSubrule(parseSymbol("]"))
      }
      return Term
    }
    //(expression)
    else if (currentToken().getValue.equals("(")) {
      Term.addSubrule(parseSymbol("("))
      Term.addSubrule(parseExpression())
      Term.addSubrule(parseSymbol(")"))
      return Term

    }
    //Unary op
    else {
      Term.addSubrule(parseSymbol(currentToken().getValue))
      Term.addSubrule(parseTerm())
      return Term


    }

  }


  private def writeASTTofile(): Unit = {
    val writer = new PrintWriter(new FileOutputStream(targetFileName, true))
    writer.append(AstTree.RuletoXml)
    writer.close()
  }

  class Token(pattern: String = null, value: String = null) {
    def getPattern = pattern

    def getValue = value

    def XMLNodeToToken(xmlNode: String): Token = {
      if (xmlNode.equals("<tokens>") || xmlNode.equals("</tokens>"))
        return null
      else {
        val spilled = xmlNode.split("[<_>]")
        return new Token(spilled(1), spilled(2).trim)
      }

    }

    override def toString: String = "<" + pattern + "> " + value + " </" + pattern + ">" + "\n"


  }


}
