/*
* @author Tyler Hoang
* CSE112
* PA4 - TLI in Scala
*/

import scala.collection.mutable.Map
import scala.io.Source

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class Word(word: String) extends Expr // in case we want to print a String
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt

object TLI {
    def parseExpr(exprLine: List[String], lineNumber:Double): Expr = {
      if(exprLine.length == 1) { // this will either be a number or a variable or a String
        var varOrConst: String = exprLine(0)
        if(varOrConst(0).isDigit) {
          return Constant(varOrConst.toDouble)
        }
        else if(varOrConst(0) == '"') {
            return Word(varOrConst)
        }
        else {
          return Var(varOrConst)
        }
      }
 
      if(exprLine(1) == "+") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber) // recursively calls parseExpr to parse both sides of the expression
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Plus",e1,e2)
      }
      else if(exprLine(1) == "-") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Minus",e1,e2)
      }
      else if(exprLine(1) == "*") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Multiply",e1,e2)
      }
      else if(exprLine(1) == "/") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Divide",e1,e2)
      }
      else if(exprLine(1) == ">") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("GreaterThan",e1,e2)
      }
      else if(exprLine(1) == "<") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("LessThan",e1,e2)
      }
      else if(exprLine(1) == ">=") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("GreaterThanEqaulTo",e1,e2)
      }
      else if(exprLine(1) == "<=") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("LessThanEqualTo",e1,e2)
      }
      else if(exprLine(1) == "==") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Equals",e1,e2)
      }
      else if(exprLine(1) == "!=") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("NotEqual",e1,e2)
      }
      else if(exprLine(1) == "=") {
        var e1Temp: List[String] = List(exprLine(0))
        var e2Temp: List[String] = List(exprLine(2))
        var e1: Expr = parseExpr(e1Temp, lineNumber)
        var e2: Expr = parseExpr(e2Temp, lineNumber)
        return BinOp("Equals",e1,e2)
      }
      else {
        println("Syntax error on line " + (lineNumber.toInt+1) + ".") // an illegal operation was found
        System.exit(1)
        return Str("Nothing")
      }
    }
  
    def eval(expr: Expr, symTab: Map[String, Double], lineNumber:Double): Double = expr match {
        case BinOp("Plus",e1,e2) => eval(e1,symTab, lineNumber) + eval(e2,symTab, lineNumber)
        case BinOp("Minus",e1,e2) => eval(e1,symTab, lineNumber) - eval(e2,symTab, lineNumber)
        case BinOp("Multiply",e1,e2) => eval(e1,symTab, lineNumber) * eval(e2,symTab, lineNumber)
        case BinOp("Divide",e1,e2) => eval(e1,symTab, lineNumber) / eval(e2,symTab, lineNumber)
        case BinOp("GreaterThan",e1,e2) => if (eval(e1,symTab, lineNumber) > eval(e2,symTab, lineNumber)) 1 else 0
        case BinOp("LessThan",e1,e2) => if (eval(e1,symTab, lineNumber) < eval(e2,symTab, lineNumber)) 1 else 0
        case BinOp("GreaterThanEqualTo",e1,e2) => if (eval(e1,symTab, lineNumber) >= eval(e2,symTab, lineNumber)) 1 else 0
        case BinOp("LessThanEqualTo",e1,e2) => if (eval(e1,symTab, lineNumber).<=(eval(e2,symTab, lineNumber))) 1 else 0
        case BinOp("Equals",e1,e2) => if (eval(e1,symTab, lineNumber) == eval(e2,symTab, lineNumber)) 1 else 0
        case BinOp("NotEquals",e1,e2) => if (eval(e1,symTab, lineNumber) != eval(e2,symTab, lineNumber)) 1 else 0
        case Var(name) => {
          try {
            symTab(name)
          } catch {
            case e: NoSuchElementException =>
              println("Undefined variable " + name + " at line " + (lineNumber.toInt+1) + ".") // an undefined variable was found
              System.exit(1)
              return 0
          }
        }
        case Constant(num) => num
    }
    
    def parseStmt(keyword: String, exprs: List[List[String]], lineNumber:Double): Stmt = {
      if(keyword == "if") {
        var currentIfStatement:List[String] = exprs(0)
        var expr: Expr = parseExpr(currentIfStatement.dropRight(2), lineNumber) // extract the expression
        var label:String = currentIfStatement.last // extract the label
        return If(expr, label)
      }
      else if(keyword == "let") {
        var expressions = exprs(0).drop(2)
        var variable = exprs(0).head
        return Let(variable, parseExpr(expressions, lineNumber))
      }
      else if(keyword == "print") {
        var parsedExprs = List[Expr]()
        for(expr <- exprs) {
          var parsedExpr:Expr = parseExpr(expr, lineNumber)
          parsedExprs = parsedExpr::parsedExprs
        }
        parsedExprs = parsedExprs.reverse // since we were only able to prepend
        return Print(parsedExprs)
      }
      else if(keyword == "input") {
        var e1: List[String] = exprs(0)
        var e2: String = e1(0)
        return Input(e2)
      }
      else {
        println("Syntax error on line " + (lineNumber.toInt+1) + ".") // an illegal keyword was found
        System.exit(1)
        return Input("Nothing")
      }
    }
    
    def perform(stmt: Stmt, symTable:Map[String, Double], progCounter: Int):Int = {
      stmt match {
        case Let(variable:String, expr: Expr) => {
          var evalExpr = eval(expr, symTable, progCounter)
          symTable.put(variable, evalExpr)
          var newProgCounter = progCounter + 1
          return newProgCounter
        }
        case Print(exprList: List[Expr]) => {
          var stringOutput:String = ""
          for(expr <- exprList) {
            expr match {
              case Word(word:String) => {
                var noQuotes:String = word.replace('"', ' ') // remove quotes from Strings, no eval necessary
                noQuotes = noQuotes.trim()
                stringOutput = stringOutput + noQuotes + " "
              }
              case _ => {
                var output:Double = eval(expr, symTable, progCounter)
                stringOutput = stringOutput + output + " "
              }
            }
          }
          println(stringOutput)
          var newProgCounter = progCounter + 1
          return newProgCounter
        }
        case If(expr: Expr, label: String) => {
          var newProgCounter:Int = 0
          var res:Double = eval(expr, symTable, progCounter)
          if(res == 1.0) {
            try {
              newProgCounter = symTable(label).toInt
            }
            catch {
              case e: NoSuchElementException => {
                println("Illegal goto " + label + " at line " + (progCounter+1) + ".") // undefined label found
                System.exit(1)
              }
            }
          }
          else {
            newProgCounter = progCounter + 1
          }
          return newProgCounter
        }
        case Input(variable: String) => {
          var input:Double = 0
          try {
            input = scala.io.StdIn.readDouble()
          } catch {
            case e: Exception => {
              println("Illegal or missing input") // illegal user input
              System.exit(1)
            }
          }
          symTable.put(variable, input)
          var newProgCounter = progCounter + 1
          return newProgCounter
        }
      }
    }

    def main(args: Array[String]) = {
      var sList = List[Stmt]()
      val symTable = Map[String, Double]()
      val filename = args(0)
      var lineCounter: Double = 0
      
      for (line <- Source.fromFile(filename).getLines) {
        if(line.trim().length > 0) { // skip empty lines
          var trimmedLine: String = line.trim() // remove leading and trailing whitespace
          var commaSplitArray: Array[String] = trimmedLine.split(",") // if this is a print stmt, there may be commas
          var exprList = List[List[String]]()
          for(possiblePrintExprs <- commaSplitArray) {
            var currentSpaceSplitExpr:Array[String] = possiblePrintExprs.trim().split("\\s+(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1) // split by whitespace NOT including whitespace in quotes
            var currentTokenList = List[String]()
            for(token <- currentSpaceSplitExpr) { // convert this Array[String] to a List[String] so that it is mutable
              currentTokenList = token::currentTokenList
            }
            currentTokenList = currentTokenList.reverse // since we can only really prepend in scala - we must reverse this list
            exprList = currentTokenList::exprList // add the tokenized expression to the expression list
          }
          exprList = exprList.reverse // we must reverse since elements were prepended instead of appended
          
          var firstStringListContainingKeyword:List[String] = exprList(0) // the first tokenized string in exprList contains the keyword
          var label:String = ""
          if(firstStringListContainingKeyword(0).contains(":")) { // if the first string is a label
            label = firstStringListContainingKeyword.head // grab the label
            firstStringListContainingKeyword = firstStringListContainingKeyword.drop(1) // remove the label
            label = label.dropRight(1) // remove the colon
          }
          var keyWord = firstStringListContainingKeyword.head // grab the keyword
          firstStringListContainingKeyword = firstStringListContainingKeyword.drop(1) // remove the keyword
          exprList = exprList.drop(1) // remove the original first string that has both the keyword and the first expression
          exprList = firstStringListContainingKeyword::exprList // prepend the edited version that only has the first expression
          
          // exprList has now been successfully set up //
          
          var stmt:Stmt = parseStmt(keyWord, exprList, lineCounter) // parse the line to create a stmt
          sList = stmt::sList // add the parsed stmt into sList
          if(label != "") {
            symTable.put(label, lineCounter) // if there was a label, add it to the symTable
          }
          lineCounter = lineCounter + 1 // update the lineCounter for error reporting
        }
      }
      
      var progCounter: Int = 0 // will be used when if statements are encountered
      sList = sList.reverse // everything was prepended
      while(progCounter < sList.length) {
        var currentStmt:Stmt = sList(progCounter)
        progCounter = perform(currentStmt, symTable, progCounter)
      } 
    }
}