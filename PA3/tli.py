#! /usr/bin/env python3
import fileinput
import sys

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return "(" + self.operator + " " + self.op1 + ")"
        else:
            return "(" + str(self.op1) + " " + self.operator + " " + str(self.op2) + ")"

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        var1 = self.op1
        var2 = self.op2

        if isinstance(var1, Expr):
            var1 = var1.eval(symTable)
        if isinstance(var2, Expr):
            var2 = var2.eval(symTable)

        if var1 in symTable:
            var1 = symTable[var1]
        if var2 in symTable:
            var2 = symTable[var2]

        evaluatedExpr = 0
        if self.operator == "Const":
            evaluatedExpr = float(var1)
        elif self.operator == "Var":
            evaluatedExpr = float(var1)
        elif self.operator == "Plus":
            evaluatedExpr = float(var1) + float(var2)
        elif self.operator == "Minus":
            evaluatedExpr = float(var1) - float(var2)
        elif self.operator == "Multiply":
            evaluatedExpr = float(var1) * float(var2)
        elif self.operator == "Divide":
            evaluatedExpr = float(var1) / float(var2)
        elif self.operator == "LessThan":
            evaluatedExpr = int(float(var1) < float(var2))
        elif self.operator == "GreaterThan":
            evaluatedExpr = int(float(var1) > float(var2))
        elif self.operator == "LessThanEqualTo":
            evaluatedExpr = int(float(var1) <= float(var2))
        elif self.operator == "GreaterThanEqualTo":
            evaluatedExpr = int(float(var1) >= float(var2))
        elif self.operator == "NotEqual":
            evaluatedExpr = int(float(var1) != float(var2))
        elif self.operator == "IsEqual":
            evaluatedExpr = int(float(var1) == float(var2))
        elif self.operator == "String":
            evaluatedExpr = var1
        else:
            return 0
        return evaluatedExpr

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs,label=None):
        self.keyword = keyword
        self.exprs = exprs
        self.label = label

    def __str__(self):
        if(isinstance(self.exprs, Expr)):
            return self.keyword + " " + str(self.exprs)
        else:
            others = ""
            for exp in self.exprs:
                others = others + " " + str(exp)
            return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable, lineNum):
        if self.keyword == "Print":
            for expr in self.exprs:
                output = expr.eval(symTable)
                print(output)
            lineNum += 1
        if self.keyword == "Let":
            e = self.exprs
            evaluatedLeft = e.op1.op1
            evaluatedRight = e.op2.eval(symTable)
            symTable[evaluatedLeft] = evaluatedRight
            lineNum += 1
        if self.keyword == "If":
            res = self.exprs.eval(symTable)
            if(res == 1):
                lineNum = symTable[self.label]
            else:
                lineNum += 1
        if self.keyword == "Input":
            x = input()
            symTable[self.exprs.op1] = x
            lineNum += 1
        return lineNum

def parseExpr(exprLine):
    if len(exprLine) == 1:
        varOrConst = exprLine[0]
        if varOrConst.isalpha():
            return Expr(exprLine[0], "Var")
        elif varOrConst.isnumeric():
            return Expr(exprLine[0], "Const")
        elif isinstance(varOrConst, str):
            return Expr(exprLine[0], "String")
        
    elif(exprLine[1] == "+"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "Plus", e2)
    elif(exprLine[1] == "-"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "Minus", e2)
    elif(exprLine[1] == "*"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "Multiply", e2)
    elif(exprLine[1] == "/"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "Divide", e2)
    elif(exprLine[1] == "="):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr(exprLine[2:])
        return Expr(e1, "Equals", e2)
    elif(exprLine[1] == "<"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "LessThan", e2)
    elif(exprLine[1] == ">"):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "GreaterThan", e2)
    elif(exprLine[1] == "<="):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "LessThanEqualTo", e2)
    elif(exprLine[1] == ">="):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "GreaterThanEqualTo", e2)
    elif(exprLine[1] == "!="):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "NotEqual", e2)
    elif(exprLine[1] == "=="):
        e1 = parseExpr([exprLine[0]])
        e2 = parseExpr([exprLine[2]])
        return Expr(e1, "IsEqual", e2)

def parseStmt(keyword, exprLine):
    if(keyword == "let"):
        return Stmt("Let", parseExpr(exprLine[0]))
    if(keyword == "print"):
        parsedExprs = []
        for expr in exprLine:
            parsedExprs.append(parseExpr(expr))
        return Stmt("Print", parsedExprs)
    if(keyword == "if"):
        currentExprLine = exprLine[0]
        labelToGoTo = currentExprLine.pop(-1)
        currentExprLine.pop(-1)
        currentExpr = currentExprLine
        return Stmt("If", parseExpr(currentExpr), labelToGoTo)
    if(keyword == "input"):
        return Stmt("Input", parseExpr(exprLine[0]))

def main():
    sList = []
    symTable = {}
    inputFile = open(sys.argv[1])
    counter = 0
    for line in inputFile:
        line = line.strip()
        exprList = line.split(' , ')
        tokens = []
        for e in exprList:
            token = e.split(' ')
            tokens.append(token)
        mainKeyword = tokens[0]
        label = ""
        if ':' in mainKeyword[0]:
            label = mainKeyword.pop(0)
            label = label[:-1]
        keyword = mainKeyword[0]
        mainKeyword.pop(0)
        exprLine = tokens
        #lastExpr = exprLine[-1]
        stmt = parseStmt(keyword, exprLine)
        sList.append(stmt)
        if label != "":
            symTable[label] = counter
        counter += 1

    i = 0
    pc = 0
    while i < len(sList):
        pc = sList[i].perform(symTable, pc)
        i = pc

main()


