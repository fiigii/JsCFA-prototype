import java.io.{FileReader, File}
import AssignOp.AssignOp
import InfixOp.InfixOp
import PrefixOp.PrefixOp

import scala.collection.JavaConverters._

import sun.org.mozilla.javascript.internal.ast._
import sun.org.mozilla.javascript.internal.{Token, Parser, CompilerEnvirons}

import scala.collection.mutable.ListBuffer

/**
  * Created by Fei Peng on 2/15/16.
  */
object GenerateAST {
  def apply(jsFile : File) = generateAST(jsFile)
  def generateAST(jsFile : File) : Statement = convert_statement(parseJavaScript(jsFile))

  private def convert_statement(node : AstNode) : Statement = {
    var ret : Statement = null
    node.getType match {
      case Token.SCRIPT => {
        val theNode = node.asInstanceOf[AstRoot]
        val stmts = ListBuffer.empty[Statement]
        for (stmt <- theNode.asScala) {
          stmts += convert_statement(stmt.asInstanceOf[AstNode])
        }
        ret = Script(stmts.toList)
      }
      case Token.EMPTY => {
        ret = new EmptyStmt()
      }
      case Token.VAR => {
        node match {
          case theNode : VariableInitializer => {
            val init = theNode.getInitializer
            val name = IntroduceVar(theNode.getTarget.asInstanceOf[Name].getIdentifier)
            if (init == null) {
              ret = VarDeclStmt(name, EmptyExpr())
            } else {
              ret = VarDeclStmt(name, convert_expression(init))
            }
          }
          case theNode : VariableDeclaration => {
            val variables = theNode.getVariables.asScala
            if (variables.size == 1) {
              ret = convert_statement(variables(0))
            } else {
              val stmts = ListBuffer.empty[Statement]
              for (stmt <- variables) {
                stmts += convert_statement(stmt)
              }
              ret = VarDeclListStmt(stmts.toList)
            }
          }
        }
      }
      case Token.FUNCTION => {
        val theNode = node.asInstanceOf[FunctionNode]
        ret = FunctionDecl(IntroduceVar(theNode.getFunctionName.getIdentifier),
          convert_expression(theNode))
      }
      case Token.RETURN => {
        val theNode = node.asInstanceOf[ReturnStatement]
        var expr : Expression = null
        if (theNode.getReturnValue == null) {
          expr = EmptyExpr()
        } else {
          expr = convert_expression(theNode.getReturnValue)
        }
        ret = ReturnStmt(expr)
      }
      case Token.IF => {
        val theNode = node.asInstanceOf[IfStatement]
        var elsePart : Statement = null
        if (theNode.getElsePart == null) {
          elsePart = EmptyStmt()
        } else {
          elsePart = convert_statement(theNode.getElsePart)
        }
        ret = IfStmt(convert_expression(theNode.getCondition),
                    convert_statement(theNode.getThenPart),
                    elsePart)
      }
      case Token.SWITCH => {
        val theNode = node.asInstanceOf[SwitchStatement]
        val caseList = ListBuffer.empty[CaseStmt]
        var defaultCase : Option[CaseStmt] = None
        for (c <- theNode.getCases.asScala) {
          var caseExpr : Expression = null
          if (c.getExpression == null) {
            caseExpr = EmptyExpr()
          } else {
            caseExpr = convert_expression(c.getExpression)
          }

          var body : Statement = null
          if (c.getStatements == null) {
            body = EmptyStmt()
          } else {
            val listBody = ListBuffer.empty[Statement]
            for (theC <- c.getStatements.asScala) {
              listBody += convert_statement(theC)
            }
            body = BlockStmt(listBody.toList)
          }

          if (!c.isDefault) {
            caseList += CaseStmt(caseExpr, body)
          } else {
            defaultCase = Some(CaseStmt(caseExpr, body))
          }
        }
       ret = SwitchStmt(convert_expression(theNode.getExpression),
                        caseList.toList,
                          defaultCase)
      }
      case Token.BREAK => {
        val theNode = node.asInstanceOf[BreakStatement]
        var label = ""
        if (theNode.getBreakLabel != null) {
          label = theNode.getBreakLabel.getIdentifier
        }
        ret = BreakStmt(label)
      }
      case Token.CONTINUE => {
        val theNode = node.asInstanceOf[ContinueStatement]
        var label = ""
        if (theNode.getLabel != null) {
          label = theNode.getLabel.getIdentifier
        }
        ret = ContinueStmt(label)
      }
      case Token.DO => {
        val theNode = node.asInstanceOf[DoLoop]
        ret = DoWhileStmt(convert_expression(theNode.getCondition),
                          convert_statement(theNode.getBody))
      }
      case Token.WHILE => {
        val theNode = node.asInstanceOf[WhileLoop]
        ret = WhileStmt(convert_expression(theNode.getCondition),
                        convert_statement(theNode.getBody))
      }
      case Token.FOR => {
        node match {
          case theNode : ForLoop => {
            val rhinoInit = theNode.getInitializer
            var init : ForInit = null
            if (rhinoInit.isInstanceOf[VariableDeclaration]) {
              val varDecl = convert_statement(rhinoInit)
              varDecl match {
                case decl : VarDeclListStmt => init = VarListInit(decl)
                case decl : VarDeclStmt => init = VarInit(decl)
                case _ => throw new RuntimeException("Only VarDecl statement can be in init of for loop")
              }
            } else if (rhinoInit != null){
              init = ExprInit(convert_expression(rhinoInit))
            } else {
              init = NoneInit()
            }
            val cond = if (theNode.getCondition == null) None else Some(convert_expression(theNode.getCondition))
            val inc = if (theNode.getIncrement == null) None else Some(convert_expression(theNode.getIncrement))
            ret = ForStmt(init,
                          cond,
                          inc,
                          convert_statement(theNode.getBody))
          }
          case theNode : ForInLoop => {
            var init : ForInInit = null
            val iterator = theNode.getIterator
            iterator match {
              case it : Name => init = ForInLValue(LVarRef(it.getString))
              case it : VariableDeclaration =>
                init = ForInVarDecl(IntroduceVar(it.getVariables.get(0).getTarget.getString))
            }
            ret = ForInStmt(init,
                            convert_expression(theNode.getIteratedObject),
                            convert_statement(theNode.getBody))
          }
        }
      }
      case Token.BLOCK => {
        ret = node match {
          case theNode : Block => {
            val stmts = ListBuffer.empty[Statement]
            val i = theNode.iterator()
            while(i.hasNext) {
              val s = i.next()
              stmts += convert_statement(s.asInstanceOf[AstNode])
            }
            BlockStmt(stmts.toList)
          }
          case theNode : Scope => {
            val stmts = ListBuffer.empty[Statement]
            val i = theNode.iterator()
            while (i.hasNext) {
              val s = i.next()
              stmts += convert_statement(s.asInstanceOf[AstNode])
            }
            BlockStmt(stmts.toList)
          }
        }
      }
      case Token.EXPR_VOID => {
        ret = node match {
          case theNode : LabeledStatement => {
            val theNode = node.asInstanceOf[LabeledStatement]
            val labels = ListBuffer.empty[String]
            for (l <- theNode.getLabels.asScala) {
              labels += l.getName
            }
            LabeledStmt(labels.toList, convert_statement(theNode.getStatement))
          }
          case theNode : ExpressionStatement => ExprStmt(convert_expression(theNode.getExpression))
          case other => throw new RuntimeException("EXPR_VOID : " + other.getClass)
        }
      }
      case Token.ASSIGN | Token.EXPR_RESULT => {
        ret = ExprStmt(convert_expression(node))
      }
      case Token.TRY => {
        val theNode = node.asInstanceOf[TryStatement]
        val finalCatch = if (theNode.getFinallyBlock != null) Some(convert_statement(theNode.getFinallyBlock))
                         else None
        val catchs = ListBuffer.empty[CatchStmt]
        for (cl : CatchClause <- theNode.getCatchClauses.asScala) {
          catchs += CatchStmt(IntroduceVar(cl.getVarName.getIdentifier),
            convert_statement(cl.getBody))
        }
        ret = TryStmt(convert_statement(theNode.getTryBlock),
                      catchs.toList,
                      finalCatch)
      }
      case Token.THROW => {
        val theNode = node.asInstanceOf[ThrowStatement]
        ret = ThrowStmt(convert_expression(theNode.getExpression))
      }
      case _ => throw new RuntimeException("Unknown Statement Type: " + Token.typeToName(node.getType) + "\n")
    }
    ret
  }

  private def convert_expression(node : AstNode) : Expression = {
    var ret : Expression = null
    node.getType match {
      case Token.EXPR_VOID | Token.EXPR_RESULT =>
        ret = convert_expression(node.asInstanceOf[ExpressionStatement].getExpression)
      case Token.EMPTY => ret = EmptyExpr()
      case Token.LP => ret = convert_expression(node.asInstanceOf[ParenthesizedExpression].getExpression)
      case Token.FUNCTION => {
        val theNode = node.asInstanceOf[FunctionNode]
        val funcName = if (theNode.getFunctionName != null) Some(IntroduceVar(theNode.getFunctionName.getIdentifier))
                       else None
        val ps = ListBuffer.empty[IntroduceVar]
        for (p : AstNode <- theNode.getParams.asScala) {
          ps += IntroduceVar(p.getString)
        }
        val funcExpr = FunctionExpr(funcName, ps.toList, convert_statement(theNode.getBody))
        funcExpr.sourceCode = theNode.toSource
        ret = funcExpr
      }
      case Token.CALL => {
        val theNode = node.asInstanceOf[FunctionCall]
        val args = ListBuffer.empty[Expression]
        for (a <- theNode.getArguments.asScala) {
          args += convert_expression(a)
        }
        val target = convert_expression(theNode.getTarget)
        ret = target match {
          case DotRef(obj, prop) => MethodCall(obj, StringLit(prop), args.toList)
          case BracketRef(obj, prop) => MethodCall(obj, prop, args.toList)
          case expr => FuncCall(expr, args.toList)
        }
      }
      case Token.NEW => {
        val theNode = node.asInstanceOf[NewExpression]
        val args = ListBuffer.empty[Expression]
        for (a <- theNode.getArguments.asScala) {
          args += convert_expression(a)
        }
        ret = NewCall(convert_expression(theNode.getTarget), args.toList)
      }
      case Token.ASSIGN | Token.ASSIGN_DIV | Token.ASSIGN_SUB | Token.ASSIGN_MOD |
           Token.ASSIGN_ADD | Token.ASSIGN_MUL | Token.ASSIGN_BITOR | Token.ASSIGN_BITXOR |
           Token.ASSIGN_BITAND | Token.ASSIGN_LSH | Token.ASSIGN_RSH | Token.ASSIGN_URSH => {
        val theNode = node.asInstanceOf[Assignment]
        val lv = convert_lvalue(theNode.getLeft)
        val rv = convert_expression(theNode.getRight)
        val op = convert_assign_op(node.getType)
        ret = AssignExpr(op, lv, rv)
      }
      case Token.NAME => {
        val theNode = node.asInstanceOf[Name]
        ret = VarRef(theNode.getIdentifier)
      }
      case Token.THIS => ret = ThisRef()
      case Token.GETPROP => {
        val theNode = node.asInstanceOf[PropertyGet]
        ret = DotRef(convert_expression(theNode.getTarget), theNode.getProperty.getIdentifier)
      }
      case Token.GETELEM => {
        val theNode = node.asInstanceOf[ElementGet]
        val target = convert_expression(theNode.getTarget)
        val prop = convert_expression(theNode.getElement)
        ret = BracketRef(target, prop)
      }
      case Token.NULL => ret = NullLit()
      case Token.TRUE => ret = BoolLit(true)
      case Token.FALSE => ret = BoolLit(false)
      case Token.NUMBER => {
        val theNode = node.asInstanceOf[NumberLiteral]
        ret = NumberLit(theNode.getNumber)
      }
      case Token.STRING => {
        val theNode = node.asInstanceOf[StringLiteral]
        ret = StringLit(theNode.getValue)
      }
      case Token.REGEXP => {
        val theNode = node.asInstanceOf[RegExpLiteral]
        val (global, case_in) = parseRegExpFlags(theNode.getFlags)
        ret = RegExp(theNode.getValue, global, case_in)
      }
      case Token.OBJECTLIT => {
        val theNode = node.asInstanceOf[ObjectLiteral]
        val properties = ListBuffer.empty[ObjectPair]
        for (p : ObjectProperty <- theNode.getElements.asScala) {
          properties +=  ObjectPair(convert_prop(p.getLeft), convert_expression(p.getRight))
        }
        ret = ObjectLit(properties.toList)
      }
      case Token.ARRAYLIT => {
        val theNode = node.asInstanceOf[ArrayLiteral]
        val arr = ListBuffer.empty[Expression]
        for (el <- theNode.getElements.asScala) {
          arr += convert_expression(el)
        }
        ret = ArrayLit(arr.toList)
      }
      case Token.HOOK => {
        val theNode = node.asInstanceOf[ConditionalExpression]
        ret = CondExpr(convert_expression(theNode.getTestExpression),
                       convert_expression(theNode.getTrueExpression),
                       convert_expression(theNode.getFalseExpression))
      }
      case Token.INC  => {
        val theNode = node.asInstanceOf[UnaryExpression]
        val operand = convert_lvalue(theNode.getOperand)
        if(theNode.isPrefix) {
          ret = UnaryAssignExpr(UnaryAssignOp.PrefixInc, operand)
        } else {
          ret = UnaryAssignExpr(UnaryAssignOp.PostfixInc, operand)
        }
      }
      case Token.DEC => {
        val theNode = node.asInstanceOf[UnaryExpression]
        val operand = convert_lvalue(theNode.getOperand)
        if(theNode.isPrefix) {
          ret = UnaryAssignExpr(UnaryAssignOp.PostfixDec, operand)
        } else {
          ret = UnaryAssignExpr(UnaryAssignOp.PostfixDec, operand)
        }
      }
      case Token.TYPEOF | Token.POS | Token.NEG | Token.BITNOT |
           Token.NOT | Token.VOID | Token.DEL_REF | Token.DELPROP => {
        val theNode = node.asInstanceOf[UnaryExpression]
        val operand = convert_expression(theNode.getOperand)
        ret = PrefixExpr(convert_unary_op(theNode.getOperator), operand)
      }
      case Token.LT | Token.LE | Token.GT | Token.GE | Token.IN |
           Token.INSTANCEOF | Token.EQ | Token.NE | Token.SHEQ |
           Token.SHNE | Token.AND | Token.OR | Token.MUL | Token.DIV |
           Token.ADD | Token.SUB | Token.MOD | Token.LSH | Token.RSH |
           Token.URSH | Token.BITAND | Token.BITOR | Token.BITXOR => {
        val theNode = node.asInstanceOf[InfixExpression]
        val expr1 = convert_expression(theNode.getLeft)
        val expr2 = convert_expression(theNode.getRight)
        ret = InfixExpr(convert_infix_op(theNode.getOperator), expr1, expr2)
      }
      case Token.COMMA => {
        var theNode = node.asInstanceOf[InfixExpression]
        val exprs = ListBuffer.empty[Expression]
        while (theNode.getLeft.getType == Token.COMMA) {
          exprs += convert_expression(theNode.getRight)
          theNode = theNode.getLeft.asInstanceOf[InfixExpression]
        }
        exprs += convert_expression(theNode.getRight)
        exprs += convert_expression(theNode.getLeft)
        ret = ListExpr(exprs.toList.reverse)
      }
      case _ => throw new RuntimeException("Unknown Expression Type: " + Token.typeToName(node.getType) + "\n")
    }
    ret
  }

  private def convert_infix_op(ttype : Int) : InfixOp = ttype match {
    case Token.LT => InfixOp.OpLT
    case Token.LE => InfixOp.OpLEq
    case Token.GT => InfixOp.OpGT
    case Token.GE => InfixOp.OpGEq
    case Token.IN => InfixOp.OpIn
    case Token.INSTANCEOF => InfixOp.OpInstanceof
    case Token.EQ => InfixOp.OpEq
    case Token.NE => InfixOp.OpNEq
    case Token.SHEQ => InfixOp.OpStrictEq
    case Token.SHNE => InfixOp.OpStrictNEq
    case Token.AND => InfixOp.OpLAnd
    case Token.OR => InfixOp.OpLOr
    case Token.MUL => InfixOp.OpMul
    case Token.DIV => InfixOp.OpDiv
    case Token.ADD => InfixOp.OpAdd
    case Token.SUB => InfixOp.OpSub
    case Token.MOD => InfixOp.OpMod
    case Token.LSH => InfixOp.OpLShift
    case Token.RSH => InfixOp.OpSpRShift
    case Token.URSH => InfixOp.OpZfRShift
    case Token.BITAND => InfixOp.OpBAnd
    case Token.BITOR => InfixOp.OpBOr
    case Token.BITXOR => InfixOp.OpBXor
    case _ => throw new RuntimeException("Unknown Infix Operator: " + Token.typeToName(ttype) + "\n")
  }

  private def convert_lvalue(node : AstNode) : LValue = {
    node.getType match {
      case Token.GETPROP => LDot(convert_expression(node.asInstanceOf[PropertyGet].getTarget),
                                 node.asInstanceOf[PropertyGet].getProperty.getIdentifier)
      case Token.GETELEM => LBracket(convert_expression(node.asInstanceOf[ElementGet].getTarget),
                                     convert_expression(node.asInstanceOf[ElementGet].getElement))
      case Token.NAME => LVarRef(node.asInstanceOf[Name].getIdentifier)
      case _ => throw new RuntimeException("Unknown LValue: " + Token.typeToName(node.getType) + "\n")
    }
  }

  private def convert_prop(node : AstNode) : Property = node match {
    case theNode : Name => PropVar(theNode.getIdentifier)
    case theNode : StringLiteral => PropString(theNode.getValue)
    case theNode : NumberLiteral => PropNum(theNode.getNumber)
    case _ => throw new RuntimeException("Property Name cannot be " + Token.typeToName(node.getType) + "\n")
  }

  private def convert_assign_op(ttype : Int) : AssignOp = ttype match {
    case Token.ASSIGN => AssignOp.OpAssign
    case Token.ASSIGN_DIV => AssignOp.OpAssignDiv
    case Token.ASSIGN_SUB => AssignOp.OpAssignSub
    case Token.ASSIGN_MOD => AssignOp.OpAssignMod
    case Token.ASSIGN_ADD => AssignOp.OpAssignAdd
    case Token.ASSIGN_MUL => AssignOp.OpAssignMul
    case Token.ASSIGN_BITOR => AssignOp.OpAssignBOr
    case Token.ASSIGN_BITXOR => AssignOp.OpAssignBXor
    case Token.ASSIGN_BITAND => AssignOp.OpAssignBAnd
    case Token.ASSIGN_LSH => AssignOp.OpAssignLShift
    case Token.ASSIGN_RSH => AssignOp.OpAssignSpRShift
    case Token.ASSIGN_URSH => AssignOp.OpAssignZfRShift
    case _ => throw new RuntimeException("Unknown Assign Operator: " + Token.typeToName(ttype) + "\n")
  }

  private def convert_unary_op(ttype : Int) : PrefixOp = ttype match {
    case Token.TYPEOF => PrefixOp.PrefixTypeof
    case Token.POS => PrefixOp.PrefixPlus
    case Token.NEG => PrefixOp.PrefixMinus
    case Token.BITNOT => PrefixOp.PrefixBNot
    case Token.NOT => PrefixOp.PrefixLNot
    case Token.VOID => PrefixOp.PrefixVoid
    case Token.DEL_REF | Token.DELPROP => PrefixOp.PrefixDelete
    case _ => throw new RuntimeException("Unknown Prefix Operator: " + Token.typeToName(ttype) + "\n")
  }

  private def parseJavaScript(file: File) : AstNode = {
    val reader = new FileReader(file)
    val compilerEnv = new CompilerEnvirons
    val errorReporter = compilerEnv.getErrorReporter
    val parser = new Parser(compilerEnv, errorReporter)
    val sourceURI = file.getCanonicalPath
    val ast = parser.parse(reader, sourceURI, 1)
    ast.getAstRoot
  }

  private def parseRegExpFlags(flags : String) : (Boolean, Boolean) = {
    (true, true)
  }

  def main(args: Array[String]) {
    val ast = generateAST(new File("test.js"))
    val decedAST = DecorateAST(ast)
    NameResolver(decedAST)
    val disk = AAM.analyze(decedAST)

    println("AST :")
    DecorateAST.mapToAST.foreach{
      case (id, t) => println(id + " :: " + t)
    }

    println("\n\nDisk")
    disk.foreach{
      case (address, set)  =>
        println("\n" + address + " -> ")
        set.foreach {
          case o => println("    " + o)
        }
    }

    println("\n\nResult : ")
    DecorateAST.mapToAST.foreach {
      case (id, v) => if (id > 0 && v.isInstanceOf[IntroduceVar] && disk.contains(JSReference(id))) {
        val values = disk(JSReference(id))
        println(v.asInstanceOf[IntroduceVar].str + " -> ")
        values.foreach[Unit](x => println("    " + x))
      }// else if(!store.contains(JSReference(id))) {
        //println("\n\nNo Value for: " + v + "\n")
      //}
    }

  }
}
