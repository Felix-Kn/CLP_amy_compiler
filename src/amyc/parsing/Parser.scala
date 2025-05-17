package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._



// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  val nullE = UnitLiteral()
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 => 
      if id == id1 then 
        ModuleDef(id, defs.toList, body).setPos(obj)
      else 
        throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  }
    

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind){
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
     abstractClassDefinition | caseClassDefinition | functionDefinition 
  
 
  lazy val functionDefinition: Syntax[ClassOrFunDef] = 
    (kw("def") ~ identifier ~ "(" ~ parameters ~ ")" ~ ":" ~ typeTree ~ "=" ~ "{" ~ opt(expr) ~ "}").map{
      case kw ~ identifier ~  _ ~ parameters ~ _ ~ _ ~ ret_type ~ _ ~ _ ~ body ~ _ => 
        FunDef(identifier, parameters, ret_type, body.get).setPos(kw)}

  lazy val abstractClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("abstract") ~ kw("class") ~ identifier).map{ case kw ~ _ ~ identifier =>
      AbstractClassDef(identifier).setPos(kw)}

  lazy val caseClassDefinition: Syntax[ClassOrFunDef] = 
    (kw("case") ~ kw("class") ~ identifier ~ "(" ~ parameters ~ ")" ~ kw("extends") ~ identifier).map{
      case kw ~ _ ~ identifier ~ _ ~ fields ~ _ ~ _ ~ parent => 
        CaseClassDef(identifier, fields.map(_.tt ), parent).setPos(kw)}
    
    
 

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] = (identifier ~ ":" ~ typeTree).map{
    case name ~ _ ~ tipee => ParamDef(name, tipee)
  }
   

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = 
    (simpleTypeTree ~ opt("[" ~ "]")).map{
      case simpleType ~ optArray => 
        optArray match
          case None => simpleType
          case Some(_) => TypeTree(ArrayType(simpleType.tpe))
    }

  lazy val simpleTypeTree: Syntax[TypeTree] = 
    primitiveType | identifierType
    

  
  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  } ~ opt("(" ~ literal ~ ")")).map { 
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) => 
      throw new AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw new AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None => 
      throw new AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) => 
      throw new AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  
  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] = (identifier ~ opt("." ~ identifier)).map{
    case id1 ~ optId  => 
      val name =  optId match
        case None => QualifiedName(None, id1)
        case Some(_ ~ id2) => QualifiedName(Some(id1), id2)
      TypeTree(ClassType(name))
  }
    

    
  def foldMatchs(x : Expr) = (matchList: Seq[Expr]) => 
    matchList.foldLeft(x)((leftExpr, rightMatch) => {
      rightMatch match
        case Match(_, cases) => Match(leftExpr, cases)
    })

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence

  // Stop the expr recursion when we hit a newVal definition
  // newVal will then create a new sequence with old-Context + newVal 
  lazy val expr: Syntax[Expr] = recursive { 
    (((exprNoOp | idStart) ~ opt(";" ~ expr)) || newValOrVar).map{

      case toTest => toTest match
        // case Sequence of not newVal
        case Left(notVal1 ~ optNextExpr) => 
          optNextExpr match
            case None => notVal1
            case Some(_ ~ nextExpr) => Sequence(notVal1, nextExpr)
          
        case Right(isVal1) => isVal1
    }
  }     


  lazy val newValOrVar :  Syntax[Expr] = {
    newVal | newVar
  }

  lazy val newVal: Syntax[Expr] = {
    (kw("val") ~ parameter ~ "=" ~ exprNoValNoReAssign ~ ";" ~ expr).map{
      case _ ~ param ~ _ ~ expr1 ~ _ ~ expr2 => Let(param, expr1, expr2) 
    }
  }

  lazy val newVar: Syntax[Expr] = {
     (kw("var") ~ parameter ~ "=" ~ exprNoValNoReAssign ~ ";" ~ expr).map{
      case _ ~ param ~ _ ~ expr1 ~ _ ~ expr2 => Assign(param, expr1, expr2) 
    }
  }

  lazy val amyWhile: Syntax[Expr] = 
    (kw("while") ~ "(" ~ expr ~")" ~ "{" ~ expr ~ "}").map{
      case _ ~ _ ~ cond ~ _ ~ _ ~ body ~ _  => While(cond, body)
    }
  

  // first expr of a val can not contain a val
  lazy val exprNoValNoReAssign: Syntax[Expr] = recursive{
    (N ~ opt(many1(matchs))).map{
      case firstExpre ~ optMatchs => 
        optMatchs match
        case None => firstExpre
        case Some(manyMatch) => foldMatchs(firstExpre)(manyMatch)           
    }
  }

  lazy val exprNoOp: Syntax[Expr] = recursive{
    (noOp ~ opt(many1(matchs))).map{
      case firstExpre ~ optMatchs => 
        optMatchs match
        case None => firstExpre
        case Some(manyMatch) => foldMatchs(firstExpre)(manyMatch)           
    }
  }
  
  lazy val noOp: Syntax[Expr] = 
    ifThenElse | amyWhile

  lazy val N: Syntax[Expr] = 
    operators |  ifThenElse | amyWhile 

  lazy val matchs: Syntax[Expr] = 
    (kw("match") ~ "{" ~ many1(matchCase) ~ "}").map{ 
      case _ ~ _ ~ caseSeq ~ _ => 
        Match(UnitLiteral(), caseSeq.toList)
    }
  
  lazy val dotId: Syntax[String] =
    ("." ~ identifier).map{
      case _ ~ id => id
    }

  lazy val followCall: Syntax[Seq[Expr]] = 
    ("(" ~ repsep(expr, ",") ~ ")").map{
      case _ ~ exprSeq ~_ => exprSeq
    }

  
  lazy val isCall: Syntax[Expr] = 
      (dotId ~ followCall).map{
        case id ~ exprSeq => Call(QualifiedName(None, id), exprSeq.toList)
      }| followCall.map{
        case exprSeq => Call(QualifiedName(None, ""), exprSeq.toList)
      } 
    
  lazy val variableOrCallOrArray: Syntax[Expr] = 
    (identifier ~ opt(isCall || isArray)).map{
      case id ~ optCallModify => 
        optCallModify match
          case None => Variable(id)
          case Some(moreExpr) =>
            moreExpr match
              case Left(callOrModify) => 
                callOrModify match
                  case Call(QualifiedName(None, tempId), args) => 
                        if(tempId.isEmpty()) Call(QualifiedName(None, id), args) else 
                          Call(QualifiedName(Some(id), tempId), args)
              case Right(arrayCall) =>
                arrayCall match
                  case ArraySize(_) => ArraySize(id)
                  case ArrayGet(_, index) => ArrayGet(id, index)
                       
    } 

  
  
  lazy val unaryExpr: Syntax[Expr] = 
      ((op("!")||op("-")) ~ simpleExpr).map{
        case op ~ nextExpr => 
          op match 
            case Left(_) => Not(nextExpr) 
            case Right(_) => Neg(nextExpr)
      } | simpleExpr

  lazy val singleOp:Syntax[Token ~ Expr] = 
    (oneOf(op("+"),op("-"),op("*"), op("/"),op("%"),op("<"),op("<="), op("=="), op("&&"), op("||"), op("++")) ~ operators)

  lazy val isCallWithOp: Syntax[Expr ~ Option[(Token ~ Expr)]] = 
    isCall ~ opt(singleOp)

  lazy val isNewAssign: Syntax[Expr] = 
    ("=" ~ N ).map{ case _ ~ newExpr => reAssign("", newExpr)}

  lazy val elseSimpleExpr: Syntax[Expr] = 
    (simpleExprNoId ~ opt(singleOp) ~ opt(many1(matchs))).map{
      case lhs ~ optOp ~ optMatchs => 
        (optOp: @unchecked) match
          case None => applyMatch(lhs, optMatchs)
          case Some((OperatorToken(name) ~ rhs)) =>
            applyMatch(rewireOperator(lhs, operatorTranslation(nullE, name, nullE), rhs), optMatchs)
    }
  
  lazy val unaryOp: Syntax[Expr] = 
    ((op("!") || op("-")) ~ simpleExpr ~ opt(singleOp) ~ opt(many1(matchs))).map{
      case lOrR ~ folowExpr ~ optOp ~ optMatchs => 
        val leftVal = 
          lOrR match
            case Left(_) => Not(folowExpr)
            case Right(_) => Neg(folowExpr)
        (optOp: @unchecked) match
          case None => applyMatch(leftVal, optMatchs)
          case Some(OperatorToken(name) ~ folowOp) => applyMatch(rewireOperator(leftVal, operatorTranslation(nullE, name, nullE), folowOp), optMatchs)
    }

  
    
  def rewireOperator(lhs: Expr, op: Expr, rhs: Expr): Expr = {
    val opLevel = getOpLevel(op)
    val rhsLevel = getOpLevel(rhs)
    // base case
    if(opLevel < rhsLevel){
      buildOpFromOp(lhs, op, rhs)
    }else{
      (rhs: @unchecked) match
        case Or(newLeft, right) => Or(rewireOperator(lhs, op, newLeft), right)
        case And(newLeft, right) => And(rewireOperator(lhs, op, newLeft), right)
        case Equals(newLeft, right) => Equals(rewireOperator(lhs, op, newLeft), right)
        case LessEquals(newLeft, right) => LessEquals(rewireOperator(lhs, op, newLeft), right)
        case LessThan(newLeft, right) => LessThan(rewireOperator(lhs, op, newLeft), right)
        case Plus(newLeft, right) => Plus(rewireOperator(lhs, op, newLeft), right)
        case Minus(newLeft, right) => Minus(rewireOperator(lhs, op, newLeft), right)
        case Times(newLeft, right) => Times(rewireOperator(lhs, op, newLeft), right)
        case Concat(newLeft, right) => Concat(rewireOperator(lhs, op, newLeft), right)
        case Mod(newLeft, right) => Mod(rewireOperator(lhs, op, newLeft), right)
        case Div(newLeft, right) => Div(rewireOperator(lhs, op, newLeft), right)
    }
  }

  def buildOpFromOp(newLeft:Expr, op:Expr, right: Expr): Expr = {
    (op: @unchecked) match
      case Or(_,_) => Or(newLeft, right)
      case And(_,_) => And(newLeft, right)
      case Equals(_,_) => Equals(newLeft, right)
      case LessEquals(_,_) => LessEquals(newLeft, right)
      case LessThan(_,_) => LessThan(newLeft, right)
      case Plus(_,_) => Plus(newLeft, right)
      case Minus(_,_) => Minus(newLeft, right)
      case Times(_,_) => Times(newLeft, right)
      case Concat(_,_) => Concat(newLeft, right)
      case Mod(_,_) => Mod(newLeft, right)
      case Div(_,_) => Div(newLeft, right)
      case _ => throw new AmycFatalError(s"Build from op failed match => ${op}")
  }
  

  def getOpLevel(op:Expr): Int = {
    (op: @unchecked) match
      case a@Or(_,_) => 0
      case a@And(_,_) => 1
      case a@Equals(_,_) => 2 
      case LessEquals(_,_) | LessThan(_,_) => 3
      case Plus(_,_) | Minus(_,_) | Concat(_,_) => 4 
      case Mod(_,_) | Times(_,_) | Div(_,_) => 5
      case IntLiteral(_) | StringLiteral(_) | BooleanLiteral(_) | UnitLiteral() | ArrayGet(_,_) | ArraySize(_) => 6
      case Call(_,_) | Variable(_) | Error(_) | Not(_) | Neg(_) | ParenthesizedExpr(_) => 6
      case _ => throw new AmycFatalError(s"get operator level failed match => ${op}")
  } 

  def applyMatch(v:Expr, optMatchs:Option[Seq[Expr]]): Expr = {
    optMatchs match
      case None => v
      case Some(matches) => foldMatchs(v)(matches)
  }

  lazy val isArray: Syntax[Expr] = 
    ("["~ opt(expr) ~"]").map{
      case _ ~ optExpr ~ _ => 
        optExpr match
          case None => ArraySize("")
          case Some(index) => ArrayGet("", index)
    }

  lazy val arraySetOrGetOrSize: Syntax[Either[Option[Token ~ Expr], Expr ~ Option[Either[Token ~ Expr, Token ~ Expr]]]] = 
    ("[" ~ ("]" ~ opt(singleOp) || (expr ~ "]" ~ opt(singleOp || ("=" ~ N) )))).map{
      case _ ~ sizeOrElse => 
        sizeOrElse match
          case Left(_ ~ sizeOptOp) => Left(sizeOptOp) // size with maybe operator 
          
          case Right(index ~ _ ~ getOpOrSet) => 
            getOpOrSet match
              case None => Right(index ~ None)
              case Some(opOrSet) => Right(index ~ Some(opOrSet))
    }

  

  lazy val idStart: Syntax[Expr] = 
    (identifier ~ opt(((singleOp || isCallWithOp) || (arraySetOrGetOrSize || isNewAssign))) ~ opt(many1(matchs))).map{
      case id1 ~ optWtf ~ optMatchs => 
        optWtf match
          case None => applyMatch(Variable(id1), optMatchs)
          case Some(extra) => 
            (extra: @unchecked) match
              case Left(moreValue) => 
                (moreValue: @unchecked) match
                  case Left(OperatorToken(name) ~ rhs) => 
                    applyMatch(rewireOperator(Variable(id1), operatorTranslation(nullE, name, nullE), rhs), optMatchs)

                  case Right(callVals ~ optOp) =>
                    val updatedCall = callVals match
                      case Call(QualifiedName(None, tempId), args) => 
                        if(tempId.isEmpty()) Call(QualifiedName(None, id1), args) else 
                          Call(QualifiedName(Some(id1), tempId), args)
                    (optOp: @unchecked) match
                      case None => applyMatch(updatedCall, optMatchs)
                      case Some((OperatorToken(name) ~ rhs)) =>
                        applyMatch(rewireOperator(updatedCall, operatorTranslation(nullE, name, nullE), rhs), optMatchs) 

              case Right(arrayOrAssign) =>
                  arrayOrAssign match
                    case Left(array) =>
                      array match
                        case Left(sizeOptOp) =>
                          val updatedArray = 
                          (sizeOptOp: @unchecked) match
                            case None => ArraySize(id1)
                            case Some(OperatorToken(name) ~ rhs) => 
                              rewireOperator(ArraySize(id1), operatorTranslation(nullE, name, nullE), rhs)
                          applyMatch(updatedArray, optMatchs)

                        case Right(index ~ optNext) => 
                          optNext match
                            case None => applyMatch(ArrayGet(id1, index), optMatchs)
                            case Some(getOpOrSet) =>
                              (getOpOrSet: @unchecked) match
                                case Left(OperatorToken(name) ~ rhs) => 
                                  applyMatch(rewireOperator(ArrayGet(id1, index), operatorTranslation(nullE, name, nullE), rhs), optMatchs)
                                case Right(_ ~ newValue) => 
                                  ArraySet(id1, index, applyMatch(newValue, optMatchs))
                              

                    case Right(assign) =>
                      assign match {case reAssign(_, newValue) => reAssign(id1, applyMatch(newValue, optMatchs))}

    } | elseSimpleExpr | unaryOp

  
      
  lazy val operators: Syntax[Expr] = 
    operators(unaryExpr)(
      Level(op("*") | op("/") | op("%"),  LeftAssociative),
      Level(op("+") | op("-") | op("++"), LeftAssociative),
      Level(op("<=") | op("<"), LeftAssociative),
      Level(op("=="), LeftAssociative),
      Level(op("&&"), LeftAssociative),
      Level(op("||"), LeftAssociative)
      )((left, op, right) =>
        (op: @unchecked )match
          case OperatorToken(operator) => operatorTranslation(left, operator, right).setPos(op))

  
  val operatorTranslation = ((left:Expr, op:String, right:Expr) => 
    op match
      case "+" => Plus(left, right)
      case "-" => Minus(left, right)
      case "*" => Times(left, right)
      case "/" => Div(left, right)
      case "%" => Mod(left, right) 
      case "<" => LessThan(left, right)
      case "<=" => LessEquals(left, right)
      case "==" => Equals(left, right)
      case "&&" => And(left, right)
      case "||" => Or(left, right)
      case "++" => Concat(left, right)
  )

  
  lazy val ifThenElse: Syntax[Expr] = 
    (kw("if") ~ "(" ~ expr ~ ")" ~ "{" ~ expr ~ "}" ~ kw("else") ~ "{" ~ expr ~ "}").map{
      case _ ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ _ ~ _ ~ elze ~ _ => Ite(cond, thenn, elze)
    }
  
  
    

  // A literal expression.
  lazy val literal: Syntax[Literal[?]] = 
    accept(LiteralKind){
      case tk@BoolLitToken(value) => BooleanLiteral(value).setPos(tk)
      case tk@IntLitToken(value) => IntLiteral(value).setPos(tk)
      case tk@StringLitToken(value) => StringLiteral(value).setPos(tk)
    }

  /// ***************** pattern start ***************************///
  
  lazy val matchCase: Syntax[MatchCase] =
    (kw("case") ~ pattern ~ "=>" ~ expr).map{
      case c ~ pat ~ _ ~ body => MatchCase(pat, body).setPos(c)
    }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive { 
    wildPattern | literalPattern | caseClassOrIdPattern 
  }

  lazy val caseClassOrIdPattern: Syntax[Pattern] = 
    (identifier ~ opt("." ~ identifier ) ~ opt("(" ~ repsep(pattern, ",") ~ ")")).map{ // todo modify to same constraint as call
      case id ~ optNextId ~ optPatternSeq =>  
        optPatternSeq match
          case None => IdPattern(id)
          case Some(_ ~ patternSeq ~ _) => 
            optNextId match 
              case None => CaseClassPattern(QualifiedName(None, id), patternSeq.toList)
              case Some(_ ~ newId) => CaseClassPattern(QualifiedName(Some(id), newId), patternSeq.toList)
    } 
    
  lazy val literalPattern: Syntax[Pattern] = 
    ("(" ~ ")").map{
      case _ ~ _ => LiteralPattern(UnitLiteral())}
    |
    literal.map(LiteralPattern(_))

  lazy val wildPattern: Syntax[Pattern] = 
    kw("_").map{case _ => WildcardPattern()}

  /// ***************** pattern end *************************** ///




  
  lazy val simpleExpr: Syntax[Expr] = 
    literal.up[Expr] | variableOrCallOrArray | error | unitOrParenthese 

  lazy val simpleExprNoId: Syntax[Expr] =
    literal.up[Expr] | error | unitOrParenthese 

  // avoid first first conflict on (
  lazy val unitOrParenthese: Syntax[Expr] =
    ("(" ~ opt(expr) ~ ")").map{
      case tk ~ optExpr ~ _ => optExpr match
        case None => UnitLiteral().setPos(tk)
        case Some(new_expr) => ParenthesizedExpr(new_expr).setPos(tk)
    }

  lazy val error: Syntax[Expr] = 
    (kw("error") ~ "(" ~ expr ~ ")").map{
     case tke ~ _ ~ errorExpr ~ _ => Error(errorExpr).setPos(tke)
    }

  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }
    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}