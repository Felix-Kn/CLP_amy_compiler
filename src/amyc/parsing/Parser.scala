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
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

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
    case id1 ~ optId => 
      optId match
        case None => TypeTree(ClassType(QualifiedName(None, id1)))
        case Some(_ ~ id2) => TypeTree(ClassType(QualifiedName(Some(id1), id2)))
  }
    

    
  def foldMatchs(x : Expr) = (matchList: Seq[Expr]) => 
    matchList.foldLeft(x)((leftExpr, rightMatch) => {
      rightMatch match
        case Match(_, cases) => Match(leftExpr, cases)
    })

  def foldSequences = (seqList :Seq[Expr]) => {
    assert(!seqList.isEmpty)
    val newList = seqList.init
    val lastElem = seqList.last
    newList.foldRight(lastElem)((leftExpr, rightExpr) => Sequence(leftExpr, rightExpr))
  }
    
  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence

  // Stop the expr recursion when we hit a newVal definition
  // newVal will then create a new sequence with old-Context + newVal 
  lazy val expr: Syntax[Expr] = recursive { 
    ((exprNoVal ~ opt(";" ~ expr))|| newVal).map{

      case toTest => toTest match
        // case Sequence of not newVal
        case Left(notVal1 ~ optNextExpr) => 
          optNextExpr match
            case None => notVal1
            case Some(_ ~ nextExpr) => Sequence(notVal1, nextExpr)
          
        case Right(isVal1) => isVal1
    }
  }     

  lazy val newVal: Syntax[Expr] = {
    (kw("val") ~ parameter ~ "=" ~ exprNoVal ~ ";" ~ expr).map{
      case _ ~ param ~ _ ~ expr1 ~ _ ~ expr2 => Let(param, expr1, expr2) 
    }
  }


  // first expr of a val can not contain a val
  lazy val exprNoVal: Syntax[Expr] =
    (N ~ opt(many1(matchs))).map{
      case firstExpre ~ optMatchs => 
        optMatchs match
        case None => firstExpre
        case Some(manyMatch) => foldMatchs(firstExpre)(manyMatch)           
    }
  
  lazy val N: Syntax[Expr] = 
    operators | ifThenElse 
  
  lazy val matchs: Syntax[Expr] = 
    (kw("match") ~ "{" ~ many1(matchCase) ~ "}").map{ 
      case _ ~ _ ~ caseSeq ~ _ => 
        Match(UnitLiteral(), caseSeq.toList)
    }
  
    
  lazy val unaryExp: Syntax[Expr] = 
    unaryOp | simpleExpr
  
  lazy val unaryOp: Syntax[Expr] = 
    unaryNeg | unaryNot
    
  lazy val unaryNot: Syntax[Expr] = 
    (op("!") ~ simpleExpr).map{
      case _ ~ notExpr => Not(notExpr) 
    }
  
  lazy val unaryNeg: Syntax[Expr] = 
    (op("-") ~ simpleExpr).map{
      case _ ~ negExpr => Neg(negExpr)
    }
    
    
  lazy val operators: Syntax[Expr] = 
    operators(unaryExp)(
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
    literal.up[Expr] | variableOrCall | error | unitOrParenthese 
  

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
    }| 
    followCall.map{
      case exprSeq => Call(QualifiedName(None, ""), exprSeq.toList)
    }

  lazy val variableOrCall: Syntax[Expr] = 
    (identifier ~ opt(isCall)).map{
      case id ~ optCall => 
        optCall match
          case None => Variable(id)
          case Some(call) => 
            call match
              case Call(QualifiedName(None, tempId), args) => 
                if(tempId.isEmpty()) Call(QualifiedName(None, id), args) else 
                  Call(QualifiedName(Some(id), tempId), args)
    } 

  // avoid first first conflict on (
  lazy val unitOrParenthese: Syntax[Expr] =
    ("(" ~ opt(expr) ~ ")").map{
      case tk ~ optExpr ~ _ => optExpr match
        case None => UnitLiteral().setPos(tk)
        case Some(new_expr) => new_expr.setPos(tk)
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