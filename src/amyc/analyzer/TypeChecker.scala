package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier


// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      val genBinConstraints = (lhs:Expr, rhs:Expr, genT:Type, binT:Type) 
        => genConstraints(lhs, genT) ++ genConstraints(rhs, genT) ++ topLevelConstraint(binT)

      e match {
        case Error(error) => 
          genConstraints(error, StringType)
          
        // val call
        case Variable(name) => 
          topLevelConstraint(env.get(name).get)
        // Literal types
        case IntLiteral(_) => topLevelConstraint(IntType)
        case BooleanLiteral(_) => topLevelConstraint(BooleanType)
        case StringLiteral(_) => topLevelConstraint(StringType)
        case UnitLiteral() => topLevelConstraint(UnitType)

        // Boolean operations
        case And(lhs, rhs) => genBinConstraints(lhs, rhs, BooleanType, BooleanType)
        case Or(lhs, rhs) => genBinConstraints(lhs, rhs, BooleanType, BooleanType)
        case Not(newE) => genConstraints(newE, BooleanType) ++ topLevelConstraint(BooleanType)

        // Int operations
        case Plus(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, IntType)
        case Times(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, IntType)
        case Div(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, IntType)
        case Mod(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, IntType)
        case Minus(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, IntType)
        case Neg(newE) => genConstraints(newE, IntType) ++ topLevelConstraint(IntType)

        case LessThan(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, BooleanType)
        case LessEquals(lhs, rhs) => genBinConstraints(lhs, rhs, IntType, BooleanType)
        
        // String operation
        case Concat(lhs, rhs) => genBinConstraints(lhs, rhs, StringType, StringType)

        //
        case Equals(lhs, rhs) =>
          val eqt = TypeVariable.fresh()
          genBinConstraints(lhs, rhs, eqt, BooleanType)
         
        case Sequence(e1, e2) => 
          val newConstraint = TypeVariable.fresh()
          genConstraints(e1, newConstraint) ++ genConstraints(e2, expected)

        case Ite(cond, e1, e2) => 
          genConstraints(cond, BooleanType) ++ genConstraints(e1, expected) ++ genConstraints(e2, expected)

        case Call(name, args) => 
          val funProperties = table.getFunction(name) match
            case None => table.getConstructor(name).get
            case Some(value) => value
          
          val argsConstraints = args
            .zip(funProperties.argTypes)
            .map((arg, argType) => genConstraints(arg, argType))
            .flatten
          topLevelConstraint(funProperties.retType) ++ argsConstraints
        
        case Let(df: ParamDef, value: Expr, body: Expr) => 
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected)(env.updated(df.name, df.tt.tpe))

        case Assign(df, value, body) => 
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected)(env.updated(df.name, df.tt.tpe))
        
        case t@reAssign(name, newValue) => 

          val varType = env.get(name).getOrElse(fatal("name analyzer failed to detect reassignement before declaration !", t))
          genConstraints(newValue, varType) ++ topLevelConstraint(UnitType)
        
        case While(cond, body) => 
          val bodyConstraint = TypeVariable.fresh()
          genConstraints(cond, BooleanType) ++ genConstraints(body, bodyConstraint) ++ topLevelConstraint(UnitType) 
            
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def patternBindings(pat: Pattern, expected: Type): (List[Constraint], Map[Identifier, Type]) = {
            pat match
              case WildcardPattern() => (Nil, Map())
              case IdPattern(name) => 
                env.get(name) match
                  case None => (Nil, Map((name, expected)))
                  case Some(existingType) =>(topLevelConstraint(existingType), Map())
              case LiteralPattern(lit) => (genConstraints(lit, expected), Map())

              case CaseClassPattern(name, args) => 
                
                val constructor = table
                  .getConstructor(name)
                  .get                  

                val (constraints, newIds) = 
                  args.zip(constructor.argTypes)
                  .map((pat, patType) => patternBindings(pat, patType))
                  .fold((Nil, Map()))((a, b) => (a._1++b._1, a._2++b._2))

                (constraints :+ Constraint(constructor.retType, expected, pat.position), newIds)
            
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = patternBindings(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env++moreEnv) 
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++
          cases.flatMap(cse => handleCase(cse, st))

      }
    }

    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type = {
      tpe match {
        case TypeVariable(`from`) => to
        case other => other
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          // TODO
          (found, expected) match
            case (TypeVariable(id1), TypeVariable(id2)) => 
              val updatedConstraints = subst_*(more, id1, expected)
              solveConstraints(updatedConstraints)

            case (TypeVariable(id), expectedType) => 
              val updatedConstraints = subst_*(more, id, expectedType)
              solveConstraints(updatedConstraints)
            
            case (actual, TypeVariable(id)) => 
              val updatedConstraints = subst_*(more, id, actual)
              solveConstraints(updatedConstraints)

            case (left, right) => 
              if left.toString != right.toString then ctx.reporter.error(f"Type conflict @ ${pos} => Resolved type: ${left} != Expected type : ${right} \n")
              solveConstraints(more) 
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
