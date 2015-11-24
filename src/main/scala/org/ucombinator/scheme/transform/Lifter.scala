package org.ucombinator.scheme.transform

import org.ucombinator.scheme.syntax._

/**
 * Lifts globalizable expressions (quoted expressions and closed lambda terms) to the top-level.
 */
class Lifter extends ProgramTransformer {

  private var prog: Program = null

  private var liftedDefs: List[Def] = null

  var changed = false

  private def addDefine(exp: Exp): Exp = {
    changed = true
    val newName = SName.gensym("lifted")
    liftedDefs = VarDef(newName, exp) :: liftedDefs
    Ref(newName)
  }

  def apply(prog: Program): Program = {
    this.prog = prog
    this.changed = false

    liftedDefs = Nil

    prog match {
      case Program(decs, defs, init) => {
        val newDefs = defs map (lift(_, true))
        val liftedInit = lift(init, true)
        Program(decs, liftedDefs ++ newDefs, liftedInit)
      }
    }
  }


  private def lift(d: Def, isTop: Boolean): Def = {
    d match {
      case VarDef(name, value) =>
        VarDef(name, lift(value, isTop))
    }
  }


  private def lift(exp: Exp, isTop: Boolean): Exp = {
    exp match {
      case _: Ref => exp
      case _: Unspecified => exp
      case _: Prim => exp
      case _: CPS => exp
      case QuoteLit(_) if !exp.mayAllocate => exp

      case _: QuoteLit if !isTop =>
        addDefine(exp)
      case _: Lit => exp

      case lam@Lambda(formals, body) if !isTop && (lam.free -- prog.globals).isEmpty => {
        val newLam = Lambda(formals, lift(body))
        addDefine(newLam)
      }
      case Lambda(formals, body) =>
        Lambda(formals, lift(body))

      case Let(bindings, body) =>
        Let(bindings map {
          case Binding(name, value) => Binding(name, lift(value, false))
        },
          lift(body))

      case MakeCell(value) =>
        MakeCell(lift(value, false))

      case CellGet(cell) =>
        CellGet(lift(cell, false))

      case SetCell(cell, value) =>
        SetCell(lift(cell, false), lift(value, false))

      case Closure(lam, ty, fields) => {
        Closure(lift(lam, false), ty, fields map (lift(_, false)))
      }

      case Begin(body) =>
        Begin(lift(body))

      case SetVar(name, value) =>
        SetVar(name, lift(value, false))

      case MakeStruct(ty, exps) =>
        MakeStruct(ty, exps map (lift(_, false)))

      case StructGet(base, field, ty) =>
        StructGet(lift(base, false), field, ty)

      case App(f, args) =>
        App(lift(f, false), lift(args))

      case Call(clo, key, args) => {
        Call(lift(clo, false), key, lift(args))
      }
    }
  }

  private def lift(arguments: Arguments): Arguments = {
    arguments match {
      case Arguments(args, rest) =>
        Arguments(args map lift, rest map (lift(_, false)))
    }
  }

  private def lift(argument: Argument): Argument = {
    argument match {
      case PosArgument(exp) => PosArgument(lift(exp, false))
      case KeywordArgument(kw, exp) => KeywordArgument(kw, lift(exp, false))
    }
  }

  private def lift(body: Body): Body = {
    body match {
      case Body(defs, exps) => {
        Body(defs map (lift(_, false)),
          exps map (lift(_, false)))
      }
    }
  }

}
