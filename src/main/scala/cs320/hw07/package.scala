package cs320

package object hw07 extends Homework07 {
  def run(str: String): String = {
    trait KXCFAEValue
    case class NumV(n: Int) extends KXCFAEValue
    case class CloV(x: List[String],
                    body: KXCFAE,
                    env: Env) extends KXCFAEValue
    case class ContV(k: Cont) extends KXCFAEValue
    case object ThrowV extends KXCFAEValue

    type Env = Map[String, KXCFAEValue]
    type Cont = KXCFAEValue => KXCFAEValue
    type Cont4List = List[KXCFAEValue] => KXCFAEValue

    def numOp(op: (Int, Int) => Int): (KXCFAEValue, KXCFAEValue) => KXCFAEValue = (_, _) match{
      case (NumV(x), NumV(y)) => NumV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }
    val numVAdd = numOp(_ + _)
    val numVSub = numOp(_ - _)

    val identK : Cont = (x: KXCFAEValue) => x

    def interpParams(args: List[KXCFAE], env: Env, retEnv: Env, index: Int, k: Cont) : KXCFAEValue = args match{
      case Nil =>
        k(CloV(List[String](), KXCFAE("1"), retEnv))
      case h::t =>
        interp(h, env, hv => {
          val newRetEnv = retEnv + (index.toString -> hv)
          interpParams(t, env, newRetEnv, index + 1, k)
        }
      )
    }

    def makingMap(params: List[String], aenv: Env, index: Int) : Env = {
      aenv map { case (key: String, value: KXCFAEValue) => (params(key.toInt), value) }
    }

    def interp(kxcfae:KXCFAE, env: Env, k: Cont) : KXCFAEValue = {
      kxcfae match {
        case Num(n) =>
          k(NumV(n))
        case Add(left, right) =>
          interp(left, env, lv =>
            interp(right, env, rv =>
              k(numVAdd(lv, rv))))
        case Sub(left, right) =>
          interp(left, env, lv =>
            interp(right, env, rv =>
              k(numVSub(lv, rv))))
        case Id(name) =>
          k(env.getOrElse(name, error(s"free identifier: $name")))
        case Fun(params, body) => //TODO Fun을 만들때 Throw가 있으면 이식하는 방법을 사용.
          k(CloV(params, body, env))
        case App(fun: KXCFAE, args: List[KXCFAE]) =>
          interp(fun, env, fv =>
            interpParams(args, env, Map[String, KXCFAEValue](), 0, av =>
              fv match {
                case CloV(params, body, fenv) => av match{
                  case CloV(aparams, abody, aenv) =>
                    if(params.length == aenv.size){
                      val argsMap = makingMap(params, aenv, 0)
                      val newEnv = env.getOrElse("", NumV(0)) match {
                        case ContV(kv) => fenv ++ argsMap + ("" -> ContV(kv))
                        case _ => fenv ++ argsMap
                      }
                      interp(body, newEnv, k)
                    }else{
                      error("wrong arity")
                    }
                  }
                case ContV(kv) => av match{
                  case CloV(aparams, abody, aenv) =>
                    if(aenv.size == 1){
                      kv(aenv.getOrElse("0", error("This error can't happen")))
                    }else{
                      //one more argument in fun when fun is ContV
                      error("wrong arity")
                    }
                }
                case v => error(s"not a closure: $v")
              }))
        case If0(cond: KXCFAE, thenE: KXCFAE, elseE: KXCFAE) =>
          interp(cond, env, cv =>
            cv match{
              case NumV(0) => interp(thenE, env, k)
              case _ => interp(elseE, env, k)
            })
        case Withcc(name: String, body: KXCFAE) =>
          interp(body, env+(name -> ContV(k)), k)
        case Try(tryE: KXCFAE, catchE: KXCFAE) =>
          val checkP = ((checkV: KXCFAEValue) => checkV match {
            case ThrowV => interp(catchE, env, k)
            case _ => error("Throw but no Thorw statement")
          })
          val tenv: Env = env + ("" -> ContV(checkP)) // TODO 이렇게 가능한가? 일단 해본다
          interp(tryE, tenv, k)
        case Throw => env.getOrElse("", NumV(0)) match{
          case ContV(kv) => kv(ThrowV)
          case _ =>
            error("no enclosing try-catch")
        }
      }
    }
    val ret = interp(KXCFAE(str), Map(), identK)
    ret match{
      case NumV(n: Int) => n.toString
      case CloV(x: List[String], body: KXCFAE, env: Env) => "function"
      case ContV(k: Cont) => "continuation"
      case _ => "Escapse case!!"
    }
  }

  def tests: Unit = {
    /* 1. NumV, function, continuation */
    test(run("12"), "12")
    test(run("{+ 3 4}"), "7")
    test(run("{- 5 1}"), "4")
    test(run("{fun {} 12}"), "function")
    test(run("{fun {k} k}"), "function")
    test(run("{fun {x y} {x}}"), "function")
    test(run("{withcc x x}"), "continuation")

    /* 2. wrong arity */
    testExc(run("{{fun {} 1} 2}"), "wrong arity")
    testExc(run("{{fun {} 1} 1 2}"), "wrong arity")
    testExc(run("{{fun {x} {+ x 1}} 1 2}"), "wrong arity")
    testExc(run("{{fun {x y} 1} 1}"), "wrong arity")

    /* 3. If0 example */
    test(run("{if0 1 3 4}"), "4")
    test(run("{if0 0 3 4}"), "3")
    test(run("{if0 {- 1 1} {fun {} 12} 4}"), "function")
    test(run("{if0 {- 1 2} {fun {} 12} {withcc x x}}"), "continuation")

    /* 4. mutliple variable */
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")

    /* 5. try-catch */
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    testExc(run("{throw}"), "no enclosing try-catch")
    test(run("{try {+ {throw} {throw}} catch 2}"), "2")
    testExc(run("{try {throw} catch {throw}}"), "no enclosing try-catch")

    /* 6. advance */
    test(run("{try {{fun {x y} x} 1 {throw}} catch 2}"), "2")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    test(run("{+ 1 {withcc k {{fun {x y} {+ x y}} {k 2} 4}}}"), "3")


    /* Write your own tests */
  }
}
