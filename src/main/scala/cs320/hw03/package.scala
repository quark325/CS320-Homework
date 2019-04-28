package cs320

package object hw03 extends Homework03 {
  def run(str: String): String = {
    trait MRFWAEValue
    case class NumV(n: Int) extends MRFWAEValue
    case class CloV(param: List[String],
                    body: MRFWAE,
                    env: Env) extends MRFWAEValue
    case class RecV(env: Env) extends MRFWAEValue
    type Env = Map[String, MRFWAEValue]

    def numOp(op: (Int, Int) => Int): (MRFWAEValue, MRFWAEValue) => MRFWAEValue = (_, _) match{
      case (NumV(x), NumV(y)) => NumV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }
    val numAdd = numOp(_ + _)
    val numSub = numOp(_ - _)

    def lookup(id: String, env: Env): MRFWAEValue = {
      env.getOrElse(id, error(s"free idnetifier: $id"))
    }

    def paramMatch(params: List[String], args: List[MRFWAE], env: Env) : Env = params match {
      case paramh::paramt => args match {
        case argsh::argst => paramMatch(paramt, argst, env) + (paramh -> run_env(argsh, env))
        case Nil => error("wrong arity")
      }
      case Nil => Map()
    }

    def run_env(muwae: MRFWAE, env: Env): MRFWAEValue = muwae match {
      case Num(num: Int)
      => NumV(num)
      case Add(left: MRFWAE, right: MRFWAE)
      => numAdd(run_env(left, env), run_env(right, env))
      case Sub(left: MRFWAE, right: MRFWAE)
      => numSub(run_env(left, env), run_env(right, env))
      case With(name: String, expr: MRFWAE, body: MRFWAE)
      => run_env(body, env + (name -> run_env(expr, env)))
      case Id(id: String)
      => lookup(id, env)
      case App(func: MRFWAE, args: List[MRFWAE])
      => run_env(func, env) match {
        case CloV(params, body, fenv)
        => if (params.length == args.length){
          run_env(body, fenv ++ paramMatch(params, args, env))
        }else{
          error("wrong arity")
        }
        case v
        => error(s"not a closure : $v")
        }
      case Fun(params: List[String], body: MRFWAE)
      => CloV(params, body, env)
      case Rec(rec)
      => RecV(rec map {case(key : String, value : MRFWAE) => (key,run_env(value, env))})
      case Acc(expr, name)
      => run_env(expr, env) match {
        case RecV(frec)
        => frec.get(name) match {
          case Some(v) => v
          case _ => error(s"no such field $name")
        }
        case v => error(s"not a record : $v")
      }
    }
    run_env(MRFWAE(str), Map()) match {
      case NumV(num) => num.toString
      case CloV(param, body, env) => "function"
      case RecV(rec) => "record"
    }
  }

  def tests: Unit = {
    /* Write your own tests */
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2} } z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{{fun {x} x} 1}"), "1")
    test(run("{record {x 1}}"), "record")
    test(run("{record {x 1} {y 2}}"), "record")
    test(run("{access {record {x {fun{z} {+ z 1}}} {y 2}} x}"), "function")

    /* testcase for grading */
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
    test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
    test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
    test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
    test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
    test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
    test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
    test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} d}}"), "2")
    test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {+ 1 2}}} a}"), "3")
    test(run("{fun {x} x}"), "function")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{record {a {- 2 1}}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {- 2 1}}} a}"), "1")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y z}}"), "3")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    testExc(run("{access {record {b 10} {b {+ 1 2}}} b}"), "duplicate fields")
    testExc(run("{access {record {a 10}} b}"), "no such field")
    testExc(run("{record {z {access {record {z 0}} y}}}"), "no such field")
  }
}
