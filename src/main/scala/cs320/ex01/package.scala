package cs320

//case class Num(num: Int) extends WAE                                 // e ::= n
//case class Add(left: WAE, right: WAE) extends WAE                    //     | {+ e e}
//case class Sub(left: WAE, right: WAE) extends WAE                    //     | {- e e}
//case class With(name: String, expr: WAE, body: WAE) extends WAE      //     | {with {x e} e}
//case class Id(id: String) extends WAE

package object ex01 extends Exercise01 {
  // Problem 1
  def freeIds(expr: WAE): Set[String] = {
    def freeIds_env(expr: WAE, env:Set[String]) : Set[String] = {
      expr match {
        case Num(num) => Set()
        case Add(left, right) => freeIds_env(left, env) ++ freeIds_env(right,env)
        case Sub(left, right) => freeIds_env(left, env) ++ freeIds_env(right,env)
        case With(name, expr, body) => freeIds_env(expr, env) ++ freeIds_env(body, env + (name))
        case Id(id) => if (env contains id) Set() else Set(id)
      }
    }
    freeIds_env(expr,Set())
  }

  // Problem 2
  def bindingIds(expr: WAE): Set[String] = {
    def bindingIds_env(expr: WAE, env:Set[String]) : Set[String] = {
      expr match {
        case Num(num) => Set()
        case Add(left, right) => bindingIds_env(left, env) ++ bindingIds_env(right,env)
        case Sub(left, right) => bindingIds_env(left, env) ++ bindingIds_env(right,env)
        case With(name, expr, body) => Set(name) ++ bindingIds_env(expr, env) ++ bindingIds_env(body, env + (name))
        case Id(id) => Set()
      }
    }
    bindingIds_env(expr,Set())
  }

  // Problem 3
  def boundIds(expr: WAE): Set[String] = {
    def boundIds_env(expr: WAE, env:Set[String]) : Set[String] = {
      expr match {
        case Num(num) => Set()
        case Add(left, right) => boundIds_env(left, env) ++ boundIds_env(right,env)
        case Sub(left, right) => boundIds_env(left, env) ++ boundIds_env(right,env)
        case With(name, expr, body) => boundIds_env(expr, env) ++ boundIds_env(body, env + (name))
        case Id(id) => if (env contains id) Set(id) else Set()
      }
    }
    boundIds_env(expr,Set())
  }

  // Tests
  def tests: Unit = {
    test(freeIds(WAE("{with {x 1} {+ x y}}")), Set("y"))
    test(freeIds(WAE("{with {z 2} 1}")), Set())
    test(bindingIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(bindingIds(WAE("{with {z 2} 1}")), Set("z"))
    test(boundIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(boundIds(WAE("{with {z 2} 1}")), Set())

    /* Write your own tests */
  }
}
