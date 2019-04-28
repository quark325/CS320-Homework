package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = { op(l,r) }
      rs.map(f) ++ binOp(op, rest, rs)
  }
  def run(str: String): List[Int] = {
    def run_env (muwae: MUWAE, env:Map[String,List[Int]]): List[Int] = muwae match {
      case Num(nums: List[Int])
      => nums
      case Add(left: MUWAE, right: MUWAE)
      => def add(l: Int, r:Int) : Int = {l+r}
        binOp(add, run_env(left,env), run_env(right,env))
      case Sub(left: MUWAE, right: MUWAE)
      => def sub(l: Int, r:Int) : Int = {l-r}
        binOp(sub, run_env(left,env), run_env(right,env))
      case With(name: String, expr: MUWAE, body: MUWAE)
      => val name_value = run_env(expr, env)
        run_env(body, env+(name -> name_value))
      case Id(id: String)
      => lookup(id, env)
      case Min(left: MUWAE, mid: MUWAE, right: MUWAE)
      => val left_ls: List[Int] = run_env(left, env)
        val mid_ls: List[Int] =  run_env(mid, env)
        val right_ls: List[Int] = run_env(right, env)
        def small(a:Int, b:Int) : Int = {if(a<b) a else b}
        val temp : List[Int] = binOp(small, left_ls, mid_ls)
        binOp(small, temp, right_ls)
      case Max(left: MUWAE, mid: MUWAE, right: MUWAE)
      => val left_ls: List[Int] = run_env(left, env)
        val mid_ls: List[Int] =  run_env(mid, env)
        val right_ls: List[Int] = run_env(right, env)
        def big(a:Int, b:Int) : Int = {if(a>b) a else b}
        val temp : List[Int] = binOp(big, left_ls, mid_ls)
        binOp(big, temp, right_ls)
    }
    def lookup(name: String, env:Map[String,List[Int]]) :List[Int] = {
      env.get(name) match {
        case Some(v) => v
        case None => error(s"free idnetifier: $name")
      }
    }
    run_env(MUWAE(str), Map())
  }

  def tests: Unit = {
    /* my own testcase */
    test(run("{+ 3 7}"), List(10))
    test(run("{+ 3 {7 14}}"), List(10, 17))
    test(run("{+ {3 7} 14}"), List(17, 21))
    test(run("{+ {3 7} {7 14}}"), List(10, 17, 14, 21))

    test(run("{- 10 3}"), List(7))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{- {10 9} 3}"), List(7, 6))
    test(run("{- {10 4} {3 5}}"), List(7, 5, 1, -1))

    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{with {x {with{x {- 2 1}} {+ x 3} }} {+ x x}}"), List(8))
    test(run("{with {x 5} {with {y 4} {+ x y}}}}"), List(9))

    test(run("{min 3 4 5}"), List(3))
    test(run("{min 3 4 {}}"), List())
    test(run("{min 3 {} 5}"), List())
    test(run("{min {} 4 5}"), List())
    test(run("{min {} {} 5}"), List())
    test(run("{min {} 4 {}}"), List())
    test(run("{min 3 {} {}}"), List())
    test(run("{min 3 {4 5} {}}"), List())
    test(run("{min {3 4} {} 5}"), List())
    test(run("{min {} 4 {3 5}}"), List())
    test(run("{min {} {} {1 5}}"), List())
    test(run("{min {} {1 4} {}}"), List())
    test(run("{min {3 2} {} {}}"), List())
    test(run("{min {1 4} {2 9} 3}}"), List(1, 1, 2, 3))
    test(run("{min {1 4} {2 9} {3 5}}"), List(1, 1, 1, 1, 2, 2, 3, 4))

    test(run("{max 3 4 {}}"), List())
    test(run("{max 3 {} 5}"), List())
    test(run("{max {} 4 5}"), List())
    test(run("{max 3 {4 5} {}}"), List())
    test(run("{max {3 4} {} 5}"), List())
    test(run("{max {} 4 {3 5}}"), List())
    test(run("{max {} {} 5}"), List())
    test(run("{max {} 4 {}}"), List())
    test(run("{max 3 {} {}}"), List())
    test(run("{max {} {} {1 5}}"), List())
    test(run("{max {} {1 4} {}}"), List())
    test(run("{max {3 2} {} {}}"), List())
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))
    test(run("{max {1 4} 2 {3 5 4}}"), List(3, 5, 4, 4, 5, 4))

    /*testcase for grading*/
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{+ {min 9 3 7} {max 6 2 20}}"), List(23))
    test(run("{+ {1 2} {3 4}}"), List(4, 5, 5, 6))
    test(run("{- {+ {1 2} {3 4}} {1 2}}"), List(3, 2, 4, 3, 4, 3, 5, 4))
    test(run("{- {10 2 1} {3 2}}"), List(7, 8, -1, 0, -2, -1))
    test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
    test(run("{with {x 9} {+ x {with {x 3} x}}}"), List(12))
    test(run("{with {x 100} {+ x {with {y 3} x}}}"), List(200))
    test(run("{with {x 5} {+ x {with {x 3} 10}}}"), List(15))
    test(run("{with {x {7 5}} {+ x x}}"), List(14, 12, 12, 10))
    test(run("{with {x {1 2}} {+ x {4 3}}}"), List(5, 4, 6, 5))
    test(run("{with {x 2} {- {+ x x} x}}"), List(2))
    test(run("{+ {min 3 5 7} {min 10 100 1000}}"), List(13))
    test(run("{+ {min 9 3 7} {max 6 2 20}}"), List(23))
    test(run("{with {x 10} {max x 2 3}}"), List(10))
    test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {max {+ x y} 0 12}}}}}"), List(35, 45))
    test(run("{with {x 20} {with {y 5} {with {z {10 20}} {+ z {min {+ x y} 0 12}}}}}"), List(10, 20))
    test(run("{with {x {min 3 9 5}} {with {y {- x 3}} y}}"), List(0))
    test(run("{with {x {max 2 3 5}} {min x 7 6}}"), List(5))
    test(run("{with {x {max 9 7 10}} {max 8 x {+ 1 x}}}"), List(11))
    test(run("{- {min 6 4 5} {max 2 3 4}}"), List(0))
    test(run("{with {x {+ 7 2}} {min x 7 0}}"), List(0))
    test(run("{+ {min 9 3 7} {max 6 2 20}}"), List(23))
    test(run("{with {x {13}} {min x 1 12}}"), List(1))
    test(run("{with {x {min 2 1 3}} {+ x x}}"), List(2))
    test(run("{with {a 10} {with {b 19} {with {c 2} {min a b c}}}}"), List(2))
    test(run("{with {x 3} {max 3 4 {+ x x}}}"), List(6))
    test(run("{with {a 10} {with {b 19} {with {c 2} {max a b c}}}}"), List(19))
    test(run("{with {x {min 2 5 4}} {+ x x}}"), List(4))
    test(run("{with {x {max 2 5 4}} {+ x x}}"), List(10))
    test(run("{with {x {- 11 3}} {max x {+ x x} {- x x}}}"), List(16))
    test(run("{with {x {- 11 3}} {min x {+ x x} {- x x}}}"), List(0))
    test(run("{min {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(3))
    test(run("{max {+ 4 4} {with {x 5} {+ x {with {x 3} 10}}} 3}"), List(15))
    test(run("{with {x {13}} {min x 1 12}}"), List(1))
    test(run("{with {x {10} } {max x 2 3}}"), List(10))
    test(run("{with {x {min 2 1 3}} {+ x x}}"), List(2))
    test(run("{with {x {max 2 1 3}} {+ x x}}"), List(6))
    test(run("{with {x 2} {min x 3 10}}"), List(2))
    test(run("{min {+ 4 4} 2 3} "), List(2))
    test(run("{max {+ 4 4} 2 3} "), List(8))
    test(run("{with {x 10} {min x 2 3}}"), List(2))
    test(run("{with {x 10} {max x 2 3}}"), List(10))
    test(run("{with {x {10}} {max x 2 3}}"), List(10))
    test(run("{max {+ 3 4} 5 6}"), List(7))
    test(run("{with {x {10}} {min x {3} {5}}}"), List(3))
    test(run("{with {x {10}} {max x {3} {5}}}"), List(10))
    test(run("{max {3} 4 {5}}"), List(5))
    test(run("{+ {10 100 1000 10000} {min {- 3 4} 5 6}}"), List(9, 99, 999, 9999))
  }
}
