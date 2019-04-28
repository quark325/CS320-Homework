package cs320

package object hw05 extends Homework05 {
  def run(str: String): String = {
    trait SRBFAEValue
    case class NumV(n: Int) extends SRBFAEValue
    case class CloV(x: String,
                    body: SRBFAE,
                    env: Env) extends SRBFAEValue
    case class BoxV(addr: Addr) extends SRBFAEValue
    case class RecV(rec: Rec) extends SRBFAEValue

    type Env = Map[String, SRBFAEValue]
    type Rec = Map[String, Addr]
    type Addr = Int
    type Sto = Map[Addr, SRBFAEValue]

    def numOp(op: (Int, Int) => Int): (SRBFAEValue, SRBFAEValue) => SRBFAEValue = (_, _) match{
      case (NumV(x), NumV(y)) => NumV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }
    val numVAdd = numOp(_ + _)
    val numVSub = numOp(_ - _)
    def lookup(name:String, env:Env) : SRBFAEValue = {
      env.getOrElse(name, error(s"free identifier: $name"))
    }
    def lookupRec(name:String, rec:Rec ) : Addr = {
      rec.getOrElse(name, error(s"no such field: $name"))
    }
    def storeLookup(addr: Addr, sto: Sto) : SRBFAEValue = {
      sto.getOrElse(addr, error(s"not in store: $addr"))
    }

    def malloc(sto: Sto) : Addr = {
      maxAddress(sto) + 1
    }
    def maxAddress(sto: Sto) : Addr = {
      sto.keySet.+(0).max //???
    }
    def interpSeqn(head: SRBFAE, tail: List[SRBFAE], env: Env, sto: Sto) : (SRBFAEValue, Sto)= {
      val (hv, hs) = interp(head, env, sto)
      tail match {
        case Nil =>
          (hv, hs)
        case h :: t =>
          interpSeqn(h, t, env, hs) // lastV, lastS
        case _ => error(s"not a Seqn $tail")
      }
    }

    def interp(srbfae: SRBFAE, env: Env, sto: Sto): (SRBFAEValue, Sto) = srbfae match {
      case Num(num: Int) => // e ::= n
        (NumV(num), sto)
      case Add(left: SRBFAE, right: SRBFAE) => //     | {+ e e}
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (numVAdd(lv, rv), rs)
      case Sub(left: SRBFAE, right: SRBFAE) => //     | {- e e}
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (numVSub(lv, rv), rs)
      case Id(name: String) => //     | x
        (lookup(name, env), sto)
      case Fun(param: String, body: SRBFAE) => //     | {fun {x} e}
        (CloV(param, body, env), sto)
      case App(fun: SRBFAE, arg: SRBFAE) => //     | {e e}
        val (fv, fs) = interp(fun, env, sto)
        val (av, as) = interp(arg, env, fs)
        fv match {
          case CloV(param, body, fenv) =>
            interp(body, fenv + (param -> av), as)
          case _ => error(s"not a closure: $fv")
        }
      case NewBox(expr: SRBFAE) => //     | {newbox e}
        val (v, s) = interp(expr, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))
      case SetBox(box: SRBFAE, expr: SRBFAE) => //     | {setbox e e}
        val (bv, bs) = interp(box, env, sto)
        bv match {
          case BoxV(addr) =>
            val (ev, es) = interp(expr, env, bs)
            (ev, es + (addr -> ev))
          case _ => error(s"not a box :$bv")
        }
      case OpenBox(box: SRBFAE) => //     | {openbox e}
        val (bv, bs) = interp(box, env, sto)
        bv match {
          case BoxV(addr) =>
            (storeLookup(addr, bs), bs)
          case _ => error(s"not a box: $bv")
        }
      case Seqn(left, right) => //     | {seqn e e*}
        interpSeqn(left, right, env, sto)
      case Rec(mapping) =>                  //     | {rec {<id> e}*}
        def interpRec(head: (String, SRBFAE), tail: List[(String, SRBFAE)], fsto: Sto) : (Rec, Sto) = {
          val (headv, heads) = interp(head._2, env, fsto)
          tail match {
            case Nil =>
              val addr = malloc(heads)
              val rEnv = Map[String, Addr](head._1 -> addr)
              (rEnv, heads + (addr -> headv))
            case h :: t =>
              val (trec, tsto) = interpRec(h, t, heads)
              val addr = malloc(tsto)
              (trec + (head._1 -> addr), tsto + (addr -> headv))
            case _ => error(s"not a record $tail")
          }
        }
        val retRec = mapping.toList
        retRec match{
          case Nil =>
            val emptyMap : Rec = Map[String, Addr]()
            (RecV(emptyMap), sto)
          case h :: t =>
            val (renv, rsto) = interpRec(h, t, sto) // ????????????????????????
            (RecV(renv), rsto)
          case _ => error(s"not a fields $mapping")
        }
      case Get(record, field) =>                //     | {get e <id>}
        val (rv, rs) = interp(record, env, sto)
        rv match {
          case RecV(rec) =>
            val addr = rec.getOrElse(field, error(s"no such field $field"))
            val value = rs.getOrElse(addr, error(s"no such data $addr"))
            (rs.getOrElse(addr, error(s"no such data $addr")), rs)
          case _ => error(s"not a record $rv")
        }
      case Set(record, field, expr) =>  //     | {set e x e}
        val (rv, rs) = interp(record, env, sto)
        rv match {
          case RecV(rec) =>
            val (ev, es) = interp(expr, env, rs)
            val addr = lookupRec(field, rec)
            (ev, es + (addr -> ev))
          case _ => error(s"not a record $rv")
        }
    }
    val (retv, rets) = interp(SRBFAE(str), Map(), Map())
    retv match {
      case NumV(n: Int) => n.toString
      case CloV(x: String, body: SRBFAE, env: Env) => "function"
      case BoxV(addr: Addr) => "box"
      case RecV(rec: Rec) => "record"
      case _ => "remain"
    }
  }
  def tests: Unit = {
    // basic features (15)
    test(run("""{+ 1 2}"""), "3")
    test(run("""{- {+ 1 2} 3}"""), "0")
    test(run("""{fun {x} x}"""), "function")
    test(run("""{fun {x} {+ x 1}}"""), "function")
    test(run("""{{{fun {f} {fun {x} {f x}}} {fun {x} {+ x 1}}} 42}"""), "43")
    test(run("""{{{fun {g} {fun {y} {g y}}} {fun {k} {- k 1}}} 42}"""), "41")
    test(run("""{{fun {x} {{{fun {x} {fun {y} x}} 2} 42}} 1}"""), "2")
    test(run("""{{fun {a} {{{fun {a} {fun {b} a}} 3} 7}} 5}"""), "3")
    test(run("""{newbox 1}"""), "box")
    test(run("""{newbox {fun {x} x}}"""), "box")
    test(run("""{setbox {newbox 1} 2}"""), "2")
    test(run("""{setbox {newbox 2} {fun {x} x}}"""), "function")
    test(run("""{{fun {b} {openbox b}} {newbox 1}}"""), "1")
    test(run("""{{fun {b} {seqn {setbox b 42} {openbox b}}} {newbox 1}}"""), "42")
    testExc(run("""{openbox 1}"""), "")

    // improving sequences (10)
    test(run("""{seqn 1}"""), "1")
    test(run("""{seqn {fun {x} x}}"""), "function")
    test(run("""{seqn 1 2 3}"""), "3")
    test(run("""{seqn 1 {+ 1 2} {newbox 1}}"""), "box")
    test(run("""{seqn 1 2 3 4 5}"""), "5")
    test(run("""{seqn 1 {newbox 1} {fun {x} x} 3 {- 42 1}}"""), "41")
    test(run("""{{fun {b} {seqn {openbox b}}} {newbox 1}}"""), "1")
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                              {setbox b {+ 3 {openbox b}}}
                              {setbox b {+ 4 {openbox b}}}
                              {openbox b}}}
              {newbox 1}}"""), "10")
    test(run("""{{{fun {f} {fun {b} {seqn {f b} {f b} {f b} {openbox b}}}} {fun {b} {setbox b {+ 1 {openbox b}}}}} {newbox 1}}"""), "4")
    test(run("""{{{fun {f} {fun {b} {seqn {f b} {f b} {f b} {openbox b}}}} {fun {b} {setbox b {+ {openbox b} {openbox b}}}}} {newbox 1}}"""), "8")

    // records (25)
    test(run("""{rec}"""), "record")
    test(run("""{rec {x 1} {y 2} {z 3}}"""), "record")
    test(run("""{get {rec {x 1} {y 2} {z 3}} y}"""), "2")
    test(run("""{get {rec {x 1} {y 2} {z 3}} z}"""), "3")
    testExc(run("""{get {rec {x 1}} y}"""), "no such field")
    testExc(run("""{get {rec {x 1}} a}"""), "no such field")
    test(run("""{set {rec {x 1} {y 2} {z 3}} x 3}"""), "3")
    test(run("""{set {rec {x 1} {y 2} {z 3}} y 10}"""), "10")
    testExc(run("""{set {rec {x 1}} y 2}"""), "no such field")
    testExc(run("""{set {rec {x 1}} a 2}"""), "no such field")
    test(run("""{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"""), "5")
    test(run("""{{fun {r} {seqn {set r x 7} {get r x}}} {rec {x 42}}}"""), "7")
    test(run("""{openbox {{fun {b} {get {rec {x {setbox b 3}} {y b}} y}} {newbox 1}}}"""), "3")
    test(run("""{openbox {{fun {b} {get {rec {x {setbox b 42}} {y b}} y}} {newbox 1}}}"""), "42")
    testExc(run("""{seqn {rec {a 1}} {get {rec {b 2}} a}}"""), "no such field")
    testExc(run("""{seqn {rec {a 1}} {get {rec {x 2}} a}}"""), "no such field")
    testExc(run("""{{fun {x} {get {rec} b}} {rec {b 2}}}"""), "no such field")
    testExc(run("""{{fun {x} {get {rec {a 1}} b}} {rec {b 2}}}"""), "no such field")
    testExc(run("""{get {fun {x} x} x}"""), "")
    testExc(run("""{get {fun {x} {rec {x 1}}} x}"""), "")
    testExc(run("""{set {fun {x} x} x 1}"""), "")
    testExc(run("""{set {fun {x} {rec {x 2}}} x 1}"""), "")
    testExc(run("""{get {set {rec {a 3}} a 4} a}"""), "")
    testExc(run("""{get {set {rec {b 3}} b 4} b}"""), "")
    test(run("""{{fun {r} {seqn {set r x r}
                              {get {get {get {get {get {get r x} x} x} x} x} y}}}
               {rec {x 42} {y 1}}}"""), "1")
  }
}

