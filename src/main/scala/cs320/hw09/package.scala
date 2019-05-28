package cs320

package object hw09 extends Homework09 {

  def typeCheck(str: String): Type = {
    case class TypeEnv(
                        vars: Map[String, Type] = Map(),
                        tbinds: Map[String, Map[String, Type]] = Map()
                      ) {
      def addVar(x: String, t: Type): TypeEnv =
        copy(vars = vars + (x -> t))

      def addTBind(x: String, cs: Map[String, Type]): TypeEnv =
        copy(tbinds = tbinds + (x -> cs))
    }

    def mustSame(left: Type, right: Type): Type = {
      if (same(left, right)) left
      else notype(s"$left is not equal to $right")
    }

    def same(left: Type, right: Type): Boolean = {
      (left, right) match {
        case (NumT, NumT) => true
        case (BoolT, BoolT) => true
        case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
          same(p1, p2) && same(r1, r2)
        case (IdT(name1), IdT(name2)) =>
          if (name1 == name2) true
          else false
        case _ => false
      }
    }

    def notype(msg: Any): Nothing = error(s"no type: $msg")

    def validType(ty: Type, typeEnv: TypeEnv): Type = ty match {
      case NumT => ty
      case BoolT => ty
      case ArrowT(s, e) =>
        ArrowT(validType(s, typeEnv), validType(e, typeEnv))
      case IdT(x) =>
        if (typeEnv.tbinds.contains(x)) ty
        else notype(s"$x is a free type")
    }

    def typeCheck_run(corel: COREL, typeEnv: TypeEnv): Type = {
      corel match {
        case Num(_) => NumT
        case Bool(_) => BoolT
        case Add(lhs, rhs) =>
          mustSame(typeCheck_run(lhs, typeEnv), NumT)
          mustSame(typeCheck_run(rhs, typeEnv), NumT)
        case Sub(lhs, rhs) =>
          mustSame(typeCheck_run(lhs, typeEnv), NumT)
          mustSame(typeCheck_run(rhs, typeEnv), NumT)
        case Equ(lhs, rhs) =>
          mustSame(typeCheck_run(lhs, typeEnv), NumT)
          mustSame(typeCheck_run(rhs, typeEnv), NumT)
          BoolT
        case With(name, ty, expr, body) =>
          mustSame(ty, typeCheck_run(expr, typeEnv))
          typeCheck_run(body, typeEnv.addVar(name, ty))
        case Id(name) =>
          typeEnv.vars.getOrElse(name, notype(s"free identifier $name"))
        case Fun(param, paramty, body) =>
          validType(paramty, typeEnv)
          ArrowT(paramty, typeCheck_run(body, typeEnv.addVar(param, paramty)))
        case App(funE, argE) =>
          val funT = typeCheck_run(funE, typeEnv)
          val argT = typeCheck_run(argE, typeEnv)
          funT match {
            case ArrowT(start, end)
              if same(argT, start) => end
            case _ => notype(s"apply $argT to $funT")
          }
        case IfThenElse(thstE, thenE, elseE) =>
          mustSame(typeCheck_run(thstE, typeEnv), BoolT)
          val thenT = typeCheck_run(thenE, typeEnv)
          val elseT = typeCheck_run(elseE, typeEnv)
          mustSame(thenT, elseT)
        case Rec(fname, fty, pname, pty, body) =>
          validType(fty, typeEnv)
          fty match {
            case ArrowT(start, end) =>
              validType(pty, typeEnv)
              mustSame(start, pty)
              mustSame(end, typeCheck_run(body,
                typeEnv.addVar(fname, fty).addVar(pname, pty)))
              fty
            case _ => notype(s"not a closure : $fty")
          }
        case WithType(name, constructors, body) =>
          def addVarList(constMap: Map[String, Type], typeEnvT: TypeEnv): TypeEnv ={
            constMap.map({case (key: String, value: Type) => (key, value)}).toList match{
              case Nil => typeEnvT
              case h::t =>
                validType(h._2, typeEnv)
                addVarList(constMap - h._1, typeEnvT.addVar(h._1, ArrowT(h._2, IdT(name))))
            }
          }

          val typeEnvT:TypeEnv = typeEnv.addTBind(name, constructors)
          val typeEnvV:TypeEnv = addVarList(constructors, typeEnvT)
          constructors.map({ case (key: String, value: Type) => validType(value, typeEnvT) })
          val ret = typeCheck_run(body, typeEnvV)
          validType(ret, typeEnv) //for soundness
        case Cases(name, dispatchE, cases) =>
          def mustSameList(typeList: List[Type]): Type = {
            if (typeList.length == 0) {
              notype("zero case class error!")
            }
            typeList match {
              case h :: t =>
                if (t.length == 0) h
                else mustSame(h, mustSameList(t))
              case Nil => notype("This case may not occur!")
            }
          }

          val cs = typeEnv.tbinds.getOrElse(name, notype(s"$name is a free typee"))
          //cases랑, cs의 크기 비교
          if (cases.size != cs.size) {
            notype("not all cases")
          }else{
            mustSame(typeCheck_run(dispatchE, typeEnv), IdT(name))
            var typeList: List[Type] = cases.map({
              case (key: String, value: (String, COREL)) =>
                //value._2 = value._1에 해당하는 case에 대한 COREL.
                //value._1 = case 이름
                //cs: 특정 type에 대하여 variant name -> variant value(type) 으로 되어있는 Map
                typeCheck_run(value._2, typeEnv.addVar(value._1, cs.getOrElse(key, notype(s"$key is free"))))
            }).toList
            mustSameList(typeList)
          }
      }
    }
    //typeCheck_run(COREL(str), TypeEnv())
    validType(typeCheck_run(COREL(str), TypeEnv()), TypeEnv())
    //validType for type soundness
  }
  def interp(str: String): String = {
    trait CORELValue
    case class NumV(n: Int) extends CORELValue
    case class BoolV(b: Boolean) extends CORELValue
    case class CloV(x: String,
                    body: COREL,
                    var env: Env) extends CORELValue
    case class VariantV(name: String, value: CORELValue) extends CORELValue
    case class ConstructorV(name: String) extends CORELValue

    type Env = Map[String, CORELValue]

    case class TypeEnv(
                        vars: Map[String, Type] = Map(),
                        tbinds: Map[String, Map[String, Type]] = Map()
                      ) {
      def addVar(x: String, t: Type): TypeEnv =
        copy(vars = vars + (x -> t))

      def addTBind(x: String, cs: Map[String, Type]): TypeEnv =
        copy(tbinds = tbinds + (x -> cs))
    }

    def numOp(op: (Int, Int) => Int): (CORELValue, CORELValue) => CORELValue = (_, _) match{
      case (NumV(x), NumV(y)) => NumV(op(x,y))
      case (x, y) => error(s"not both numbers: $x, $y")
    }
    val numVAdd = numOp(_ + _)
    val numVSub = numOp(_ - _)

    def interp_run(corel: COREL, env: Env): CORELValue = {
      corel match {
        case Num(n) => NumV(n)
        case Bool(b) => BoolV(b)
        case Add(lhs, rhs) =>
          numVAdd(interp_run(lhs, env), interp_run(rhs, env))
        case Sub(lhs, rhs) =>
          numVSub(interp_run(lhs, env), interp_run(rhs, env))
        case Equ(lhs, rhs) =>
          if (numVSub(interp_run(lhs, env), interp_run(rhs, env)) == NumV(0)) BoolV(true)
          else BoolV(false)
        case With(name, _, expr, body) =>
          interp_run(body, env + (name -> interp_run(expr, env)))
        case Id(name) =>
          env.getOrElse(name, error(s"free identifier: $name"))
        case Fun(param, _, body) =>
          CloV(param, body, env)
        case App(funE, argE) =>
          interp_run(funE, env) match {
            case CloV(param, body, fenv) =>
              interp_run(body, fenv + (param -> interp_run(argE, env)))
            case ConstructorV(name) =>
              VariantV(name, interp_run(argE, env))
            case fv => error(s"not a closure: $fv")
          }
        case IfThenElse(thstE, thenE, elseE) =>
          if (interp_run(thstE, env) == BoolV(true)) interp_run(thenE, env)
          else interp_run(elseE, env)
        case Rec(fname, _, pname, _, body) =>
          val cloV = CloV(pname, body, env)
          cloV.env = env + (fname -> cloV)
          cloV
        case WithType(_, constructors, body) =>
          val envC: Env = constructors.map(
            { case (key: String, value: Type) =>
              key -> ConstructorV(key)
            })
          interp_run(body, env ++ envC)
        case Cases(_, dispatchE, cases) =>
          interp_run(dispatchE, env) match {
            case VariantV(name, av) =>
              val matchCase: (String, COREL) = cases.getOrElse(name, error(s"$name is a free constructor"))
              interp_run(matchCase._2, env + (matchCase._1 -> av))
            case v => error(s"not a variant: $v")
          }
      }
    }

    val ret = interp_run(COREL(str), Map())
    ret match{
      case NumV(n: Int) => n.toString
      case BoolV(b: Boolean) =>
        if (b) "true"
        else "false"
      case CloV(x: String, body: COREL, env: Env) => "function"
      case VariantV(name: String, value: CORELValue) => "variant"
      case ConstructorV(name: String) => "constructor"
      case _ => "Escapse case!!"
        //VariantV, constructorV cases are error cases. They are against the type soundness rule!
    }
  }

  def tests: Unit = {
    test(run("42"), "42")
    test(run("true"), "true")
    test(run("{+ 1 2}"), "3")
    test(run("{- 2 1}"), "1")
    test(run("{= 1 0}"), "false")
    testExc(run("{= true 0}"), "no type")
    test(run("{with {x : num 1} x}"), "1")
    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    test(run("""
      {{recfun {f: {num -> num} x: num}
               {if {= x 0} 0 {+ {f {- x 1}} x}}}
       10}"""), "55")
    testExc(run("{if 1 2 3}"), "no type")
    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}
               {banana {y} y}}}"""), "1")
    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}}}"""), "not all cases")

    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {{fun {x: fruit}
          {cases fruit x
            {apple {n} n}
            {banana {m} {+ 1 m}}}}
        {banana 3}}}"""), "4")

    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {withtype
          {color {apple {num -> num}}
                 {banana bool}}
          {{fun {x: fruit}
            {cases fruit x
              {apple {n} n}
              {banana {m} {+ m 4}}}}
          {banana true}}}}"""), "no type")

    testExc(run("""
        {{withtype
          {foo  {a num}
                {b num}}
          {fun {x : foo}
            {cases foo x
              {a {n} {+ n 3}}
              {b {n} {+ n 4}}}}}
        {withtype
          {foo {c {num -> num}}
               {d num}}
          {c {fun {y : num} y}}}}"""), "no type")

    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {banana 3}}"""), "no type") //type soundness

    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        banana}"""), "no type") //type soundness

    testExc(run("""
      {withtype
        {fruit {apple error}
               {banana error}}
        {{fun {x: fruit}
          {cases fruit x
            {apple {n} n}
            {banana {m} m}}}
        {banana 3}}}"""), "no type") //withtype vaild type
    testExc(run("""
      {{fun {x: fruit}
          {+ 1 2}}
         3}"""), "no type") //fun valid type


  }
}
