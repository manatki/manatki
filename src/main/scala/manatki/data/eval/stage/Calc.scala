//package manatki.data.eval.stage
//
//import cats.{MonadError, StackSafeMonad}
//import cats.kernel.Monoid
//import cats.syntax.either._
//import manatki.data.eval.stage.Calc.Connect
//
//sealed trait Calc[-R, -IS, -IP, -IE, -IV, +OS, +OP, +OE, +OV] {
////  final def run(r: R, init: IS): (OS, Either[OE, OV])  = Calc.run(this, r, init)
////  final def runUnit(init: IS)(implicit ev: Unit <:< R) = run((), init)
//
//  def join[R1 <: R, XS, XP, XE, XV](
//      calc: Calc[R1, OS, OP, OE, OV, XS, XP, XE, XV]): Calc[R1, IS, IP, IE, IV, XS, XP, XE, XV] = Join(this, calc)
//}
//
//object Calc {
//  def pure[S, OV](a: OV): Calc[Any, S, Any, Any, Any, S, Nothing, Nothing, OV] = Pure(a)
//  def read[S, R]: Calc[R, S, Any, Any, Any, S, Nothing, Nothing, R]            = Read()
//  def get[S]: Calc[Any, S, Any, Any, Any, S, Nothing, Nothing, S]              = Get()
//  def set[S](s: S): Calc[Any, Any, Any, Any, Any, S, Nothing, Nothing, Unit]   = Set(s)
//  def update[IS, OS](f: IS => OS): Calc[Any, IS, Any, Any, Any, OS, Nothing, Nothing, Unit] =
//    get[IS].flatMapS(s => set(f(s)))
//  def raise[S, OE](e: OE): Calc[Any, S, Any, Any, Any, S, Nothing, OE, Nothing] = Raise(e)
//  def defer[R, IS, IP, IE, IV, OS, OP, OE, OV](
//      x: => Calc[R, IS, IP, IE, IV, OS, OP, OE, OV]): Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] = Defer(() => x)
//  def delay[S, OV](x: => OV): Calc[Any, S, Any, Any, Any, S, Nothing, Nothing, OV]            = defer(pure(x))
//
//  def write[S](s: S)(implicit S: Monoid[S]): Calc[Any, S, Any, Any, Any, S, Nothing, Nothing, Unit] =
//    update(S.combine(_, s))
//
//  sealed trait CalcRes[-R, -IS, -IP, -IE, -IV, +OS, +OP, +OE, +OV] extends Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] {
//    def submit[X](r: R, s: IS, ke: (OS, OE) => X, ka: (OS, OV) => X): X
//  }
//  final case class Pure[S, +OV](a: OV) extends CalcRes[Any, S, Any, Any, Any, S, Nothing, Nothing, OV] {
//    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, OV) => X): X = ka(s, a)
//  }
//  final case class Read[S, R]() extends CalcRes[R, S, Any, Any, Any, S, Nothing, Nothing, R] {
//    def submit[X](r: R, s: S, ke: (S, Nothing) => X, ka: (S, R) => X): X = ka(s, r)
//  }
//  final case class Get[S]() extends CalcRes[Any, S, Any, Any, Any, S, Nothing, Nothing, S] {
//    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, S) => X): X = ka(s, s)
//  }
//  final case class Set[S](s: S) extends CalcRes[Any, Any, Any, Any, Any, S, Nothing, Nothing, Unit] {
//    def submit[X](r: Any, s1: Any, ke: (S, Nothing) => X, ka: (S, Unit) => X): X = ka(s, ())
//  }
//  final case class Raise[S, OE](e: OE) extends CalcRes[Any, S, Any, Any, Any, S, Nothing, OE, Nothing] {
//    def submit[X](r: Any, s: S, ke: (S, OE) => X, ka: (S, Nothing) => X): X = ke(s, e)
//  }
//  final case class Produce[-R, -IS, -IP, -IE, -IA, +OS, +OP, +OE, +OV](p: OP,
//                                                                       next: Calc[R, IS, IP, IE, IA, OS, OP, OE, OV])
//      extends Calc[R, IS, IP, IE, IA, OS, OP, OE, OV]
//
//  final case class Defer[R, IS, IP, IE, IV, OS, OP, OE, OV](e: () => Calc[R, IS, IP, IE, IV, OS, OP, OE, OV])
//      extends Calc[R, IS, IP, IE, IV, OS, OP, OE, OV]
//
//  trait Consume[-R, -IS, -IP, -IE, -IV, +OS, +OP, +OE, +OV] extends Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] {
//    def consume(p: IP): Calc[R, IS, IP, IE, IV, OS, OP, OE, OV]
//    def suc(s: IS, a: IV): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV]
//    def err(s: IS, e: IE): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV]
//
//    override def join[R1 <: R, XS, XP, XE, XV](
//        calc: Calc[R1, OS, OP, OE, OV, XS, XP, XE, XV]): Calc[R1, IS, IP, IE, IV, XS, XP, XE, XV] =
//      JoinConsume(this, calc)
//  }
//
//  trait Continue[-R, -IS, -IP, -IE, -IV, +OS, +OP, +OE, +OV] extends Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] {
//    def suc(s: IS, a: IV): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV]
//    def err(s: IS, e: IE): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV]
//
//    override def join[R1 <: R, XS, XP, XE, XV](
//      calc: Calc[R1, OS, OP, OE, OV, XS, XP, XE, XV]): Calc[R1, IS, IP, IE, IV, XS, XP, XE, XV] =
//      JoinConsume(this, calc)
//  }
//
//  final private case class Result[E, V, S]() extends Consume[Any, S, Any, E, V, S, Nothing, E, V] {
//    def consume(p: Any): Result[E, V, S]                               = this
//    def suc(s: S, a: V): Calc[Any, S, Any, Any, Any, S, Nothing, E, V] = Pure(a)
//    def err(s: S, e: E): Calc[Any, S, Any, Any, Any, S, Nothing, E, V] = Raise(e)
//  }
//
//  final case class Cont[-R, -IS, -IE, -IV, +OS, P, +OE, +OV](
//      fsuc: IV => Calc[R, IS, Any, Any, Any, OS, P, OE, OV],
//      ffail: IE => Calc[R, IS, Any, Any, Any, OS, P, OE, OV]
//  ) extends Consume[R, IS, P, IE, IV, OS, P, OE, OV] {
//    def consume(p: P): Calc[R, IS, P, IE, IV, OS, P, OE, OV]         = Produce(p, this)
//    def suc(s: IS, a: IV): Calc[R, IS, Any, Any, Any, OS, P, OE, OV] = fsuc(a)
//    def err(s: IS, e: IE): Calc[R, IS, Any, Any, Any, OS, P, OE, OV] = ffail(e)
//  }
//
//  final case class Continue[-R, IS, -IP, -IE, -IV, MS, MP, ME, MV, +OS, +OP, +OE, +OV](
//    first: Calc[R, IS, IP, IE, IV, MS, MP, ME, MV],
//    next: Consume[R, MS, Nothing, ME, MV, OS, OP, OE, OV]
//  ) extends Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] {
//    type MidState = MS
//    type MidErr   = ME
//    type MidVal   = MV
//    type MidPoint = MP
//  }
//
//  final case class Connect[-R, S, -IP, -IE, -IV,  MP, ME, MV,  +OP, +OE, +OV](
//      feed: Calc[R, S, IP, IE, IV, S, MP, ME, MV],
//      focus: Calc[R, S, MP, ME, MV, S, OP, OE, OV]
//  ) extends Calc[R, S, IP, IE, IV, S, OP, OE, OV] {
//    type MidErr   = ME
//    type MidVal   = MV
//    type MidPoint = MP
//  }
//
//  final case class JoinConsume[-R, IS, -IP, -IE, -IV, MP, ME, MV, MS, +OS, +OP, +OE, +OV](
//      first: Consume[R, IS, IP, IE, IV, MS, MP, ME, MV],
//      next: Calc[R, MS, MP, ME, MV, OS, OP, OE, OV]
//  ) extends Consume[R, IS, IP, IE, IV, OS, OP, OE, OV] {
//    type MidState = MS
//    type MidErr   = ME
//    type MidVal   = MV
//    type MidPoint = MP
//
//    def consume(p: IP): Calc[R, IS, IP, IE, IV, OS, OP, OE, OV]       = first.consume(p).join(next)
//    def suc(s: IS, a: IV): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV] = first.suc(s, a).join(next)
//    def err(s: IS, e: IE): Calc[R, IS, Any, Any, Any, OS, OP, OE, OV] = first.err(s, e).join(next)
//  }
//
//  implicit class invariantOps[R, IS, IP, IE, IV, OS, OP, OE, OV](val calc: Calc[R, IS, IP, IE, IV, OS, OP, OE, OV])
//      extends AnyVal {
//    def cont[E, S, V](
//        f: OV => Calc[R, OS, Any, Any, Any, S, OP, E, V],
//        h: OE => Calc[R, OS, Any, Any, Any, S, OP, E, V]
//    ): Calc[R, IS, IP, IE, IV, S, OP, E, V] = Join(calc, Cont(f, h))
//    def flatMap[B](f: OV => Calc[R, OS, Any, Any, Any, OS, OP, OE, B]): Calc[R, IS, IP, IE, IV, OS, OP, OE, B] =
//      cont(f, raise(_: OE))
//    def handleWith(f: OE => Calc[R, OS, Any, Any, Any, OS, OP, OE, OV]): Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] =
//      cont(pure(_: OV), f)
//    def handle(f: OE => OV): Calc[R, IS, IP, IE, IV, OS, OP, OE, OV] = handleWith(e => pure(f(e)))
//    def map[B](f: OV => B): Calc[R, IS, IP, IE, IV, OS, OP, OE, B]   = flatMap(a => pure(f(a)))
//  }
//
//  implicit class successfullOps[R, IS, IP, IE, IV, OS, OP, OE, OV](
//      val calc: Calc[R, IS, IP, IE, IV, OS, OP, Nothing, OV])
//      extends AnyVal {
//    def flatMapS[S, V, E](f: OV => Calc[R, OS, Any, Any, Any, S, OP, E, V]): Calc[R, IS, IP, IE, IV, S, OP, E, V] =
//      calc.cont(f, (void: Nothing) => void)
//  }
//
//  def infifeed[S]: Calc[Any, S, Any, Any, Any, S, Any, Nothing, Nothing] = {
//    lazy val p: Calc[Any, S, Any, Any, Any, S, Any, Nothing, Nothing] = Produce((), defer(p))
//    p
//  }
//
//  def run[R, IS, IP, IE, IV, XS, YS, OS, OP, OE, OV](calc: Calc[R, XS, IP, IE, IV, YS, OP, OE, OV],
//                                                     feed: Calc[R, IS, Any, Any, Any, IS, IP, IE, IV],
//                                                     cont: Consume[R, OS, OP, OE, OV, OS, Nothing, OE, OV],
//                                                     r: R,
//                                                     init: IS): (OS, Either[OE, OV]) =
//    calc match {
//      case res: CalcRes[R, IS, IP, IE, IV, OS, OP, OE, OV] =>
//        res.submit(
//          r,
//          init,
//          (s, e) => s -> cont.err(s, e),
//          (s, a) => s -> cont.suc(s, a)
//        ) match {
//          case (s, res: CalcRes[R, OS, OP, OE, OV, OS, Nothing, OE, OV]) =>
//            res.submit(r, s, (s, e) => s -> Left(e), (s, res) => s -> Right(res))
//          case (s, JoinConsume(calc1, next)) => run(calc1, infifeed[OS], next, r, s)
//          case (s, calc1)                    => run(calc1, infifeed[OS], Result(), r, s)
//        }
//
//      case Defer(f)        => run(f(), feed, cont, r, init)
//      case Connect(src, next) => run(next, feed.join(src), cont, r, init)
//    }
//
////  implicit def calcInstance[R, I, S, OP, OE]: CalcFunctorInstance[R, I, S, OP, OE] =
////    new CalcFunctorInstance[R, I, S, OP, OE]
////
////  class CalcFunctorInstance[R, I, S, OP, OE]
////      extends MonadError[Calc[R, I, S, S, OP, OE, ?], OE] with cats.Defer[Calc[R, I, S, S, OP, OE, ?]]
////      with StackSafeMonad[Calc[R, I, S, S, OP, OE, ?]] {
////    def defer[OV](fa: => Calc[R, I, S, S, OP, OE, OV]): Calc[R, I, S, S, OP, OE, OV] = Calc.defer(fa)
////    def raiseError[OV](e: OE): Calc[R, I, S, S, OP, OE, OV]                          = Calc.raise(e)
////    def handleErrorWith[OV](fa: Calc[R, I, S, S, OP, OE, OV])(
////        f: OE => Calc[R, I, S, S, OP, OE, OV]): Calc[R, I, S, S, OP, OE, OV] = fa.handleWith(f)
////    def flatMap[OV, B](fa: Calc[R, I, S, S, OP, OE, OV])(
////        f: OV => Calc[R, I, S, S, OP, OE, B]): Calc[R, I, S, S, OP, OE, B] =
////      fa.flatMap(f)
////    def pure[OV](x: OV): Calc[R, I, S, S, OP, OE, OV] = Calc.pure(x)
////  }
//}
