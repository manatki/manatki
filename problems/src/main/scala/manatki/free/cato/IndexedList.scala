package manatki.free.cato

import scala.annotation.tailrec
import scala.util.chaining._

object Example {
  // "колчан" - события с типами состояний в качестве индексов
  // final обязательны, иначе скала тупит и принимает в патмате что-угодно

  type StepInfo[Step] <: String
  object StepInfo {
    def apply[Step](data: String): StepInfo[Step] = data.asInstanceOf[StepInfo[Step]]
  }

  sealed trait Evt[From, To]                                               extends Product with Serializable
  final case class GoToA[X](info: StepInfo["Start"], next: Evt["A", X])    extends Evt["Start", X]
  final case class GoToB[X](info: StepInfo["Continue"], next: Evt["B", X]) extends Evt["A", X]
  final case class LoopB[X](info: StepInfo["Loop"], next: Evt["B", X])     extends Evt["B", X]
  final case class GoToC[X](info: StepInfo["SwitchC"], next: Evt["C", X])  extends Evt["B", X]
  final case class GoToD[X](info: StepInfo["SwitchD"], next: Evt["D", X])  extends Evt["B", X]
  final case class FromC(info: StepInfo["EndC"])                           extends Evt["C", "End"]
  final case class FromD(info: StepInfo["EndD"])                           extends Evt["D", "End"]

  // представим, что какую-то такую информацию мы хотим собрать
  case class CollectedInfo(
      start: StepInfo["Start"],
      continue: StepInfo["Continue"],
      loop: Vector[StepInfo["Loop"]],
      corD: Either[(StepInfo["SwitchC"], StepInfo["EndC"]), (StepInfo["SwitchD"], StepInfo["EndD"])],
  )

  def collectInfo(chain: Evt["Start", "End"]): CollectedInfo =
    chain match {
      case GoToA(infoA, GoToB(infoB, next)) =>
        @tailrec def loop(chain: Evt["B", "End"], acc: Vector[StepInfo["Loop"]] = Vector.empty): CollectedInfo =
          chain match {
            case LoopB(bl, next)          => loop(next, acc :+ bl)
            case GoToC(infoC, FromC(end)) => CollectedInfo(infoA, infoB, acc, Left(infoC -> end))
            case GoToD(infoD, FromD(end)) => CollectedInfo(infoA, infoB, acc, Right(infoD -> end))
          }
        loop(next)
    }

  val chain1 =
    StepInfo["EndC"]("end")
      .pipe(FromC(_))
      .pipe(GoToC(StepInfo("switch"), _))
      .pipe(LoopB(StepInfo("loop2"), _))
      .pipe(LoopB(StepInfo("loop1"), _))
      .pipe(GoToB(StepInfo("continue"), _))
      .pipe(GoToA(StepInfo("start"), _))

  def main(args: Array[String]): Unit = {
    println(collectInfo(chain1))
  }
}

object Fix {
  type IL[E[_, _, +_], From, To]

  def wrap[E[_, _, +_], From, Mid, To](evt: E[From, Mid, IL[E, Mid, To]]): IL[E, From, To] =
    evt.asInstanceOf[IL[E, From, To]]

  implicit class Ops[E[_, _, +_], From, To](private val self: IL[E, From, To]) extends AnyVal {
    def unwrap: E[From, Mid, IL[E, Mid, To]] forSome { type Mid } =
      self.asInstanceOf[E[From, Mid, IL[E, Mid, To]] forSome { type Mid }]
  }
}

object Example2{
  import Example.StepInfo

  sealed trait Evt[From, To, +X]                                  extends Product with Serializable
  final case class GoToA[+X](info: StepInfo["Start"], next: X)    extends Evt["Start", "A", X]
  final case class GoToB[+X](info: StepInfo["Continue"], next: X) extends Evt["A", "B", X]
  final case class LoopB[+X](info: StepInfo["Loop"], next: X)     extends Evt["B", "B", X]
  final case class GoToC[+X](info: StepInfo["SwitchC"], next: X)  extends Evt["B", "C", X]
  final case class GoToD[+X](info: StepInfo["SwitchD"], next: X)  extends Evt["B", "D", X]
  final case class FromC(info: StepInfo["EndC"])                  extends Evt["C", "End", Nothing]
  final case class FromD(info: StepInfo["EndD"])                  extends Evt["D", "End", Nothing]
}
