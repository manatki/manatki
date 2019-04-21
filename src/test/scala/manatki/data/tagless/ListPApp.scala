package manatki.data.tagless
import cats.syntax.all._

import cats.effect.{ExitCode, IO, IOApp}
import supertagged._

object ListPApp extends IOApp {
  def list[B](implicit list: ListP[Long, B, B]): B = ListP.range(1, 5686)

  def fold[B](implicit B: Integral[B]) = {
    val lst = List.range(B.one, B.fromInt(5686))
    def go(lst: List[B]): B = lst match {
      case Nil => B.zero
      case h :: t => B.plus(h, go(t))
    }
    go(lst)
  }

  def run(args: List[String]): IO[ExitCode] =
    IO(println(list[Long @@ SumT])) *>
    IO(println(fold[Long])) *>
      IO(ExitCode.Success)

}
