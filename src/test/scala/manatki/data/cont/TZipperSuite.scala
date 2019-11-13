package manatki.data.cont

import cats.Traverse
import cats.data.Writer
import cats.effect.{IO, Sync}
import org.scalatest.FlatSpec
import cats.instances.list._
import cats.syntax.foldable._

class TZipperSuite extends FlatSpec {


  val list = List.range(0, 10)
  //  val replacements = List(None, None, Some(-2), None, Some(-4))

  def replaceWith[F[_] : Traverse, A](source: F[A], replacements: List[Option[A]]): (List[A], F[A]) =
    replacements.foldLeftM[Writer[List[A], *], TZipper[F, A]](TZipper(source)) {
      case (TZipper.Done(res), _) => Writer.value(TZipper.Done(res))
      case (TZipper.Cursor(cur, f), repl) => Writer(List(cur), f(repl))
    }.map(_.zipUp).run

  "list replacement" should "replace nothing in empty List" in
    {
      val (seen, modified) = replaceWith(List.empty[Int], List(Some(1), None))
      assert(seen === List.empty)
      assert(modified === List.empty)
    }

  it should "see elements" in {
    val original = List.range(0, 5)
    val (seen, modified) = replaceWith(original, List(None, None, None))
    assert(seen === List(0, 1, 2))
    assert(modified === original)
  }

  it should "modify elements" in {
    val original = List.range(0, 5)
    val (seen, modified) = replaceWith(original, List(Some(-1), None, Some(-2)))
    assert(seen === List(0, 1, 2))
    assert(modified === List(-1, 1, -2, 3, 4))
  }

}
