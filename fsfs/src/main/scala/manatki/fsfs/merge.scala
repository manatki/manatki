package manatki.fsfs
import cats.Order
import fs2.{Chunk, Stream, Pull}
import cats.syntax.order._

import scala.collection.mutable

object merge {

  //discussed with @lmnet89 https://t.me/scala_ru/234449 8.08.2019 10:09
  def ordered[F[_], A, R: Order](f: A => R)(xs: Stream[F, A], ys: Stream[F, A]): Stream[F, A] = {
    val pull: Pull[F, A, Any] = xs.pull.uncons.flatMap {
      case Some((xc, xt)) =>
        ys.pull.uncons.flatMap {
          case None => (Stream.chunk(xc) ++ xt).pull.echo
          case Some((yc, yt)) =>
            val (next, rest, left) = mergeChunks(xc, yc, f)
            val rc                 = Stream.chunk(rest)
            val tail               = if (left) ordered(f)(xt, rc ++ yt) else ordered(f)(rc ++ xt, yt)
            (Stream.chunk(next) ++ tail).pull.echo
        }
      case None => ys.pull.echo
    }
    pull.stream
  }

  private def mergeChunks[A, R: Order](as: Chunk[A], bs: Chunk[A], f: A => R): (Chunk[A], Chunk[A], Boolean) = {
    val buffer = mutable.Buffer[A]()
    def go(i: Int, j: Int): (Int, Boolean) =
      if (as.size <= i) (j, true)
      else if (bs.size <= j) (i, false)
      else if (f(as(i)) < f(bs(j))) {
        buffer += as(i)
        go(i + 1, j)
      } else {
        buffer += bs(j)
        go(i, j + 1)
      }
    val (i, left) = go(0, 0)
    val produce   = Chunk.buffer(buffer)
    val rest      = (if (left) bs else as).drop(i)
    (produce, rest, left)
  }

}
