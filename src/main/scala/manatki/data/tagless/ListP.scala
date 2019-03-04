package manatki.data.tagless
import cats.arrow.Profunctor

trait ListP[I, A, B] {
  def nil: B
  def cons(head: I, tail: A): B
}

object ListP {
  implicit def profunctor[I]: Layered[ListP[I, ?, ?]] = new Layered[ListP[I, ?, ?]] {
    def tagless[A, C](f: L[A] => C): ListP[I, A, C] =
      new ListP[I, A, C] {
        def nil: C =
          f(new L[A] { def cont[B](pa: ListP[I, A, B]): B = pa.nil })
        def cons(head: I, tail: A): C =
          f(new L[A] { def cont[B](pa: ListP[I, A, B]): B = pa.cons(head, tail) })
      }

    def dimap[A, B, C, D](fab: ListP[I, A, B])(f: C => A)(g: B => D): ListP[I, C, D] =
      new ListP[I, C, D] {
        def nil: D                    = g(fab.nil)
        def cons(head: I, tail: C): D = g(fab.cons(head, f(tail)))
      }

    override def zip[A, B, C, D](pab: ListP[I, A, B], pcd: ListP[I, C, D]): ListP[I, (A, C), (B, D)] =
      new ListP[I, (A, C), (B, D)] {
        def nil: (B, D)                         = (pab.nil, pcd.nil)
        def cons(head: I, tail: (A, C)): (B, D) = (pab.cons(head, tail._1), pcd.cons(head, tail._2))
      }
  }
}
