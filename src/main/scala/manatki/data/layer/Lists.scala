package manatki.data.layer

trait Single[-A, +O] {
  def single(a: A): O
}

trait Empty[+O] {
  def empty: O
}

trait Cons[-A, -I, +O] {
  def cons(a: A, y: I): O
}

trait OptP[-A, +O]      extends Single[A, O] with Empty[O]
trait NelP[-A, -I, +O]  extends Single[A, O] with Cons[A, I, O]
trait ListP[-A, -I, +O] extends Cons[A, I, O] with Empty[O]

object Lists {

//  implicit def layered[E]: Layered[ListP[E, -?, +?]] =
//    new Layered[ListP[E, -?, +?]] {
//      override type Lz[-A, +B] = ListP[E, Eval[A], Eval[B]]
//      val layer: LayerConst[ListP[E, -?, +?]] = new LayerConst[ListP[E, -?, +?]] {
//        def empty: Layer[ListP[E, -?, +?]] = new Layer[ListP[E, -?, +?]] {
//          def unpack[A](p: ListP[E, Layer[ListP[E, -?, +?]], A]): A = p.empty
//        }
//        def cons(a: E, y: Layer[ListP[E, -?, +?]]): Layer[ListP[E, -?, +?]] = new Layer[ListP[E, -?, +?]] {
//          def unpack[A](p: ListP[E, Layer[ListP[E, -?, +?]], A]): A = p.cons(a, y)
//        }
//      }
//      val lzLayer: LayerConst[Lz]                                                      =
//        new LayerConst[Lz]{
//          val empty: Eval[Layer[Lz]] = Eval.now(new Layer[Lz] {
//            def unpack[A](p: ListP[E, Eval[Layer[Lz]], Eval[A]]): Eval[A] = p.empty
//          })
//          def cons(a: E, y: Eval[Layer[Lz]]): Eval[Layer[Lz]] = ???
//        }
//      def capture: RecursiveConst[ListP[E, -?, +?]]                                    = ???
//      def lzCapture: RecursiveConst[Lz]                                                = ???
//      def dimap[A, B, C, D](fab: ListP[E, A, B])(f: C => A)(g: B => D): ListP[E, C, D] = ???
//    }

}
