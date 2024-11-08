import shapeless._

// independent ADT
sealed trait Animal
object Animal {
  case object Cat   extends Animal
  case object Dog   extends Animal
  case object Horse extends Animal
}

// independent ADT
sealed trait Fruit
object Fruit {
  case object Banana extends Fruit
  case object Apple  extends Fruit
}

// grouping different entities into few categories
sealed trait Goods[+T]
object Goods {
  case class AnimalCategory[T <: Animal with Singleton](animal: T) extends Goods[T]
  case class FruitCategory[T <: Fruit with Singleton](fruit: T)    extends Goods[T]
  case object Other                                                extends Goods[Nothing]
}

sealed abstract class Box[+T](val category: Goods[T])
sealed abstract class BoxOf[+T <: Singleton](category: Goods[T]) extends Box[T](category)
case object CatBox                                               extends BoxOf(category = Goods.AnimalCategory(Animal.Cat))
case object DogBox                                               extends BoxOf(category = Goods.AnimalCategory(Animal.Dog))
case object BananaBox                                            extends BoxOf(category = Goods.FruitCategory(Fruit.Banana))
case object AppleBox                                             extends BoxOf(category = Goods.FruitCategory(Fruit.Apple))
// ERROR: no box for AnimalCategory(Animal.Horse)

trait Unbox[C <: Coproduct, +Q] extends DepFn0 {
  type Out <: HList
}

trait UnboxLP {
  type Aux[C <: Coproduct, +Q, O <: HList] = Unbox[C, Q] { type Out = O }
  trait Impl[C <: Coproduct, +Q, O <: HList] extends Unbox[C, Q] { type Out = O }
  protected def impl[C <: Coproduct, Q, O <: HList](x: => O): Impl[C, Q, O] = () => x

  implicit def caseOther[I, Q, C <: Coproduct](implicit
      tail: Unbox[C, Q]
  ): Aux[I :+: C, Q, tail.Out] = impl(tail())
}
object Unbox extends UnboxLP {
  implicit def caseAnimal[Q, I, A <: Q, C <: Coproduct](implicit
      sub: I <:< Box[A],
      tail: Unbox[C, Q],
      w: Witness.Aux[A]
  ): Aux[I :+: C, Q, A :: tail.Out] = impl(w.value :: tail())

  implicit val caseCNil: Aux[CNil, Nothing, HNil]                              = impl(HNil)
  def apply[C <: Coproduct, Q](implicit unbox: Unbox[C, Q]): Aux[C, Q, unbox.Out] = unbox
}

val boxes        = Generic[Box[Any]]
val animals      = Generic[Animal]
val boxesL       = ops.coproduct.ToHList[boxes.Repr]
val boxesAnimalL = Unbox[boxes.Repr, Animal]
val animalsL     = ops.coproduct.ToHList[animals.Repr]
// boxesAnimalL
val uncovered    = ops.hlist.Diff[animalsL.Out, boxesAnimalL.Out]
val remains      = ops.hlist.Reify[uncovered.Out].apply()
// shapeless.Witness[uncovered.Out]
