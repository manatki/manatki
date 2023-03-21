import shapeless.ops.hlist
import scala.reflect.ClassTag
import shapeless._
import shapeless.ops._

abstract class Animal

final class Dog extends Animal {
  def woof = "woof"
}
final class Cat extends Animal {
  def meow = "meow"
}
final class Cow extends Animal {
  def moo = "moo"
}

sealed trait PetTag {
  type T <: Animal
  def tagT: PetTagT[T]
}
sealed trait PetTagT[C <: Animal] extends PetTag {
  type T = C
  def tagT: PetTagT[T] = this
}

object PetTag {
  implicit case object Dog extends PetTagT[Dog]
  implicit case object Cat extends PetTagT[Cat]
  implicit case object Cow extends PetTagT[Cow]
}

case class Tagged[A <: Animal](x: A, tag: PetTagT[A])

trait PetTagFinder[C] {
  type T <: Animal
  def tag: PartialFunction[Animal, Tagged[T]]
  def erasedTag: PartialFunction[Animal, Tagged[_ <: Animal]] = tag
}
trait PetTagFinderT[C, A <: Animal] extends PetTagFinder[C] { type T = A }

implicit def findTag[C <: PetTag, A <: Animal](implicit C: C { type T = A }, ct: ClassTag[A]): PetTagFinderT[C, A] =
  new PetTagFinderT[C, A] {

    val tag = { case k: A => Tagged[A](k, C.tagT) }
  }

implicitly[PetTagFinder[PetTag.Dog.type]]

val petTagGen  = Generic[PetTag]
val petTagList = coproduct.ToHList[petTagGen.Repr]
val toPet      = hlist
  .LiftAll[PetTagFinder, petTagList.Out]
  .instances
  .toList
  .map(_.erasedTag)
  .reduce(_ orElse _)

def sayPet[A <: Animal](a: Tagged[A]) = a match {
  case Tagged(cat, PetTag.Cat) => cat.meow
  case Tagged(dog, PetTag.Dog) => dog.woof
//   case Tagged(cow, PetTag.Cow) => cow.moo
}

sayPet(toPet(new Dog))
