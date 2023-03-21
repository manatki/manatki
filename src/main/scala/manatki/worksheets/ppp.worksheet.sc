import shapeless._
import shapeless.ops.coproduct._
import shapeless.syntax.CoproductOps

abstract class Animal

final class Dog extends Animal{
  def woof = "woof"
}
final class Cat extends Animal{
  def meow = "meow"
}
final class Cow extends Animal{
  def moo = "moo"
}

type Pet = Dog :+: Cat :+: Cow :+: CNil


val toPet = {
  val inject = implicitly[RuntimeInject[Pet]]
  (inject.apply(_: Animal).get)
}

object goodCheck extends Poly1{
  implicit val whenDog = at[Dog](_.woof)
  implicit val whenCat = at[Cat](_.meow)
  implicit val whenCow = at[Cow](_.moo)
}

def goodCheckAnimal(animal: Animal) = toPet(animal).fold(goodCheck)

goodCheckAnimal(new Dog)

object badCheck extends Poly1{
    implicit val whenDog = at[Dog](_.woof)
    implicit val whenCat = at[Cat](_.meow)
}

// def badCheckAnimal(animal: Animal) = toPet(animal).fold(badCheck)