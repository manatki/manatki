trait AnimalType[A]
case object CatType extends AnimalType[Cat]
case object DogType extends AnimalType[Dog]

def compareAnimalType[A, B](at: AnimalType[A], bt: AnimalType[B]): Option[A =:= B] = (at, bt) match {
  case (CatType, CatType) => Some(implicitly[A =:= B])
  case (DogType, DogType) => Some(implicitly[A =:= B])
  case _                  => None
}

trait Animal[A] {
  self: A =>
  def data: A = self
  def animalType: AnimalType[A]
}

case class Cat(catProperty: String) extends Animal[Cat] {
  def animalType: AnimalType[Cat] = CatType
}

case class Dog(dogProperty: String) extends Animal[Dog] {
  def animalType: AnimalType[Dog] = DogType
}

trait AdoptionCenter[A] {
  def animalType: AnimalType[A]
  def adopt(animal: Animal[A]): Unit

  def forAnimalType[B](t: AnimalType[B]): Option[AdoptionCenter[B]] =
    compareAnimalType(animalType, t).map(eq => eq.liftCo[AdoptionCenter].apply(this))
}

class DogAdoptionCenter extends AdoptionCenter[Dog] {
  override def animalType: AnimalType[Dog]      = DogType
  override def adopt(animal: Animal[Dog]): Unit = println(
    s"dog with property:${animal.data.dogProperty}"
  )

}
class CatAdoptionCenter extends AdoptionCenter[Cat] {
  override def animalType: AnimalType[Cat]      = CatType
  override def adopt(animal: Animal[Cat]): Unit =
    println(s"cat with property:${animal.data.catProperty}")

}

class AdoptionCenterService(adoptionServices: Seq[AdoptionCenter[?]]) {
  private def findFor[A](t: AnimalType[A]): Option[AdoptionCenter[A]] =
    adoptionServices.view.flatMap(_.forAnimalType(t)).headOption

  private def adoptAnimal[A](animal: Animal[A]) =
    findFor(animal.animalType).foreach(_.adopt(animal))

  def adoptAll(animals: Animal[?]*) = animals.foreach(a => adoptAnimal(a))
}

val sdac = new DogAdoptionCenter()
val scac = new CatAdoptionCenter()
val acs  = new AdoptionCenterService(Seq(sdac, scac))

acs.adoptAll(new Dog("ushi"), new Cat("hvost"))

class AdoptionCenterController(service: AdoptionCenterService) {
  def endpointLogic() = service.adoptAll(new Dog("ushi"), new Cat("hvost"))
}

val controller = new AdoptionCenterController(acs)
controller.endpointLogic()
