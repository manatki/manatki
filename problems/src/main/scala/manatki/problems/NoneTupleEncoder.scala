package manatki.problems

import shapeless._
import shapeless.ops.tuple._
import scala.annotation.implicitNotFound

trait IsOptions[L <: HList] {
  def nones: L
}
object IsOptions            {
  implicit val hnilIsOptions: IsOptions[HNil] = new IsOptions[HNil] {
    def nones = HNil
  }

  implicit def hlistIsOptions[H, T <: HList](implicit tail: IsOptions[T]): IsOptions[Option[H] :: T] =
    new IsOptions[Option[H] :: T] {
      def nones = None :: tail.nones
    }
}

trait Encoder[A] {
  type Persist
  def encode(a: A): Persist
}
object Encoder   {
  type Aux[A, P] = Encoder[A] { type Persist = P }

  implicit def methods[A, P](enc: Encoder.Aux[A, P]): EncoderPartiallyApplied[A, P] =
    new EncoderPartiallyApplied[A, P](enc)
}

final class EncoderPartiallyApplied[A, P](private val encoder: Encoder.Aux[A, P]) extends AnyVal {
  def options[L <: HList](implicit
      @implicitNotFound("Your Persist type must be a tuple of Options, e.g. (Option[Identifier], Option[Instant])")
      isTuple: IsTuple[P],
      persistAsHList: Generic.Aux[P, L],
      @implicitNotFound("All fields of your Persist type must be Options")
      isOptions: IsOptions[L],
  ): Encoder.Aux[Option[A], Option[P]] =
    new Encoder[Option[A]] {
      type Persist = Option[P]

      override def encode(a: Option[A]): Persist = a match {
        case Some(value) => Some(encoder.encode(value))
        case None        => Some(persistAsHList.from(isOptions.nones))
      }
    }
}

object noneTupleEncoder {

  def main(args: Array[String]): Unit = {
    val xx = new Encoder[String] {
      type Persist = (Option[String], Option[String])
      def encode(a: String): Persist = a match {
        case s"$a $b" => (Some(a), Some(b))
        case _        => (None, None)
      }
    }.options

    println(xx.encode(Some("Anime Govno")))
    println(xx.encode(None))
  }
}
