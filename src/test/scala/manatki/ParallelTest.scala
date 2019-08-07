package manatki
import cats.{ApplicativeError, MonadError}
import cats.data.NonEmptyList
import org.scalatest.Matchers
import tofu.syntax.paralleled._
import tofu.syntax.splitting._
import cats.syntax.either._
import cats.instances.either._
import cats.instances.parallel._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.monadError._
import org.scalatest.WordSpec
import tofu.parallel.Paralleled

class ParallelTest extends WordSpec with Matchers {

  type Err = NonEmptyList[String]
  type Res[a] = Either[Err, a]

  def addPair[F[_] : Paralleled](x: F[Int], y: F[Int]): F[Int] = x.parMap2(y)(_ + _)
  def verifyAllEvens[F[_] : Paralleled : MonadError[?[_], Err]](xs: List[Int]): F[Unit] =
    xs.parTraverse_[F, Int](x => x.pure[F].ensure(NonEmptyList.of(x.toString))(_ % 2 == 0))

  "parallel either" when {
    "using syntax extensions" should {
      "collect pair of errors" in {
        ("Lol".leftNel: Res[Int]).parMap2("Kek".leftNel)(_ + _) shouldBe NonEmptyList.of("Lol", "Kek").asLeft
      }
      "collect list of errors" in {
        List.range(1, 10)
          .parTraverse[Res, Int](x => if (x % 2 == 0) x.rightNel else x.toString.leftNel) shouldBe
          NonEmptyList.of("1", "3", "5", "7", "9").asLeft
      }
    }
    "using polymorphic methods" should {
      "collect pair of errors" in {
        addPair[Res]("Lol".leftNel, "Kek".leftNel) shouldBe NonEmptyList.of("Lol", "Kek").asLeft
      }
      "collect list of errors" in {
        verifyAllEvens[Res](List.range(1, 10)) shouldBe
          NonEmptyList.of("1", "3", "5", "7", "9").asLeft
      }
    }
  }
}
