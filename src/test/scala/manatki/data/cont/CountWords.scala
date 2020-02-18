package manatki.data.cont

import cats.Eval.now
import cats.data.{ContT, ReaderT, State}
import cats.effect.{ExitCode, IO, IOApp}
import cats.mtl.MonadState
import cats.mtl.instances.local._
import cats.mtl.instances.state._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Eval, Monad}
import manatki.data.MonoStr
import manatki.data.cont.contState._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.Function.const
import scala.util.Random

object CountWords {
  sealed trait St {
    def words: Map[String, Int]
  }
  final case class Space(words: Map[String, Int])                  extends St
  final case class Word(words: Map[String, Int], coll: List[Char]) extends St

  def consume[F[_]](char: Char)(implicit F: MonadState[F, St]): F[Unit] =
    F.monad.flatMap(F.get) {
      case Space(words) if char.isLetter      => F.set(Word(words, List(char)))
      case Space(_)                           => F.monad.unit
      case Word(words, coll) if char.isLetter => F.set(Word(words, char :: coll))
      case Word(words, coll) =>
        val name = coll.reverse.mkString
        F.set(Space(words + (name -> (words.getOrElse(name, 0) + 1))))
    }

  def getString() = {
    val parts = "lol kek yuu cheburek tii tot ryu".split(' ')
    Iterator
      .continually(parts(Random.nextInt(parts.length)))
      .zip(Iterator.continually(" " * (1 + Random.nextInt(6))))
      .flatMap { case (w, s) => Iterator(w, s) }
      .take(10000)
      .mkString
  }

  val start = Space(Map())

  def countWordsM[F[_]: Monad: MonadState[*[_], St]](str: String): F[Unit] =
    MonoStr(str).traverse_(consume[F])

  def countWordsState(str: String): Map[String, Int] =
    countWordsM[State[St, *]](str).runS(start).value.words

  def countWordsContT(str: String): Map[String, Int] =
    countWordsM[ContT[ReaderT[Eval, St, *], Map[String, Int], *]](str)
      .run(_ => ReaderT(st => now(st.words)))
      .run(start)
      .value

  def countWordsCont(str: String): Map[String, Int] =
    countWordsM[ContE.State[Map[String, Int], St, *]](str)
      .run(_ => now(st => now(st.words)))
      .value(start)
      .value

  def countWordsConto(str: String): Map[String, Int] =
    countWordsM[Cont.State[Map[String, Int], St, *]](str)
      .run(_ => st => Cont.Pure(st.words))(start)
      .evalue


  def run(args: List[String]): IO[ExitCode] =
    IO(println(countWordsConto(getString))) *>
      IO(println(countWordsState(getString))) *>
      IO(println(countWordsCont(getString))) *>
//      IO(println(countWordsContT(getString))) *> // SO
      IO(ExitCode.Success)
}

class CountWords extends AnyFlatSpec with Matchers {
  import CountWords._
  val string = getString()

  val correct = string.split("\\s+").groupMapReduce(identity)(const(1))(_ + _)

  def check(f: String => Map[String, Int]) = f(string) should ===(correct)

  "State monad" should "give correct result" in check(countWordsState)
  "Eval based Cont monad" should "give correct result" in check(countWordsCont)
  "Trampolined Cont monad" should "give correct result" in check(countWordsConto)
  "cats Cont monad" should "give correct result in " in check(countWordsContT)
}
