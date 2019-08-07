//package manatki.concurrent
//
//import cats.{Applicative, Functor, Monad}
//import cats.effect.concurrent.{Deferred, Ref}
//import manatki.concurrent.VersionedList.Version
//import monocle.macros.Lenses
//import cats.syntax.foldable._
//import cats.syntax.functor._
//import cats.syntax.applicative._
//import cats.syntax.flatMap._
//import cats.instances.option._
//import monocle.{Lens, PLens, POptional}
//import tofu.Timeout
//import tofu.concurrent.{Deferreds, MakeDeferred, MakeRef}
//import tofu.syntax.timeout._
//
//import scala.collection.immutable.LongMap
//import scala.concurrent.duration.{Duration, FiniteDuration}
//
//
//// discussed with Sergey Alaev 27.07.2019 11:11 https://t.me/scala_ru/232132
//// see also shitty versions here https://gist.github.com/Odomontois/085b56654232e5bd3eadfd9b615ad35b
//// and here https://gist.github.com/Odomontois/c98dba3ad27330babf4772bb62173c88
///** collection of items having incrementing version with ability to wait for new version */
//trait VersionedList[F[_], A] {
//  def add(a: A): F[Unit]
//  def poll(version: Option[Version]): F[(Version, A)]
//}
//
//object VersionedList {
//  type Version = Long
//
//  def apply[I[_]: Functor, F[_]: Monad: Deferreds, A](implicit refs: MakeRef[I, F]): I[VersionedList[F, A]] =
//    MakeRef[I, F].of(State[F, A](LongMap.empty, None, 0L)).map(new Impl(_))
//
//  implicit class VersionedLisOps[F[_], A](private val self: VersionedList[F, A]) extends AnyVal {
//    def tryPoll(version: Option[Version], duration: FiniteDuration)(implicit t: Timeout[F],
//                                                                    F: Applicative[F]): F[Option[(Version, A)]] =
//      self.poll(version).timeout(duration)
//  }
//
//  private type Promise[F[_], A]   = Deferred[F, (Version, A)]
//  private type OptResult[F[_], A] = Either[(Version, A), Promise[F, A]]
//
//  private case class State[F[_], A] private (
//      items: LongMap[A],
//      onNext: Option[Promise[F, A]],
//      version: Version
//  )
//
//  private class Impl[F[_]: Monad: Deferreds, A](ref: Ref[F, State[F, A]]) extends VersionedList[F, A] {
//    def add(a: A): F[Unit] =
//      ref.modify {
//        case State(items, onNext, version) =>
//          val state  = State[F, A](items + (version -> a), None, version + 1)
//          val effect = onNext.traverse_(_.complete(version -> a))
//          state -> effect
//      }.flatten
//
//    def poll(version: Option[Version]): F[(Version, A)] = {
//      val accessor: OptimMod[State[F, A], OptResult[F, A], Promise[F, A]] =
//        POptional(
//          (state: State[F, A]) =>
//            version
//              .flatMap(v => state.items.get(v).tupleLeft(v))
//              .map(Left(_))
//              .orElse(state.onNext.map(Right(_)))
//              .toRight(state))(
//          x => _.copy(onNext = Some(x))
//        )
//
//      ref.optimisticModify(accessor)(MakeDeferred.apply)(Right(_)).flatMap {
//        case Left(va)       => va.pure[F]
//        case Right(promise) => promise.get
//      }
//    }
//  }
//}
