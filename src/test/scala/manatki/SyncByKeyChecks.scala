package manatki

import akka.actor.typed.scaladsl.Actor
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import akka.util.Timeout
import cats.arrow.FunctionK
import cats.~>
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

object SyncByKeyChecks {
  def main(args: Array[String]): Unit = {
    val mode: SyncMode[String] = if (args.lift(0).contains("unsync")) unsynchonized else synchonized

    val system = ActorSystem(
      SyncByKeyChecks.Guard.actor[String](Seq("lol", "kek", "cheburek"), mode),
      "checker")

    Await.ready(system.whenTerminated, Duration.Inf)
    println("done")
  }

  sealed trait Memo

  type SyncMode[K] = SyncByKey[K] => K => Task ~> Task

  def synchonized[K]: SyncMode[K] = sync => key => sync.trans(key)
  def unsynchonized[K]: SyncMode[K] = _ => _ => FunctionK.id

  implicit val timeout: Timeout = Timeout(1 second)
  object Memo {
    final case class Store(x: Int) extends Memo
    final case class Current(resp: ActorRef[Option[Int]]) extends Memo
    final case class Stop() extends Memo

    def actor(current: Option[Int] = None): Behavior[Memo] = Actor.immutable[Memo] {
      case (_, Store(x)) => actor(Some(x))
      case (_, Current(resp)) =>
        resp ! current
        Actor.same
      case (_, Stop()) => Actor.stopped
    }
  }


  def attacker[K](sync: Task ~> Task,
                  memo: ActorRef[Memo],
                  key: K,
                  count: Long,
                  parent: ActorRef[Guard[K]]
                 ) =
    Actor.deferred[Void] { ctx =>
      implicit val sched = ctx.system.scheduler

      for (_ <- 1L to count) {
        sync(Task.deferFuture {
          val number = Random.nextInt()
          memo ! Memo.Store(number)
          memo ? Memo.Current map ((_, number))
        }).foreach { case (answer, number) =>
          if (!answer.contains(number)) parent ! Guard.Error(key, number, answer)
        }
      }

      Actor.stopped
    }

  trait Guard[K]

  object Guard {
    final case class Done[K]() extends Guard[K]
    final case class Error[K](key: K, number: Int, memo: Option[Int]) extends Guard[K]


    def actor[K](keys: TraversableOnce[K],
                 syncMode: SyncMode[K],
                 attackersByKey: Int = 10,
                 count: Long = 1000,
                ) = Actor.deferred[Guard[K]] { ctx =>


      val keyList = keys.toList
      val sync = SyncByKey(keyList)

      val memos = keyList.map(k => k -> ctx.spawnAnonymous(Memo.actor()))
      val attackers = memos.flatMap { case (k, m) =>
        List.fill(attackersByKey)(
          ctx.watch(ctx.spawnAnonymous(
            attacker[K](syncMode(sync)(k), m, k, count, ctx.self))))
      }

      def waitAttackers(remains: Long): Behavior[Guard[K]] = Actor.immutable[Guard[K]] {
        case (_, Done()) => if (remains == 1) stopMemos else waitAttackers(remains - 1)
        case (_, Error(key, num, prev)) =>
          println(s"sync error [$key] $num vs $prev")
          Actor.same
      } onSignal {
        case (_, _: Terminated) =>
          ctx.self ! Done()
          Actor.same
      }

      def stopMemos: Behavior[Guard[K]] = Actor.deferred { ctx =>
        for ((_, m) <- memos) {
          ctx.watch(m)
          m ! Memo.Stop()
        }

        def stopping(remains: Int): Behavior[Guard[K]] = Actor.onSignal {
          case (_, _: Terminated) =>
            ctx.self ! Done()
            if (remains == 1) Actor.stopped else stopping(remains - 1)
        }

        stopping(memos.size)
      }

      waitAttackers(1L * keyList.size * attackersByKey)
    }
  }


}


