//package manatki.data
//
//import akka.actor.Scheduler
//import akka.actor.typed.scaladsl.AskPattern._
//import akka.actor.typed.scaladsl.Behaviors
//import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
//import akka.util.Timeout
//import cats.arrow.FunctionK
//import cats.~>
//import monix.eval.Task
//import monix.execution.Scheduler.Implicits.global
//
//import scala.concurrent.Await
//import scala.concurrent.duration._
//import scala.util.Random
//
//object SyncByKeyChecks {
//  def main(args: Array[String]): Unit = {
//    val mode: SyncMode[String] =
//      if (args.lift(0).contains("unsync")) unsynchonized else synchonized
//
//    val system = ActorSystem(
//      SyncByKeyChecks.Guard.actor[String](Seq("lol", "kek", "cheburek"), mode),
//      "checker")
//
//    Await.ready(system.whenTerminated, Duration.Inf)
//    println("done")
//  }
//
//  sealed trait Memo
//
//  type SyncMode[K] = SyncByKey[K] => K => Task ~> Task
//
//  def synchonized[K]: SyncMode[K] = sync => key => sync.trans(key)
//  def unsynchonized[K]: SyncMode[K] = _ => _ => FunctionK.id
//
//  implicit val timeout: Timeout = Timeout(1 second)
//  object Memo {
//    final case class Store(x: Int) extends Memo
//    final case class Current(resp: ActorRef[Option[Int]]) extends Memo
//    final case class Stop() extends Memo
//
//    def actor(current: Option[Int] = None): Behavior[Memo] =
//      Behaviors.receive[Memo] {
//        case (_, Store(x)) => actor(Some(x))
//        case (_, Current(resp)) =>
//          resp ! current
//          Behaviors.same
//        case (_, Stop()) => Behaviors.stopped
//      }
//  }
//
//  def attacker[K](sync: Task ~> Task,
//                  memo: ActorRef[Memo],
//                  key: K,
//                  count: Long,
//                  parent: ActorRef[Guard[K]]): Behavior[Void] =
//    Behaviors.setup[Void] { ctx =>
//      implicit val sched: Scheduler = ctx.system.scheduler
//
//      for (_ <- 1L to count) {
//        sync(Task.deferFuture {
//          val number = Random.nextInt()
//          memo ! Memo.Store(number)
//          memo ? Memo.Current map ((_, number))
//        }).foreach {
//          case (answer, number) =>
//            if (!answer.contains(number))
//              parent ! Guard.Error(key, number, answer)
//        }
//      }
//
//      Behaviors.stopped
//    }
//
//  sealed trait Guard[K]
//
//  object Guard {
//    final case class Done[K]() extends Guard[K]
//    final case class Error[K](key: K, number: Int, memo: Option[Int])
//        extends Guard[K]
//
//    def actor[K](keys: TraversableOnce[K],
//                 syncMode: SyncMode[K],
//                 attackersByKey: Int = 10,
//                 count: Long = 1000L,
//    ): Behavior[Guard[K]] = Behaviors.setup[Guard[K]] { ctx =>
//      val keyList = keys.toList
//      val syncTask = SyncByKey(keyList)
//
//      val memos = keyList.map(k => k -> ctx.spawnAnonymous(Memo.actor()))
//      val attackers = syncTask.foreach { sync =>
//        memos.flatMap {
//          case (k, m) =>
//            List.fill(attackersByKey)(
//              ctx.watch(ctx.spawnAnonymous(
//                attacker[K](syncMode(sync)(k), m, k, count, ctx.self))))
//        }
//      }
//
//      def waitAttackers(remains: Long): Behavior[Guard[K]] =
//        Behaviors.receiveMessage[Guard[K]] {
//          case Done() =>
//            if (remains == 1) stopMemos else waitAttackers(remains - 1)
//          case Error(key, num, prev) =>
//            println(s"sync error [$key] $num vs $prev")
//            Behaviors.same
//        } receiveSignal {
//          case (_, _: Terminated) =>
//            ctx.self ! Done()
//            Behaviors.same
//        }
//
//      def stopMemos: Behavior[Guard[K]] = Behaviors.setup { ctx =>
//        for ((_, m) <- memos) {
//          ctx.watch(m)
//          m ! Memo.Stop()
//        }
//
//        def stopping(remains: Int): Behavior[Guard[K]] = Behaviors.receiveSignal {
//          case (_, _: Terminated) =>
//            ctx.self ! Done()
//            if (remains == 1) Behaviors.stopped else stopping(remains - 1)
//        }
//
//        stopping(memos.size)
//      }
//
//      waitAttackers(1L * keyList.size * attackersByKey)
//    }
//  }
//
//}
