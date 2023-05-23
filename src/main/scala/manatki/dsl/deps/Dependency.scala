package manatki.dsl.deps

import BuilderDSL._
import Dependency._

sealed trait Dependency {
  type Evidence[-A, +B] = BuilderDSL.Evidence[this.type, A, B]
}

object Dependency {
  case object Cassandra extends Dependency
  case object Kafka     extends Dependency
  case object Postgres  extends Dependency
}

object BuilderDSL {
  trait Evidence[Resource, -Config, +Result] {
    def build(c: Config): Result
  }

  sealed class Build[+R](val build: Vector[R]) {
    def add[Config, Res >: R](res: Dependency)(implicit ev: Evidence[res.type, Config, Res]): WithConfig[Config, Res] =
      new WithConfig(build, ev)
  }

  object Build extends Build[Nothing](Vector.empty)

  final class WithConfig[Config, Result](build: Vector[Result], ev: Evidence[_, Config, Result]) {
    def withConfig(config: Config): Build[Result] =
      new Build(build :+ ev.build(config))
  }

}

object EvidencesForDependencies {
  implicit val forCassandra: Cassandra.Evidence[String, List[String]] = c => List(c)
  implicit val forKafka: Kafka.Evidence[Long, List[String]]           = c => List(c.toString)
  implicit val forPostgres: Postgres.Evidence[Double, List[String]]   = c => List(c.toString)
}

import EvidencesForDependencies._

object DepTest extends App {
  val result =
    Build.add(Cassandra).withConfig("cassandra string").add(Kafka).withConfig(1L).add(Postgres).withConfig(0.0d).build

  println(s"result: $result")
}
