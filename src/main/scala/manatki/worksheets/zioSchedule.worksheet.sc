import zio.Schedule
def take[R, E, A](s: Schedule[R, E, A], n: Long): Schedule[R, E, A] =
  (s.zip(Schedule.count) >>> Schedule.recurWhile(_._2 < 1000)).map(_._1)
