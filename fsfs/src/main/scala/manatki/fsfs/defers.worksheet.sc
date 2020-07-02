import cats.free.Free
import cats.Defer
import fs2._

implicitly[Defer[Free[Pure, *]]]