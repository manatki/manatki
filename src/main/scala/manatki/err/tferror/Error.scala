package manatki.err.tferror

trait Error {
  type !
}

trait Failed[-T] {
  def continue[R](f: Error.Aux[T, R]): R
}

object Failed {
  def apply[T] = new Applied[T]

  class Applied[T](private val __ : Boolean = true) extends AnyVal {
    type Arbitrary

    def apply(f: Failure[T, Arbitrary]): Failed[T]                           = f
    def left[R](f: Failure[T, Arbitrary]): Either[Failed[T], R]              = Left(f)
    def condE(b: Boolean)(f: Failure[T, Arbitrary]): Either[Failed[T], Unit] = if (b) left(f) else Right(())
  }

  abstract class Failure[T, Arb] extends Failed[T] {
    def arbContinue(f: Error.Aux[T, Arb]): Arb
    def continue[R](f: Error.Aux[T, R]): R = arbContinue(f.asInstanceOf[Error.Aux[T, Arb]]).asInstanceOf[R]
  }

  implicit class EitherOps[T, A](private val e: Either[Failed[T], A]) extends AnyVal {
    def mapOrElse[B](f: A => B)(h: Error.Aux[T, B]): B = e.fold(_.continue(h), f)
    def mapError[B](h: Error.Aux[T, B]): Either[B, A]  = e.left.map(_.continue(h))
    def handle(h: Error.Aux[T, A]): A                  = mapOrElse(identity)(h)
  }
}

object Error {
  type Aux[+T, R] = T { type ! = R }
}

object Example {
  final case class User private (name: String, age: Int, tags: List[String]) { def copy = () }

  trait UserError extends Error {
    def nameTooLong: !
    def nameTooShort: !
    def tooYoung: !
    def repeatedTag(tag: String): !
  }

  def UserError = Failed[UserError]

  object User {
    def apply(name: String, age: Int, tags: List[String]): Either[Failed[UserError], User] = for {
      _ <- UserError.condE(name.size < 10)(_.nameTooShort)
      _ <- UserError.condE(name.size > 100)(_.nameTooLong)
      _ <- UserError.condE(age < 18)(_.tooYoung)
      _ <- tags.diff(tags.distinct).headOption match {
             case Some(tag) => UserError.left(_.repeatedTag(tag))
             case None      => Right(())
           }
    } yield new User(name, age, tags)
  }

  trait PasswordError extends Error {
    def tooShort: !
    def tooLong: !
    def noDigits: !
    def noLetters: !
  }

  val PasswordError = Failed[PasswordError]

  def validatePassword(password: String): Either[Failed[PasswordError], String] = {
    val digits  = password.count(_.isDigit)
    val letters = password.count(_.isLetter)
    for {
      _ <- PasswordError.condE(password.size < 8)(_.tooShort)
      _ <- PasswordError.condE(password.size > 100)(_.tooLong)
      _ <- PasswordError.condE(digits == 0)(_.noDigits)
      _ <- PasswordError.condE(letters == 0)(_.noLetters)
    } yield password
  }

  case class UserRequest(name: String, age: Int, tags: List[String], password: String)

  def checkUserRequest(req: UserRequest): Either[Failed[UserError with PasswordError], User] =
    for {
      user <- User(req.name, req.age, req.tags)
      _    <- validatePassword(req.password)
    } yield user

  def checkOrString(req: UserRequest): Either[String, User] =
    checkUserRequest(req).mapError(new UserError with PasswordError {
      type ! = String

      def nameTooLong              = "Name length should be less than 100 characters"
      def nameTooShort             = "Name length should be more than 10 characters"
      def tooYoung                 = "Age should be greater than 18"
      def repeatedTag(tag: String) = s"Tag $tag is repeated"
      def tooShort                 = "Password should be at least 8 characters long"
      def tooLong                  = "Password should be at most 100 characters long"
      def noDigits                 = "Password should contain at least one digit"
      def noLetters                = "Password should contain at least one letter"

    })

}
