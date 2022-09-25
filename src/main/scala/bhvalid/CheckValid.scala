package bhvalid

sealed trait CheckValid[+T] { self =>

  def getOrElse[A >: T](value: => A): A = self match {
    case Valid(value) => value
    case invalid: Invalid => value
  }

  def flatMap[A](f: T => CheckValid[A]): CheckValid[A] = self match {
    case Valid(value) => f(value)
    case invalid: Invalid => invalid
  }

  def map[A](f: T => A): CheckValid[A] = self match {
    case Valid(value) => Valid(f(value))
    case invalid: Invalid => invalid
  }

  def checkValid = self match {
    case Valid(value) => value
    case invalid: Invalid => invalid.e.foldLeft("")((acc, e) => acc + e.error + ", ")
  }
}

final case class Valid[A](value: A) extends CheckValid[A]
final case class Invalid(e: ValidError*) extends CheckValid[Nothing]

final case class ValidError(error: String)