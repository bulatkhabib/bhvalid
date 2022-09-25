package bhvalid

trait Validation[A] { self =>

  def apply(a: A): CheckValid[A]

  def &(validation: => Validation[A]): Validation[A] = (a: A) => self(a) match {
    case Valid(value) => validation(a) match {
      case value @ Valid(_) => value
      case invalid: Invalid => invalid
    }
    case invalid: Invalid => validation(a) match {
      case Valid(value) => invalid
      case invalid2: Invalid => Invalid(invalid.e ++ invalid2.e: _*)
    }
  }

  def |(validation: Validation[A]): Validation[A] = (a: A) => self(a) match {
    case value @ Valid(_) => value
    case invalid: Invalid => validation(a) match {
      case value @ Valid(_) => value
      case invalid2: Invalid => Invalid(invalid.e ++ invalid2.e: _*)
    }
  }
}

object Validation {

  def apply[A](t: A => CheckValid[A]): Validation[A] = (a: A) => t(a)
}