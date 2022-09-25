package bhvalid.typeValidation

import bhvalid._
import Ordering.Implicits._

import scala.math.Numeric.Implicits.infixNumericOps

object NumbersCheck {

  def isMoreThanZero[A : Numeric]: Validation[A] = Validation[A] { n =>
    if (n.abs == n) Valid(n) else Invalid(ValidError("number is < 0"))
  }

  def >[A: Numeric](a: A): Validation[A] = Validation[A] { n =>
    if (n > a) Valid(n) else Invalid(ValidError(s"number is <= $a"))
  }

  def >=[A: Numeric](a: A): Validation[A] = Validation[A] { n =>
    if (n >= a) Valid(n) else Invalid(ValidError(s"number is < ${a}"))
  }

  def <[A: Numeric](a: A): Validation[A] = Validation[A] { n =>
    if (n < a) Valid(n) else Invalid(ValidError(s"number is >= ${a}"))
  }

  def <=[A: Numeric](a: A): Validation[A] = Validation[A] { n =>
    if (n <= a) Valid(n) else Invalid(ValidError(s"number is > ${a}"))
  }

  def ==[A: Numeric](a: A): Validation[A] = Validation[A] { n =>
    if (n == a) Valid(a) else Invalid(ValidError(s"numbers is != ${a}"))
  }
}
