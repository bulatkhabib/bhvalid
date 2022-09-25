package bhvalid.typeValidation

import bhvalid._

object StringCheck {

  def notNull: Validation[String] = Validation[String] { str =>
    if (str.nonEmpty) Valid(str) else Invalid(ValidError("string is null"))
  }

  def lengthMore(size: Int = 0): Validation[String] = Validation[String] { str =>
    if (str.length > size) Valid(str) else Invalid(ValidError(s"string size less or equal $size"))
  }

  def lengthLess(size: Int = 0): Validation[String] = Validation[String] { str =>
    if (str.length < size) Valid(str) else Invalid(ValidError(s"string size more or equal $size"))
  }

  def isEqual(string: String): Validation[String] = Validation[String] { str =>
    if (str == string) Valid(str) else Invalid(ValidError(s"string not equal $string"))
  }

  def stringStartWith(string: String): Validation[String] = Validation[String] { str =>
    if (str.startsWith(string)) Valid(str) else Invalid(ValidError(s"string not start with: $string"))
  }

  def stringEndWith(string: String): Validation[String] = Validation[String] { str =>
    if (str.endsWith(string)) Valid(str) else Invalid(ValidError(s"string not end with: $string"))
  }

  def stringHas(string: String): Validation[String] = Validation[String] { str =>
    if (str.contains(string)) Valid(str) else Invalid(ValidError(s"string not contains: $string"))
  }
}
