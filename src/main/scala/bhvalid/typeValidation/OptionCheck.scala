package bhvalid.typeValidation

import bhvalid._

object OptionCheck {

  def optionNotNull[A]: Validation[Option[A]] = Validation[Option[A]] { option =>
    if (option.nonEmpty) Valid(option) else Invalid(ValidError("option is Null"))
  }

  def isOptional[A](validation: Validation[A]): Validation[Option[A]] = Validation[Option[A]] { option =>
    option.map(a => validation(a).map(a => Some(a))).getOrElse(Valid(option))
  }
}
