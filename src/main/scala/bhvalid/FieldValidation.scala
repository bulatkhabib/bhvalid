package bhvalid

trait FieldValidation[A] {

  def rule[B](f: A => B)(validation: Validation[B]): FieldValidation[A]

  def validate: CheckValid[A]
}

object FieldValidation {

  private class FieldValidationImpl[A](a: A, val rules: List[A => CheckValid[_]] = Nil)

    extends FieldValidation[A] {
    override def rule[B](f: A => B)(validation: Validation[B]): FieldValidation[A] = {
      val valid: B => CheckValid[B] = value => validation(value)
      val res = f andThen(valid)

      new FieldValidationImpl(a, res :: rules)
    }

    override def validate: CheckValid[A] = {
      val e = rules.foldLeft(List.empty[Invalid]) { case (acc, v) =>
        v(a) match {
          case Valid(value) => acc
          case invalid: Invalid => invalid :: acc
        }
      }

      e match {
        case errors => Invalid(errors.flatMap(_.e): _*)
        case Nil => Valid(a)
      }
    }
  }

  def apply[A](a: A): FieldValidation[A] =
    new FieldValidationImpl[A](a, Nil)
}
