package bhvalid

trait ToValidator[A] {

  def apply(a: A): CheckValid[A]
}

object ToValidator {

  implicit def toValidator[A](v: ToValidator[A]): Validation[A] = Validation[A] { value =>
    v(value)
  }
}
