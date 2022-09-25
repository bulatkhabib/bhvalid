package bhvalid.typeValidation

import bhvalid._

object CollectionsCheck {

  def listNotNil[A]: Validation[List[A]] = Validation[List[A]] { list =>
    if (list.nonEmpty) Valid(list) else Invalid(ValidError("list is empty"))
  }

  def listHasElement[A](el: A): Validation[List[A]] = Validation[List[A]] { list =>
    if (list.contains(el)) Valid(list) else Invalid(ValidError("list hasn't Element"))
  }

  def listSizeMore[A](size: Int): Validation[List[A]] = Validation[List[A]] { list =>
    if (list.length > size) Valid(list) else Invalid(ValidError(s"list size < $size"))
  }

  def listSizeLess[A](size: Int): Validation[List[A]] = Validation[List[A]] { list =>
    if (list.length < size) Valid(list) else Invalid(ValidError(s"list size > $size"))
  }

  def mapNotNil[A, B]: Validation[Map[A, B]] = Validation[Map[A, B]] { map =>
    if (map.nonEmpty) Valid(map) else Invalid(ValidError("map is empty"))
  }

  def mapHasKey[A, B](k: A): Validation[Map[A, B]] = Validation[Map[A, B]] { map =>
    if (map.contains(k)) Valid(map) else Invalid(ValidError(s"map hasn't key ${k}"))
  }
}
