import bhvalid.typeValidation.CollectionsCheck.{listSizeMore, mapHasKey}
import bhvalid.typeValidation.NumbersCheck.{<=, >, >=, isMoreThanZero}
import bhvalid.typeValidation.OptionCheck.{isOptional, optionNotNull}
import bhvalid.typeValidation.StringCheck.{isEqual, lengthMore, stringHas}
import bhvalid.{CheckValid, FieldValidation, ToValidator, Validation}

object Main extends App {

  case class User(id: Int, name: String, phoneNumber: Option[String])

  object UserValidation extends ToValidator[User] {
    override def apply(a: User): CheckValid[User] = {
      FieldValidation(a)
        .rule(_.id)(>(0) & <=(10))
        .rule(_.name)(lengthMore(4) & stringHas("a"))
        .rule(_.phoneNumber)(optionNotNull[String])
        .validate
    }
  }

  val user = User(10, "Olag", Some("12345678901"))

  println("Field validation:")

  val res: CheckValid[User] = UserValidation(user)

  println(res.checkValid)
  println("_________________")

  println("Option validation:")

  val optionValidation: Validation[Option[Int]] = isOptional(isMoreThanZero[Int])
  val optionValidation2: Validation[Option[Int]] = isOptional(<=[Int](6))
  val optionValidationFalseResult: CheckValid[Option[Int]] = optionValidation(Some(-5))
  val optionValidationTrueResult: CheckValid[Option[Int]] = optionValidation(Some(7)).flatMap(a => optionValidation2(a))
  println(optionValidationTrueResult.checkValid)
  println(optionValidationFalseResult.checkValid)

  println("_________________")

  println("String validation:")
  val stringValidation: Validation[String] = isEqual("hello")
  val strValFalseRes: CheckValid[String] = stringValidation("hellow")
  val strValTrueRes: CheckValid[String] = stringValidation("hello")
  println(strValTrueRes.checkValid)
  println(strValFalseRes.checkValid)

  println("_________________")

  println("Numeric validation:")
  val numValidation: Validation[Double] = >=(3.0)
  val numValidationFalseRes: CheckValid[Double] = numValidation(2.0)
  val numValidationTrueRes: CheckValid[Double] = numValidation(3.0)
  println(numValidationTrueRes.checkValid)
  println(numValidationFalseRes.checkValid)

  println("_________________")

  println("Collections validation:")
  val collValidation: Validation[List[Int]] = listSizeMore[Int](2)
  val collValidationTrueRes: CheckValid[List[Int]] = collValidation(List(1, 2, 3))
  val collValidationFalseRes: CheckValid[List[Int]] = collValidation(List(1, 2))
  println(collValidationTrueRes.checkValid)
  println(collValidationFalseRes.checkValid)

  println()
  println("Map validation:")
  val mapValidation: Validation[Map[Int, String]] = mapHasKey(2)
  val mapValidationTrueRes: CheckValid[Map[Int, String]] = mapValidation(Map(2 -> "hello"))
  val mapValidationFalseRes: CheckValid[Map[Int, String]] = mapValidation(Map(1 -> "hello"))
  println(mapValidationTrueRes.checkValid)
  println(mapValidationFalseRes.checkValid)
}
