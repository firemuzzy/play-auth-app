package validation

/**
 * @author mcharkin
 * @since 7/1/12
 *
 */
import com.mongodb.casbah.Imports._

import play.api.data.validation._
import play.api.data.validation.ValidationError
import models._

object MyConstraints {
  def loginValid: Constraint[Option[Account]] = Constraint[Option[Account]]("constraint.loginValid", null) { accountOpt =>
    accountOpt match {
      case Some(_) => Valid
      case None => Invalid(ValidationError("error.loginInvalid"))
    }
  }

  def accountVerified: Constraint[Option[Account]] = Constraint[Option[Account]]("constraint.accountVerified", null) { accountOpt =>
    accountOpt match {
      case Some(account) => if(account.verified.isDefined) Valid else Invalid(ValidationError("error.accountUnverified"))
      case None => Valid
    }
  }

  /**
   * Defines a minimum value for `A` values, i.e. the value must be greater than or equal to the constraint parameter
   *
   * '''name'''[constraint.min(minValue)]
   * '''error'''[error.min(minValue)]
   */
  def min(minValue: Double ): Constraint[Double] = Constraint[Double]("constraint.min", minValue) { o =>
    if (o >= minValue) Valid else Invalid(ValidationError("error.min", minValue))
  }

  /**
   * Defines a maximum value constraint for `A` values, i.e. value must be less than or equal to the constraint parameter
   *
   * '''name'''[constraint.max(maxValue)]
   * '''error'''[error.max(maxValue)]
   */
  def max(maxValue: Double): Constraint[Double] = {
    Constraint[Double]("constraint.max", maxValue) { o =>
      if (o <= maxValue) Valid else Invalid(ValidationError("error.max", maxValue))
    }
  }

  def notEmpty : Constraint[String] = {
    Constraint[String]("constraint.notEmpty", Nil) { o =>
      if (o.isEmpty) Invalid(ValidationError("error.notEmpty", Nil))
      else Valid
    }
  }

  def nonEmptyOpt[String] : Constraint[Option[String]] = {
    Constraint[Option[String]]("constraint.notEmpty", Nil) { o =>
      o match {
        case Some(a) => {
          if (a.toString.size >= 1) Valid
          else Invalid(ValidationError("error.notEmpty", Nil))
        }
        case None => Valid
      }
    }
  }

  def required[T] : Constraint[Option[T]] = {
    Constraint[Option[T]]("constraint.required", Nil) { o =>
      o match {
        case Some(_) => Valid
        case None => Invalid(ValidationError("error.required"))
      }
    }
  }

  def undefined[T] : Constraint[Option[T]] = {
    Constraint[Option[T]]("constraint.required", Nil) { o =>
      o match {
        case Some(_) => Invalid(ValidationError("error.undefined", Nil))
        case None => Valid
      }
    }
  }

  def requiredCustMsg[T](msg: String) : Constraint[Option[T]] = {
    Constraint[Option[T]]("constraint.required", Nil) { o =>
      o match {
        case Some(_) => Valid
        case None => Invalid(ValidationError(msg, Nil))
      }
    }
  }

  def goodPass[String] : Constraint[String] = {
    Constraint[String]("constraint.strongPassword", Nil) { str =>
      if (str.toString.length >= 6) Valid
      else Invalid(ValidationError("error.strongPassword", Nil))
    }
  }

  def goodPassOpt[String] : Constraint[Option[String]] = {
    Constraint[Option[String]]("constraint.strongPassword", Nil) { o =>
      o match {
        case Some(str) => {
          if (str.toString.length >= 6) Valid
          else Invalid(ValidationError("error.strongPassword", Nil))
        }
        case None => Invalid(ValidationError("error.strongPassword", Nil))
      }
    }
  }

  /**
   * Defines a minimum optional value for `A` values, i.e. the value must be greater than or equal to the constraint parameter
   * if there is no value, the constraint is not applied
   *
   * '''name'''[constraint.min(minValue)]
   * '''error'''[error.min(minValue)]
   */
  def minOpt(minValue: Double ): Constraint[Option[Double]] = Constraint[Option[Double]]("constraint.min", minValue) { o =>
    if (o.isEmpty) Valid else if (o.get >= minValue) Valid else Invalid(ValidationError("error.min", minValue))
  }

  def minOptStr(minValue: Double ): Constraint[Option[String]] = Constraint[Option[String]]("constraint.min", minValue) { o =>
    try {
      if (!o.isDefined) Valid
      else if (o.get.toDouble >= minValue) Valid
      else Invalid(ValidationError("error.min", minValue))
    } catch {
      case e:Exception => Invalid(ValidationError("error.notANumber", Nil))
    }
  }

  /**
   * Defines a maximum optional value constraint for `A` values, i.e. value must be less than or equal to the constraint parameter
   * if there is no value, the constraint is not applied
   *
   * '''name'''[constraint.max(maxValue)]
   * '''error'''[error.max(maxValue)]
   */
  def maxOpt(maxValue: Double): Constraint[Option[Double]] = Constraint[Option[Double]]("constraint.max", maxValue) { o =>
    if (o.isEmpty) Valid else if (o.get <= maxValue) Valid else Invalid(ValidationError("error.max", maxValue))
  }

  def maxOptStr(maxValue: Double ): Constraint[Option[String]] = Constraint[Option[String]]("constraint.max", maxValue) { o =>
    try {
      if (!o.isDefined || o.get.size == 0) Valid
      else if (o.get.toDouble <= maxValue) Valid
      else Invalid(ValidationError("error.max", maxValue))
    } catch {
      case e:Exception => Invalid(ValidationError("error.notANumber", Nil))
    }
  }

  val titlePattern = "[^a-zA-Z0-9 \\-\\.#&=/'!\\+:]".r
  def title: Constraint[String] = Constraint[String]("constraint.titleChars", Nil) { o =>
    val matches = titlePattern.findAllIn(o)

    if (matches.isEmpty) Valid
    else Invalid(ValidationError("error.titleChars", matches.toSeq.mkString(" ")))
  }

  def titleOpt: Constraint[Option[String]] = Constraint[Option[String]]("constraint.titleChars", Nil) { o =>
    val pattern = titlePattern
    if (o.isDefined) {
      val matches = titlePattern.findAllIn(o.get)

      if (matches.isEmpty) Valid
      else Invalid(ValidationError("error.titleChars", matches.toSeq.mkString(" ")))
    }
    else Valid
  }

  def paragraphOpt: Constraint[Option[String]] = Constraint[Option[String]]("constraint.paragraphChars", Nil) { o =>
    val pattern = "[^a-zA-Z0-9 \\-\\.#\\&=/',\\(\\)!%?\\*\";:$°]".r
    if (o.isDefined) {
      val matches = pattern.findAllIn(o.get)

      if (matches.isEmpty) Valid
      else Invalid(ValidationError("error.titleChars", matches.toSeq.mkString(" ")))
    }
    else Valid
  }

  def latOpt: Constraint[Option[Double]] = Constraint[Option[Double]]("constraint.lat", Nil) { o =>
    if (o.isEmpty) Valid else if (o.get >= -90 && o.get <= 90) Valid else Invalid(ValidationError("error.lat"))
  }

  def lngOpt: Constraint[Option[Double]] = Constraint[Option[Double]]("constraint.lng", Nil) { o =>
    if (o.isEmpty) Valid else if (o.get >= -180 && o.get <= 180) Valid else Invalid(ValidationError("error.lng"))
  }

  def uniqueEmail: Constraint[String] = Constraint[String]("constraint.uniqueEmail", Nil) { o =>
    if (Account.findByEmail(o).isEmpty) Valid else Invalid(ValidationError("error.uniqueEmail", Nil))
  }

  def charkinEmail: Constraint[String] = Constraint[String]("constraint.limitedEmail", Nil) { o =>
    if (o == "mcharkin@gmail.com" || o == "acharkin@gmail.com") Valid else Invalid(ValidationError("error.limitedEmail", Nil))
  }

  def validPassword(implicit user: Account): Constraint[String] = {
    Constraint[String]("constraint.validPassword", Nil) { o =>
      if (user.isCorrectPassword(o)) Valid
      else Invalid(ValidationError("error.invalidPassword", Nil))
    }
  }

  def notEmptyList[A]: Constraint[Iterable[A]] = Constraint[Iterable[A]]("constraint.emptyList", Nil) { traits =>
    if (traits.isEmpty) Invalid(ValidationError("error.emptyList", Nil))
    else Valid
  }
}
