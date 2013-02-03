package authentication

import models.Account
import validation.MyConstraints
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat.Formats._

import play.api.data._
import play.api.data.Forms._
import validation._

import scalaz._
import Scalaz._
import org.joda.time.{DateTimeZone, DateTime}

/**
 * @author mcharkin
 * @since 9/5/12
 *
 */
case class PasswordReset(
  password: String,
  password2: String,
  accountId: ObjectId,
  passwordResetToken: String
)

object PasswordReset {
  val form = Form(
    mapping(
      "password" -> text.verifying(Constraints.nonEmpty, MyConstraints.goodPass),
      "password2" -> text.verifying(Constraints.nonEmpty, MyConstraints.goodPass),
      "accountId" -> of[ObjectId],
      "passwordResetToken" -> text.verifying(Constraints.nonEmpty)
    )
    ((password, password2, accountId, passwordResetToken) => PasswordReset(password = password, password2 = password2, accountId = accountId, passwordResetToken = passwordResetToken))
    ((reset : PasswordReset) => Some((reset.password, reset.password2, reset.accountId, reset.passwordResetToken)))
    .verifying(matchingPasswords, validPasswordResetToken)
  )

  def matchingPasswords: Constraint[PasswordReset] = Constraint[PasswordReset]("constraint.matchingPasswords", Nil) { o =>
    if (o.password == o.password2) Valid
    else Invalid(ValidationError("error.matchingPasswords", Nil))
  }

  def validPasswordResetToken: Constraint[PasswordReset] = Constraint[PasswordReset]("constraint.passwordResetToken", Nil) { o =>
    val resetValidation = for {
      account <- Account.dao.findOneById(o.accountId).toSuccess("error.accountId")
      isValidToken <- account.verificationTokenValid(o.passwordResetToken)
      currentPassToken <- account.passForgotToken.toSuccess("error.passwordResetAlreadyCompleted")
      passForgot <- account.passForgot.toSuccess("error.passwordResetAlreadyCompleted")
      tokenNotExpired <- {
        val passForgotExpiration = (new DateTime(passForgot, DateTimeZone.getDefault)).plusMinutes(10)
        val now = DateTime.now(DateTimeZone.getDefault);
        if(now.isBefore(passForgotExpiration)) {
          Success(true)
        } else {
          Failure("error.passResetTokenExpiered")
        }
      }
    } yield currentPassToken

    resetValidation match {
      case Success(currentPassToken) =>
      case Failure(msg) => Invalid(ValidationError(msg, Nil))
    }

    if (o.password == o.password2) Valid
    else Invalid(ValidationError("error.matchingPasswords", Nil))
  }
}
