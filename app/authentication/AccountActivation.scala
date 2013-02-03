package authentication

import validation.MyConstraints
import se.radley.plugin.salat.Formats._

import play.api.data._
import play.api.data.Forms._
import validation._

import models.Account
import org.bson.types.ObjectId

import scalaz._
import Scalaz._


/**
 * @author mcharkin
 * @since 10/14/12
 *
 */
case class AccountActivation(
  password: String,
  password2: String,
  accountId: ObjectId,
  accountActivationToken: String
)

object AccountActivation {
  val form = Form(
    mapping(
      "password" -> text.verifying(Constraints.nonEmpty, MyConstraints.goodPass),
      "password2" -> text.verifying(Constraints.nonEmpty, MyConstraints.goodPass),
      "accountId" -> of[ObjectId],
      "accountActivationToken" -> text.verifying(Constraints.nonEmpty)
    )
    ((password, password2, accountId, accountActivationToken) => AccountActivation(password = password, password2 = password2, accountId = accountId, accountActivationToken = accountActivationToken))
    ((reset : AccountActivation) => Some((reset.password, reset.password2, reset.accountId, reset.accountActivationToken)))
    .verifying(matchingPasswords, validateAccountActivationToken)
  )

  def matchingPasswords: Constraint[AccountActivation] = Constraint[AccountActivation]("constraint.matchingPasswords", Nil) { o =>
    if (o.password == o.password2) Valid
    else Invalid(ValidationError("error.matchingPasswords", Nil))
  }

  def validateAccountActivationToken: Constraint[AccountActivation] = Constraint[AccountActivation]("constraint.passwordResetToken", Nil) { o =>
    val resetValidation = for {
      account <- Account.dao.findOneById(o.accountId).toSuccess("error.accountId")
      isValid <- account.verificationTokenValidWMessageErr(o.accountActivationToken)
    } yield account

    resetValidation match {
      case Success(account) => {
        if (o.password == o.password2) Valid
        else Invalid(ValidationError("error.matchingPasswords", Nil))
      }
      case Failure(msg) => Invalid(ValidationError(msg, Nil))
    }
  }
}
