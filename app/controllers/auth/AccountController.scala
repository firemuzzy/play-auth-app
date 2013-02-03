package controllers.auth

import play.api.mvc._
import jp.t2v.lab.play20.auth.{Auth, LoginLogout}

import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints
import play.api.libs.json._

import com.mongodb.casbah.Imports._
import authentication._
import models._

import scalaz._
import validation.MyConstraints
import viewImplicits.NavAccountData
import play.api.libs.concurrent.Akka

import scalaz._
import Scalaz._
import emailer.Emailer

/**
 * @author mcharkin
 * @since 12/14/12
 * 
 */
object AccountController extends Controller with LoginLogout with Auth with AuthConfigImpl {
  private val loginForm = Form {
    mapping("email" -> text, "password" -> text)(Account.authenticate)(_.map(u => (u.email, ""))).verifying(MyConstraints.loginValid, MyConstraints.accountVerified)
  }
  def login = Action { implicit request => Ok(views.html.auth.login(loginForm)) }
  def loginPost = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.auth.login(formWithErrors)),
      user => {
        gotoLoginSucceeded(user.get.id)
      }
    )
  }

  private val registerForm = Form(
    tuple(
      "email" -> text.verifying(Constraints.nonEmpty, MyConstraints.uniqueEmail),
      "password" -> text.verifying(Constraints.nonEmpty),
      "password2" -> text.verifying(Constraints.nonEmpty)
    ) verifying("Passwords did not match", (t: Tuple3[String,String,String]) => !t._2.isEmpty && !t._3.isEmpty && t._2.equals(t._3) )
  )
  def register = Action { implicit request =>
    Ok(views.html.auth.register(registerForm))
  }
  def registerPost =  Action { implicit request =>
    play.api.Logger.info("registerPost")

    registerForm.bindFromRequest.fold(
      formWithErrors => {
        play.api.Logger.info("errs: %s".format(formWithErrors))
        BadRequest(views.html.auth.register(formWithErrors))
      },
      user => {
        val email = user._1
        val password = user._2
        val account = Account.createUnverified(email = email, password = password)
        play.api.Logger.info("created account: %s".format(account))
        Account.save(account)

        val baseUrl = controllers.routes.Application.index().absoluteURL(false)(request).dropRight(1)
        Emailer.sendVerificationEmail(baseUrl, account)

        Redirect(controllers.auth.routes.AccountController.message).flashing(
          "success" -> "Account created for %s, please click on the emailed link to activate it".format(email)
        )
      }
    )
  }

  def verify(accountId: ObjectId, verificationToken: String) = Action { implicit request =>
    val resetResult = for {
      account <- Account.findOneById(accountId).toSuccess("No account found")
      isValidToken <- account.verificationTokenValid(verificationToken)
      varifiedAccount <- Account.verifyAccount(account)
    } yield varifiedAccount

    resetResult match {
      case Success(account) => gotoLoginSucceeded(account.id)
      case Failure(message) => {
        implicit val nav:Option[NavAccountData] = None
        Ok(views.html.errors.error(message))
      }
    }
  }

  val forgotPasswordForm = Form(
    single(
      "email" -> text.verifying(Constraints.nonEmpty)
    )
  )
  def forgotPassword = Action { implicit request =>
    Ok(views.html.auth.forgotPassword(forgotPasswordForm))
  }
  def forgotPasswordPost = Action { implicit request =>
    forgotPasswordForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.auth.forgotPassword(formWithErrors)),
      userEmail => {
        val result = for {
          account <- Account.findByEmail(userEmail).toSuccess("no account present with email %s".format(userEmail))
          accountWithResetToken <- Account.resetPassword(account.id)
        } yield {
          val baseUrl = controllers.routes.Application.index().absoluteURL(false)(request).dropRight(1)
          Emailer.sendResetEmail(baseUrl, accountWithResetToken)
          play.api.Logger.info("password reset url: "+ controllers.auth.routes.AccountController.resetPassword(account.id, accountWithResetToken.passForgotToken.getOrElse("")).url)
        }

        result match {
          case Success(obj) => {
            Redirect(controllers.auth.routes.AccountController.forgotPassword).flashing("success" -> "We have sent an email to %s with instructions how to reset your password".format(userEmail))
          }
          case Failure(error) => {
            Redirect(controllers.auth.routes.AccountController.forgotPassword).flashing("error" -> error)
          }
        }
      }
    )
  }

  def resetPassword(accountId: ObjectId, passForgotToken: String) = Action { implicit request =>
    val resetResult = for {
      account <- Account.findOneById(accountId).toSuccess("No account found")
      validToken <- account.passwordResetTokenValid(passForgotToken)
    } yield {
      account
    }

    resetResult match {
      case Success(account) => {
        val fillData = PasswordReset(password = "", password2 = "", accountId = account.id, passwordResetToken = account.passForgotToken.get)
        val preFilled = PasswordReset.form.fill(fillData)
        Ok(views.html.auth.resetPassword(preFilled))
      }
      case Failure(message) => {
        implicit val nav:Option[NavAccountData] = None
        Ok(views.html.errors.error(message))
      }
    }
  }
  def resetPasswordPost = Action { implicit request =>
    PasswordReset.form.bindFromRequest.fold(
      formWithErrors => {
        play.api.Logger.error("global errors:" + formWithErrors.globalErrors.toString)
        play.api.Logger.error("form errors:" + formWithErrors.errors.toString)

        BadRequest(views.html.auth.resetPassword(formWithErrors))
      },
      resetPassForm => {
        Account.changePassword(resetPassForm.accountId, resetPassForm.password)
        gotoLoginSucceeded(resetPassForm.accountId).flashing("success" -> "password changed")
      }
    )
  }

  def logout = Action { implicit request =>
    gotoLogoutSucceeded
  }

  def message = Action { implicit request =>
    implicit val nav:Option[NavAccountData] = None
    Ok(views.html.auth.message())
  }
}
