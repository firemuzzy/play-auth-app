package emailer

import models.Account
import scalaz.{Failure, Success, Validation}
import com.typesafe.plugin._
import play.api.Play.current

/**
 * @author mcharkin
 * @since 2/3/13
 * 
 */
object Emailer {
  def sendEmail(subject: String, recepient: String, bodyHtml: String): Validation[String, Unit] = {
    val mail = use[MailerPlugin].email
    mail.setSubject(subject)
    mail.addRecipient(recepient)
    mail.addFrom("BottleCub <bottlebot@bottlecub.com>")

    //sends html
    try {
      mail.sendHtml(bodyHtml)
      Success(())
    } catch {
      case e: Exception => {
        play.api.Logger.error("could not send an email to " + recepient, e)
        Failure("could not send an email to " + recepient)
      }
    }
  }

  def sendResetEmail(baseUrl: String, account: Account) {
    play.api.Logger.info("sending password reset email to " + account.email)
    val html = views.html.emailTemplates.resetPassword(baseUrl, account, account.passForgotToken.getOrElse(""))
    val subject =  "Reset your BottleCub password"
    sendEmail(subject, account.email, html.toString)
  }

  def sendVerificationEmail(baseUrl: String, account: Account) {
    play.api.Logger.info("sending verification email to " + account.email)

    val html = views.html.emailTemplates.validateAccount(baseUrl, account, account.verificationToken.getOrElse(""))
    val subject =  "Verify your email"
    sendEmail(subject, account.email, html.toString)
  }
}
