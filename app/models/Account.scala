package models

import MongoContext._
import authentication._

import com.novus.salat._
import com.novus.salat.annotations._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import play.api.Play.current

import play.api.libs.json._
import play.api.libs.json.JsString
import scala.Some
import org.apache.commons.lang3._

import scalaz._
import Scalaz._
import org.joda.time.{DateTimeZone, DateTime}
import play.api.i18n.Messages
import java.util.regex.Pattern
import org.mindrot.jbcrypt.BCrypt

/**
 * @author mcharkin
 * @since 8/16/12
 *
 */

case class Account(
  @Key("_id")id: ObjectId = new ObjectId,
  email: String,
  password: String,
  salt: String,
  token: Option[String] = None,
  admin: Boolean = false,
  passForgot: Option[java.util.Date] = None,
  passForgotToken: Option[String] = None,
  verificationRequested: Option[java.util.Date] = None,
  verified: Option[java.util.Date] = None,
  verificationToken: Option[String] = None
) {
  def activated = !verificationRequested.isDefined || verified.isDefined

  def passwordResetTokenValid(token: String) = {
    val result = passwordResetTokenValidWMessageErr(token).fold(a => Messages(a).fail, _.success)
    result
  }

  def passwordResetTokenValidWMessageErr(token: String) = {
    for {
      currentPassToken <- this.passForgotToken.toSuccess("error.passwordResetAlreadyCompleted")
      passForgot <- this.passForgot.toSuccess("error.passwordResetAlreadyCompleted")
      tokenNotExpired <- {
        val passForgotExpiration = (new DateTime(passForgot, DateTimeZone.getDefault)).plusMinutes(10)
        val now = DateTime.now(DateTimeZone.getDefault);
        if(now.isBefore(passForgotExpiration)) {
          Success(true)
        } else {
          Failure("error.passResetTokenExpiered")
        }
      }
    } yield true
  }

  def verificationTokenValid(token: String) = {
    val result = verificationTokenValidWMessageErr(token).fold(a => Messages(a).fail, _.success)
    result
  }

  def verificationTokenValidWMessageErr(token: String) = {
    for {
      myVRequested <- this.verificationRequested.toSuccess("error.accountVerificationNotRequested")
      notVerified <- this.verified.map(a => "error.accountAlreadyActivated").toFailure(true)
      myVToken <- this.verificationToken.toSuccess("error.accountVerificationNotSet")
      tokenMatch <- if(myVToken == token) Success(true) else Failure("error.accountVerificationTokenNoMatch")
      tokenNotExpired <- {
        val passForgotExpiration = (new DateTime(myVRequested, DateTimeZone.getDefault)).plusHours(24)
        val now = DateTime.now(DateTimeZone.getDefault);
        if(now.isBefore(passForgotExpiration)) {
          Success(true)
        } else {
          Failure("error.accountVerificationTokenExpired")
        }
      }
    } yield true
  }

  def permission :Permission = if (admin || email == "mcharkin@gmail.com" || email == "acharkin@gmail.com") { Administrator } else NormalUser
  def isAdmin = {
    permission match {
      case Administrator => true
      case _ => false
    }
  }

  def isCorrectPassword(passTest: String) = {
    val testPassHash = BCrypt.hashpw(passTest, salt)
    testPassHash == password
  }
}

object Account extends ModelCompanion[Account, ObjectId] {
  val collection = mongoCollection("accounts")
  val dao = new AccountDao(collection)

  implicit def toDbObj(b: Account) = grater[Account].asDBObject(b)

  def createVerified(email: String, password: String) = {
    val salt = BCrypt.gensalt()
    val passHash = BCrypt.hashpw(password, salt)

    //TODO: this token generation might need some work
    val token = Some(BCrypt.hashpw(email, BCrypt.gensalt()))

    val lowercasedEmail = email.toLowerCase
    Account(email = lowercasedEmail, password = passHash, salt = salt, token = token, verified = Some(new java.util.Date))
  }

  def createUnverified(email: String, password: String) = {
    val salt = BCrypt.gensalt()
    val passHash = BCrypt.hashpw(password, salt)

    //TODO: this token generation might need some work
    val token = Some(BCrypt.hashpw(email, BCrypt.gensalt()))

    val verificationToken =  RandomStringUtils.randomAlphanumeric(20)

    val lowercasedEmail = email.toLowerCase
    Account(email = lowercasedEmail, password = passHash, salt = salt, token = token, verified = None, verificationToken = Some(verificationToken), verificationRequested = Some(new java.util.Date))
  }

  def findOrCreate(email: String) = {
    val account = Account.findByEmail(email)

    account match {
      case Some(foundAccount) => Success(foundAccount)
      case None => {
        val randomPassword = RandomStringUtils.randomAlphanumeric(10)
        val newAccount = Account.createUnverified(email, randomPassword)
        Account.insert(newAccount) match {
          case Some(id) => Success(newAccount)
          case None => Failure("failed to insert account")
        }
      }
    }
  }

  def verifyAccount(accountId: ObjectId): Validation[String, Account] = {
    for {
      account <- Account.findOneById(accountId).toSuccess("no account found")
      verified <- verifyAccount(account)
    } yield verified
  }
  def verifyAccount(account: Account): Validation[String, Account] = {
    val verifiedDate = new java.util.Date
    Account.dao.update(MongoDBObject("_id" -> account.id), $set("verified" -> verifiedDate), false, false)
    Success(account.copy(verified = Some(verifiedDate)))
  }

  def changePassword(accountId: ObjectId, newPassword: String) = {
    for{
      account <- Account.findOneById(accountId).toSuccess("no account found")
    } yield {
      val salt = BCrypt.gensalt()
      val newPassHash = BCrypt.hashpw(newPassword, salt)

      Account.dao.update(MongoDBObject("_id" -> accountId), $unset("passForgotToken"), false, false)

      Account.dao.update(MongoDBObject("_id" -> accountId), $set("password" -> newPassHash), false, false)
      Account.dao.update(MongoDBObject("_id" -> accountId), $set("salt" -> salt), false, false)

      account.copy(salt = salt, password = newPassHash, passForgotToken = None)
    }
  }

  def resetPassword(accountId: ObjectId): Validation[String, Account] = {
    for {
      account <- Account.findOneById(accountId).toSuccess("no accout found")
      updatedAccount <- resetPassword(account)
    } yield updatedAccount
  }

  def resetPassword(account: Account): Validation[String, Account] = {
    val forgotToken = RandomStringUtils.randomAlphanumeric(20)
    val resetRequestedDate = new java.util.Date
    Account.dao.update(MongoDBObject("_id" -> account.id), $set("passForgot" -> resetRequestedDate), false, false)
    Account.dao.update(MongoDBObject("_id" -> account.id), $set("passForgotToken" -> forgotToken), false, false)
    Success(account.copy(passForgot = Some(resetRequestedDate), passForgotToken = Some(forgotToken)))
  }

  def searchByEmail(email: String) = {
    val regex = ("^(?i)"+Pattern.quote(email)).r
    val query = MongoDBObject("email" -> MongoDBObject("$regex" -> regex))
    dao.find(query).toList
  }

  def find(ids: Seq[ObjectId]) = {
    dao.find("_id" $in ids)
  }

  def findByEmail(email: String) = {
    val lowerCased = email.toLowerCase
    dao.findOne(MongoDBObject("email" -> lowerCased))
  }

  def authenticate(email: String, password: String): Option[Account] = {
    findByEmail(email).filter { account => BCrypt.checkpw(password, account.password) }
  }

  def authenticateToken(email: String, token: String): Option[Account] = {
    findByEmail(email).filter { a => a.token.isDefined && a.token.get == token }
  }

  def matchingPasswords(p1 : String, p2 : String) = {
    !p1.isEmpty && !p2.isEmpty && p1.equals(p2)
  }

  implicit object AccountFormat extends Writes[Account] {
    def writes(account: Account) = {
      JsObject(Seq(
        // the json is read by android, which required the ID to be a long
        // so we cannot use the 12 byte object id and have to shrink it
        "_id" -> JsString(account.id.toString),
        "email" -> JsString(account.email))
      )
    }
  }
}

class AccountDao(collection: MongoCollection) extends SalatDAO[Account, ObjectId](collection) {
  val ensuredIndexs = {
    play.api.Logger.info("ensuring indecies on Account")
    this.collection.ensureIndex(MongoDBObject("email" -> 1 ))
    this.collection.ensureIndex(MongoDBObject("token" -> 1 ))
    this.collection.ensureIndex(MongoDBObject("passForgotToken" -> 1 ))
    this.collection.ensureIndex(MongoDBObject("verificationToken" -> 1 ))
  }
}