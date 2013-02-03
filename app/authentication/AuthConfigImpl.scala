package authentication

import jp.t2v.lab.play20.auth.{FromString, ToString, CookieRelationResolver, AuthConfig}
import com.mongodb.casbah.Imports._
import play.api.mvc._
import play.api.mvc.Results._
import controllers.routes
import models.Account
import crypto.MyCrypto

/**
 * @author mcharkin
 * @since 8/16/12
 *
 */

trait AuthConfigImpl extends AuthConfig {
  /**
   * A type that is used to identify a user.
   * `String`, `Int`, `Long` and so on.
   */
  type Id = ObjectId

  /**
   * A type that represents a user in your application.
   * `User`, `Account` and so on.
   */
  type User = Account

  /**
   * A type that is defined by every action for authorization.
   * This sample uses the following trait:
   *
   * sealed trait Permission
   * case object Administrator extends Permission
   * case object NormalUser extends Permission
   */
  type Authority = Permission

  /**
   * A `ClassManifest` is used to retrieve an id from the Cache API.
   * Use something like this:
   */
  val idManifest: ClassManifest[Id] = classManifest[Id]

  /**
   * The session timeout in seconds
   */
  val sessionTimeoutInSeconds: Int = 3600

  /**
   * A function that returns a `User` object from an `Id`.
   * You can alter the procedure to suit your application.
   */
  def resolveUser(userId: Id): Option[User] = Account.findOneById(userId)

  /**
   * Where to redirect the user after a successful login.
   */
  def loginSucceeded[A](request: Request[A]): PlainResult = {
    val uri = request.session.get("access_uri").getOrElse(routes.Application.index().url.toString)
    request.session - "access_uri"
    Redirect(uri)
  }

  /**
   * Where to redirect the user after logging out
   */
  def logoutSucceeded[A](request: Request[A]): PlainResult = Redirect(controllers.routes.Application.index)

  /**
   * If the user is not logged in and tries to access a protected resource then redirct them as follows:
   */
  def authenticationFailed[A](request: Request[A]): PlainResult =
    Redirect(controllers.auth.routes.AccountController.login).withSession("access_uri" -> request.uri)

  /**
   * If authorization failed (usually incorrect password) redirect the user as follows:
   */
  def authorizationFailed[A](request: Request[A]): PlainResult = Forbidden("no permission")

  /**
   * A function that determines what `Authority` a user has.
   * You should alter this procedure to suit your application.
   */
  def authorize(user: User, authority: Authority): Boolean =
    (user.permission,authority) match {
      case (Administrator,Administrator) => true
      case (NormalUser,NormalUser) => true
      case (Administrator,NormalUser) => true
      case _ => false
    }

  //  /*
  //   * TODO: make the cookie timeout
  //   */
  override def resolver[A](implicit request: Request[A]) = {
    new CookieRelationResolver[Id, A](request)
  }

  implicit val objIdToS = ToString[ObjectId](id => MyCrypto.encryptAES(id.toString))
  implicit val objIdFromS = FromString[ObjectId](s => {
    val decrypted = MyCrypto.decryptAES(s)
    new ObjectId(decrypted)
  })

}