package models

import java.util.Properties
import net.tanesha.recaptcha.ReCaptchaFactory
import net.tanesha.recaptcha.ReCaptchaImpl
import net.tanesha.recaptcha.ReCaptchaResponse
import play.api.Play.current

/**
 * @author mcharkin
 * @since 2/2/13
 * 
 */
object ReCaptcha {
  def enabled: Boolean = {
    current.configuration.getBoolean("recaptcha.enabled").getOrElse(true)
  }

  def publicKey(): String = {
    current.configuration.getString("recaptcha.publickey").get
  }
  def privateKey(): String = {
    current.configuration.getString("recaptcha.privatekey").get
  }
  def render(): String = {
    ReCaptchaFactory.newReCaptcha(publicKey(), privateKey(), false).createRecaptchaHtml(null, new Properties)
  }

  def check(addr: String, challenge: String, response: String): Boolean = {
    if (enabled) {
      val reCaptcha = new ReCaptchaImpl()
      reCaptcha.setPrivateKey(privateKey())
      val reCaptchaResponse = reCaptcha.checkAnswer(addr, challenge, response)
      reCaptchaResponse.isValid()
    } else {
      true
    }
  }
}
