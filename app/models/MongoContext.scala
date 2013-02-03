/**
 * @author mcharkin
 * @since 7/12/12
 *
 */

import com.novus.salat.{TypeHintFrequency, StringTypeHintStrategy, Context}
import play.api.Play
import play.api.Play.current

package object MongoContext {
  implicit val context = {
    val context = new Context {
      val name = "global"

      /**
       * mongo db does not like 2d indexed fields to have anything other than
       * the lng, lat fields. Salat puts a type hint, so I had to define a
       * new context overwriting the type hinting behavior
       */
      override val typeHintStrategy = StringTypeHintStrategy(when = TypeHintFrequency.WhenNecessary, typeHint = "_t")
    }
    context.registerGlobalKeyOverride(remapThis = "id", toThisInstead = "_id")
    context.registerClassLoader(Play.classloader)
    context
  }
}
