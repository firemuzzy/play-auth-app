package authentication

/**
 * @author mcharkin
 * @since 8/16/12
 *
 */

sealed trait Permission
case object Administrator extends Permission
case object NormalUser extends Permission
