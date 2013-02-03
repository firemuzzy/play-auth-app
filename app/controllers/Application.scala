package controllers

import play.api._
import play.api.mvc._
import jp.t2v.lab.play20.auth.Auth
import authentication._
import viewImplicits.NavAccountData

object Application extends Controller with Auth with AuthConfigImpl {
  
  def index = optionalUserAction { user => implicit request =>
    implicit val navAccountDataOpt = user.map(u => NavAccountData(u))
    Ok(views.html.index("Your new application is ready."))
  }

  def javascriptRoutes = Action { implicit request =>
    Ok(
      Routes.javascriptRouter("jsRoutes")(

      )
    ).as("text/javascript")
  }
}