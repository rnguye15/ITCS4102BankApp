package controllers

import models.LoginCredentialsModel
import play.api.mvc.{AbstractController, ControllerComponents}

import java.util.Calendar
import javax.inject.Inject
import scala.math.BigDecimal.double2bigDecimal


class loginController @Inject()(cc: ControllerComponents) extends AbstractController(cc){

  def login = Action { implicit request =>
    Ok(views.html.login())
  }

  def loginPost = Action { implicit request =>
    val postVal = request.body.asFormUrlEncoded
    postVal.map { args =>
      val username = args("username").head
      val password = args("password").head
      if (LoginCredentialsModel.validateUser(username, password)){

        Redirect(routes.loginController.mainMenu()).withSession("username" -> username)
      }else{
        Redirect(routes.loginController.login()).flashing("error" -> "Invalid username/password combination")
      }
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def register = Action { implicit request =>
    Ok(views.html.register())
  }

  def registerPost = Action { implicit request =>
    val postVal = request.body.asFormUrlEncoded
    postVal.map { args =>
      val username = args("username").head
      val password = args("password").head
      val password2 = args("password2").head

      if(password == password2){
        if (LoginCredentialsModel.createUser(username, password)){
          Redirect(routes.loginController.login()).flashing("success" -> "Successfully registered")
        }else{
          Redirect(routes.loginController.register()).flashing("error" -> "User already defined")
        }
      }else{
        Redirect(routes.loginController.register()).flashing("error" -> "Password does not match")
      }
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def deposit = Action { implicit  request =>
    Ok(views.html.deposit())
  }

  def depositPost = Action {implicit  request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val currentUser = username
      val postVal = request.body.asFormUrlEncoded
      val r = scala.util.Random
      val now = Calendar.getInstance().getTime()
      postVal.map {args =>
        val amount = args("amount").head
        LoginCredentialsModel.addBalance(currentUser,amount.toDouble)
        val transactionData = "+ Balance $" + LoginCredentialsModel.getBalance(username) + ", " + now + ": Deposited $" + amount + " Transaction ID: " + r.nextInt().abs
        LoginCredentialsModel.addTransactions(username,transactionData)
        val usersbalance = LoginCredentialsModel.getBalance(username)
        Redirect(routes.loginController.mainMenu()).flashing("success" -> "Amount successfully deposited")
      }.getOrElse(Redirect(routes.loginController.login()))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def withdraw = Action {implicit request =>
    Ok(views.html.withdraw())
  }

  def withdrawPost = Action {implicit  request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val currentUser = username
      val postVal = request.body.asFormUrlEncoded
      postVal.map {args =>
        val amount = args("amount").head
        val r = scala.util.Random
        val now = Calendar.getInstance().getTime()
        val usersbalance = LoginCredentialsModel.getBalance(username)
        val isNegative = usersbalance - amount.toDouble
        val negativeAmount = amount.toDouble * -1
        if (isNegative < 0){
          Redirect(routes.loginController.withdraw()).flashing("error" -> "Insufficient amount")
        }else{
          LoginCredentialsModel.addBalance(username,negativeAmount)
          val transactionData = "- Balance $" + LoginCredentialsModel.getBalance(username) + ", " + now + ": Withdrawn $" + amount + " Transaction ID: " + r.nextInt().abs
          LoginCredentialsModel.addTransactions(username,transactionData)
          Redirect(routes.loginController.mainMenu()).flashing("success" -> "Amount successfully withdrawn")
        }
      }.getOrElse(Redirect(routes.loginController.login()))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def transfer = Action {implicit request =>
    Ok(views.html.transfer())
  }

  def transferPost = Action {implicit request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val currentUser = username
      val postVal = request.body.asFormUrlEncoded
      postVal.map {args =>
        val amount = args("amount").head
        val transferUser = args("transfername").head
        val r = scala.util.Random
        var transactionid = 0
        transactionid = r.nextInt().abs
        val now = Calendar.getInstance().getTime()
        val usersbalance = LoginCredentialsModel.getBalance(username)
        val isNegative = usersbalance - amount.toDouble
        val negativeAmount = amount.toDouble * -1
        if (isNegative < 0){
          Redirect(routes.loginController.transfer()).flashing("error" -> "Insufficient amount")
        }else{
          if(LoginCredentialsModel.containUser(transferUser) && transferUser != username){
            LoginCredentialsModel.addBalance(username,negativeAmount)
            LoginCredentialsModel.addBalance(transferUser,amount.toDouble)
            val transactiondata = "- Balance $"+ LoginCredentialsModel.getBalance(username) + ", "+  now + ": Transferred $" + amount + " to " + transferUser + " Transaction ID: " + transactionid
            val transactionData2 = "+ Balance $" + LoginCredentialsModel.getBalance(transferUser) + ", " + now + ": Received $" + amount + " Transaction ID: " + transactionid
            LoginCredentialsModel.addTransactions(username,transactiondata)
            LoginCredentialsModel.addTransactions(transferUser,transactionData2)
            Redirect(routes.loginController.mainMenu()).flashing("success" -> "Amount successfully transfer")
          }else if(transferUser == username){
           Redirect(routes.loginController.transfer()) .flashing("error"-> "Cannot send money to yourself")
          }else{
            Redirect(routes.loginController.transfer()) .flashing("error"-> "User does not exist")
          }
        }
      }.getOrElse(Redirect(routes.loginController.login()))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def mainMenu = Action { implicit request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val usersbalance = LoginCredentialsModel.getBalance(username)
      Ok(views.html.mainmenu(usersbalance))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def transaction = Action { implicit  request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val userTransaction = LoginCredentialsModel.getTransactions(username)
      Ok(views.html.transaction(userTransaction))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def back = Action {implicit  request =>
    val usernameOption = request.session.get("username")
    usernameOption.map { username =>
      val usersbalance = LoginCredentialsModel.getBalance(username)
      Ok(views.html.mainmenu(usersbalance))
    }.getOrElse(Redirect(routes.loginController.login()))
  }

  def changePass = Action {implicit  request =>
    Ok(views.html.changePass())
  }

  def changePassPost = Action {implicit request =>
    val postVal = request.body.asFormUrlEncoded
    postVal.map { args =>
      val oldpass = args("oldpassword").head
      val password = args("password").head
      val password2 = args("password2").head
      val usernameOption = request.session.get("username")
      usernameOption.map { username =>
        if (oldpass == LoginCredentialsModel.getPass(username)){
          if(password == password2){
            LoginCredentialsModel.resetPass(username,password)
            Redirect(routes.loginController.mainMenu()).flashing("success" -> "Password successfully reset")
          }else{
            Redirect(routes.loginController.changePass()).flashing("error" -> "Password did not match")
          }
        } else  {
          Redirect(routes.loginController.changePass()).flashing("error" -> "Invalid old password")
        }
      }.getOrElse(Redirect(routes.loginController.login()))
      }.getOrElse(Redirect(routes.loginController.login()))
  }


  def account = Action {implicit  request =>
    Ok(views.html.account())
  }

  def logOut = Action {implicit request =>
    Redirect(routes.loginController.login()).withNewSession
  }
}
