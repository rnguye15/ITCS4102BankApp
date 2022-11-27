package models

import scala.collection.mutable

object LoginCredentialsModel {
  private val users = mutable.Map[String,String]("Rin" -> "Rin123")
  private val usersbalance = mutable.Map[String,Double]("Rin" -> 10000)
  private val transactions = mutable.Map[String,List[String]]()

  def validateUser(username: String, password: String): Boolean = {
    users.get(username).map(_== password).getOrElse(false)
  }

  def containUser(username: String): Boolean = {
    users.contains(username)
  }

  def createUser(username: String, password: String): Boolean = {
    if (users.contains((username))) false else{
      users(username) = password
      usersbalance(username) = 0.0;
      true
    }
  }

  def resetPass(username: String, password: String): Unit = {
    users(username) = password
  }

  def getPass(username: String): String = {
    return users(username)
  }

  def getBalance(username: String): Double = {
    usersbalance.get(username).getOrElse(0)
  }

  def addBalance(username: String, amount: Double): Unit = {
    var currentBalance = usersbalance(username)
    currentBalance = amount + currentBalance
    usersbalance(username) = currentBalance
  }

  def getTransactions(username: String): Seq[String] = {
    transactions.get(username).getOrElse(Nil)
  }

  def addTransactions(username: String, transaction: String) = {
    transactions(username) = transaction :: transactions.get(username).getOrElse(Nil)
  }

}
