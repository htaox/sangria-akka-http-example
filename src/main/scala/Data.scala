import java.util.UUID

import sangria.execution.{ExceptionHandler, HandledException}
import sangria.marshalling.ResultMarshaller
import SchemaDefinition.Mutation

object Episode extends Enumeration {
  val NEWHOPE, EMPIRE, JEDI = Value
}

trait Character {
  def id: String
  def name: Option[String]
  def friends: List[String]
  def appearsIn: List[Episode.Value]
}

case class Human(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  homePlanet: Option[String]) extends Character

case class Droid(
  id: String,
  name: Option[String],
  friends: List[String],
  appearsIn: List[Episode.Value],
  primaryFunction: Option[String]) extends Character

class CharacterRepo extends Mutation {
  import CharacterRepo._

  def getHero(episode: Option[Episode.Value]) =
    episode flatMap (_ ⇒ getHuman("1000")) getOrElse droids.last

  def getHuman(id: String): Option[Human] = humans.find(c ⇒ c.id == id)

  def getDroid(id: String): Option[Droid] = droids.find(c ⇒ c.id == id)
  
  def getHumans(limit: Int, offset: Int): List[Human] = humans.drop(offset).take(limit)
  
  def getDroids(limit: Int, offset: Int): List[Droid] = droids.drop(offset).take(limit)
}

object CharacterRepo {
  var humans = List(
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List("1002", "1003", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List("1004"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")),
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = List("1000", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None),
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List("1000", "1002", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan")),
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = List("1001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None)
  )

  val droids = List(
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = List("1000", "1002", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol")),
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = List("1000", "1002", "1003"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech"))
  )
}

// Reference
// https://github.com/OlegIlyenko/sangria-auth-example/blob/master/src/main/scala/Data.scala
object Data {

  /*
  trait UserRepo {
    /** Gives back a token or sessionId or anything else that identifies the user session  */
    def authenticate(userName: String, password: String): Option[String]

    /** Gives `User` object with his/her permissions */
    def authorise(token: String): Option[User]
  }
  */

  case class User(userName: String, permissions: List[String])

  class UserRepo {
    var tokens = Map.empty[String, User]

    /** Gives back a token or sessionId or anything else that identifies the user session  */
    def authenticate(userName: String, password: String): Option[String] =
      if (userName == "admin" && password == "secret") {
        val token = UUID.randomUUID().toString
        tokens = tokens + (token → User("admin", "VIEW_PERMISSIONS" :: "EDIT_COLORS" :: "VIEW_COLORS" :: Nil))
        Some(token)
      } else if (userName == "john" && password == "apples") {
        val token = UUID.randomUUID().toString
        tokens = tokens + (token → User("john", "VIEW_COLORS" :: Nil))
        Some(token)
      } else None

    /** Gives `User` object with his/her permissions */
    def authorise(token: String): Option[User] = tokens.get(token)
  }

  case class AuthenticationException(message: String) extends Exception(message)
  case class AuthorisationException(message: String) extends Exception(message)

  /*
  class ColorRepo {
    var colors = List("red", "green", "blue")

    def addColor(color: String) =
      colors = colors :+ color
  }
  */

  case class SecureContext(token: Option[String], userRepo: UserRepo, characterRepo: CharacterRepo) {
    def login(userName: String, password: String) = userRepo.authenticate(userName, password) getOrElse (
      throw new AuthenticationException("UserName or password is incorrect"))

    def authorised[T](permissions: String*)(fn: User ⇒ T) =
      token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token")) { user ⇒
        if (permissions.forall(user.permissions.contains)) fn(user)
        else throw AuthorisationException("You do not have permission to do this operation")
      }

    def ensurePermissions(permissions: List[String]): Unit =
      token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token")) { user ⇒
        if (!permissions.forall(user.permissions.contains))
          throw AuthorisationException("You do not have permission to do this operation")
      }

    def user = token.flatMap(userRepo.authorise).fold(throw AuthorisationException("Invalid token"))(identity)
  }

  val errorHandler = ExceptionHandler {
    case (m, AuthenticationException(message)) ⇒ HandledException(message)
    case (m, AuthorisationException(message)) ⇒ HandledException(message)
  }

}
