import Data._
import Middleware.{Authorised, Permission}
import sangria.execution.UserFacingError
import sangria.execution.deferred.{Fetcher, HasId}
import sangria.schema._
import sangria.macros.derive.{GraphQLField, deriveObjectType, _}

import scala.concurrent.Future

/**
 * Defines a GraphQL schema for the current project
 */
object SchemaDefinition {
  case class MutationError(message: String) extends Exception(message) with UserFacingError

  /**
    * Resolves the lists of characters. These resolutions are batched and
    * cached for the duration of a query.
    */
  val characters = Fetcher.caching(
    (ctx: SecureContext, ids: Seq[String]) ⇒
      Future.successful(ids.flatMap(id ⇒ ctx.characterRepo.getHuman(id) orElse ctx.characterRepo.getDroid(id))))(HasId(_.id))

  val EpisodeEnum = deriveEnumType[Episode.Value](
    EnumTypeName("Episode"),
    EnumTypeDescription("One of the films in the Star Wars Trilogy")
  )

  val CharacterType: InterfaceType[SecureContext, Character] =
    InterfaceType(
      "Character",
      "A character in the Star Wars Trilogy",
      () ⇒ fields[SecureContext, Character](
        Field("id", StringType,
          Some("The id of the character."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the character."),
          resolve = _.value.name),
        Field("friends", ListType(CharacterType),
          Some("The friends of the character, or an empty list if they have none."),
          resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e)))
      ))

  implicit val episodeEnum = EpisodeEnum

  implicit val HumanType = deriveObjectType[SecureContext, Human](
    ObjectTypeName("Human"),
    ObjectTypeDescription("A humanoid creature in the Star Wars universe."),
    Interfaces[SecureContext, Human](CharacterType),
    ReplaceField("friends",
      Field("friends", ListType(CharacterType),
        Some("The friends of the human, or an empty list if they have none."),
        resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends))
    ),
    DocumentField("homePlanet", "The home planet of the human, or null if unknown.")
  )

  trait Mutation {
    @GraphQLField
    def addHuman(id: String, name: Option[String]) = {
      val h = Human(id, name, List(), List(), None)

      val preInsert = if (CharacterRepo.humans.exists(a => a.id == id))
        CharacterRepo.humans.filter(a => a.id != id) else CharacterRepo.humans

      CharacterRepo.humans =  preInsert ::: List(h)
      h
    }
  }

  // Can derivedContextObject get access to Context?
  // val MutationType2 = deriveContextObjectType[SecureContext, Mutation, Unit](_.characterRepo)

  val MutationType = ObjectType("Mutation", fields[SecureContext, Unit](
    Field("login", OptionType(StringType),
      arguments = UserNameArg :: PasswordArg :: Nil,
      resolve = ctx ⇒ UpdateCtx(ctx.ctx.login(ctx.arg(UserNameArg), ctx.arg(PasswordArg))) { token ⇒
        ctx.ctx.copy(token = Some(token))
      }),
    Field("addHuman", OptionType(ListType(StringType)),
      arguments = NewHumanIdArg :: NewHumanNameArg :: Nil,
      resolve = ctx ⇒ ctx.ctx.authorised("EDIT_HUMANS") { _ ⇒
        ctx.ctx.characterRepo.addHuman(ctx.arg(NewHumanIdArg), ctx.arg(NewHumanNameArg))
        ctx.ctx.characterRepo.getHuman(ctx.arg(NewHumanIdArg))
      })
  ))


  implicit val DroidType = deriveObjectType[SecureContext, Droid](
    ObjectTypeName("Droid"),
    ObjectTypeDescription("A mechanical creature in the Star Wars universe."),
    Interfaces[SecureContext, Droid](CharacterType),
    ReplaceField("friends",
      Field("friends", ListType(CharacterType),
        Some("The friends of the human, or an empty list if they have none."),
        resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends))
    ),
    DocumentField("primaryFunction", "The primary function of the droid.")
  )

  // Security types
  val UserType = ObjectType("User", fields[SecureContext, User](
    Field("userName", StringType, resolve = _.value.userName),
    Field("permissions", OptionType(ListType(StringType)),
      tags = Permission("VIEW_PERMISSIONS") :: Nil,
      resolve = _.value.permissions)
  ))

  val UserNameArg = Argument("userName", StringType)
  val PasswordArg = Argument("password", StringType)
  val NewHumanIdArg = Argument("id", StringType)
  val NewHumanNameArg = Argument("name", OptionInputType(StringType))

  val ID = Argument("id", StringType, description = "id of the character")

  val EpisodeArg = Argument("episode", OptionInputType(EpisodeEnum),
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")

  val LimitArg = Argument("limit", OptionInputType(IntType), defaultValue = 20)
  val OffsetArg = Argument("offset", OptionInputType(IntType), defaultValue = 0)

  val Query = ObjectType(
    "Query", fields[SecureContext, Unit](
      Field("me", OptionType(UserType), tags = Authorised :: Nil,resolve = _.ctx.user),
      Field("hero", CharacterType,
        arguments = EpisodeArg :: Nil,
        deprecationReason = Some("Use `human` or `droid` fields instead"),
        resolve = ctx => ctx.ctx.characterRepo.getHero(ctx.arg(EpisodeArg))
        // resolve = (ctx) ⇒ ctx.ctx.getHero(ctx.arg(EpisodeArg))
      ),
      Field("human", OptionType(HumanType),
        arguments = ID :: Nil,
        resolve = ctx => ctx.ctx.characterRepo.getHuman(ctx arg ID)
        // resolve = ctx ⇒ ctx.ctx.getHuman(ctx arg ID)
      ),
      Field("droid", DroidType,
        arguments = ID :: Nil,
        resolve = ctx => ctx.ctx.characterRepo.getDroid(ctx arg ID)
        // resolve = ctx ⇒ ctx.ctx.getDroid(ctx arg ID).get
      ),
      Field("humans", ListType(HumanType),
        arguments = LimitArg :: OffsetArg :: Nil,
        tags = Permission("VIEW_HUMANS") :: Nil,
        // resolve = ctx ⇒ ctx.ctx.getHumans(ctx arg LimitArg, ctx arg OffsetArg)
        resolve = ctx => ctx.ctx.characterRepo.getHumans(ctx arg LimitArg, ctx arg OffsetArg)
      ),
      Field("droids", ListType(DroidType),
        arguments = LimitArg :: OffsetArg :: Nil,
        tags = Permission("VIEW_DROIDS") :: Nil,
        resolve = ctx => ctx.ctx.characterRepo.getDroids(ctx arg LimitArg, ctx arg OffsetArg)
        // resolve = ctx ⇒ ctx.ctx.getDroids(ctx arg LimitArg, ctx arg OffsetArg)
      )
    ))

  val StarWarsSchema = Schema(Query, Some(MutationType))
}
