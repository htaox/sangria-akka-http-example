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
    (ctx: CharacterRepo, ids: Seq[String]) ⇒
      Future.successful(ids.flatMap(id ⇒ ctx.getHuman(id) orElse ctx.getDroid(id))))(HasId(_.id))

  val EpisodeEnum = deriveEnumType[Episode.Value](
    EnumTypeName("Episode"),
    EnumTypeDescription("One of the films in the Star Wars Trilogy")
  )

  val CharacterType: InterfaceType[CharacterRepo, Character] =
    InterfaceType(
      "Character",
      "A character in the Star Wars Trilogy",
      () ⇒ fields[CharacterRepo, Character](
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

  implicit val HumanType = deriveObjectType[CharacterRepo, Human](
    ObjectTypeName("Human"),
    ObjectTypeDescription("A humanoid creature in the Star Wars universe."),
    Interfaces[CharacterRepo, Human](CharacterType),
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

  val MutationType = deriveContextObjectType[CharacterRepo, Mutation, Unit](identity)

  implicit val DroidType = deriveObjectType[CharacterRepo, Droid](
    ObjectTypeName("Droid"),
    ObjectTypeDescription("A mechanical creature in the Star Wars universe."),
    Interfaces[CharacterRepo, Droid](CharacterType),
    ReplaceField("friends",
      Field("friends", ListType(CharacterType),
        Some("The friends of the human, or an empty list if they have none."),
        resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends))
    ),
    DocumentField("primaryFunction", "The primary function of the droid.")
  )

  val ID = Argument("id", StringType, description = "id of the character")

  val EpisodeArg = Argument("episode", OptionInputType(EpisodeEnum),
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")

  val LimitArg = Argument("limit", OptionInputType(IntType), defaultValue = 20)
  val OffsetArg = Argument("offset", OptionInputType(IntType), defaultValue = 0)

  val Query = ObjectType(
    "Query", fields[CharacterRepo, Unit](
      Field("hero", CharacterType,
        arguments = EpisodeArg :: Nil,
        deprecationReason = Some("Use `human` or `droid` fields instead"),
        resolve = (ctx) ⇒ ctx.ctx.getHero(ctx.arg(EpisodeArg))),
      Field("human", OptionType(HumanType),
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getHuman(ctx arg ID)),
      Field("droid", DroidType,
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getDroid(ctx arg ID).get),
      Field("humans", ListType(HumanType),
        arguments = LimitArg :: OffsetArg :: Nil,
        resolve = ctx ⇒ ctx.ctx.getHumans(ctx arg LimitArg, ctx arg OffsetArg)),
      Field("droids", ListType(DroidType),
        arguments = LimitArg :: OffsetArg :: Nil,
        resolve = ctx ⇒ ctx.ctx.getDroids(ctx arg LimitArg, ctx arg OffsetArg))
    ))

  val StarWarsSchema = Schema(Query, Some(MutationType))
}
