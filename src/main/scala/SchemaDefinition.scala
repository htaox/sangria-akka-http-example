import SchemaDefinition.EpisodeEnum
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

  /*
  val EpisodeEnum = EnumType(
    "Episode",
    Some("One of the films in the Star Wars Trilogy"),
    List(
      EnumValue("NEWHOPE",
        value = Episode.NEWHOPE,
        description = Some("Released in 1977.")),
      EnumValue("EMPIRE",
        value = Episode.EMPIRE,
        description = Some("Released in 1980.")),
      EnumValue("JEDI",
        value = Episode.JEDI,
        description = Some("Released in 1983."))))
  */

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
  implicit val characterType = CharacterType

  implicit val HumanType = deriveObjectType[CharacterRepo, Human](
    ObjectTypeName("Human"),
    ObjectTypeDescription("A humanoid creature in the Star Wars universe."),
    Interfaces[CharacterRepo, Human](CharacterType),
    DocumentField("id", "The id of the human."),
    DocumentField("name", "The name of the human."),
    DocumentField("friends", "The friends of the human, or an empty list if they have none."),
    DocumentField("appearsIn", "Which movies they appear in."),
    DocumentField("homePlanet", "The home planet of the human, or null if unknown.")
  )

  /*
    mutation AddTest {
      addHuman(id: "1234", name: "Tester") {
        id,
        name
      }
    }

    query AllHumans {
      humans {
        id,
        name
      }
    }

    query GetId1234 {
      human(id: "1234") {
        id,
        name
      }
    }
  // Using variables
  query GetId1234($id: String!) {
    human(id: $id) {
      id,
      name
    }
  }

  {
    "id": "1234"
  }

  */
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

  /*
  val Human2 =
    ObjectType(
      "Human",
      "A humanoid creature in the Star Wars universe.",
      interfaces[CharacterRepo, Human](Character),
      fields[CharacterRepo, Human](
        Field("id", StringType,
          Some("The id of the human."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the human."),
          resolve = _.value.name),
        Field("friends", ListType(Character),
          Some("The friends of the human, or an empty list if they have none."),
          resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("homePlanet", OptionType(StringType),
          Some("The home planet of the human, or null if unknown."),
          resolve = _.value.homePlanet)
      ))
  */

  /*
  val Droid = ObjectType(
    "Droid",
    "A mechanical creature in the Star Wars universe.",
    interfaces[CharacterRepo, Droid](Character),
    fields[CharacterRepo, Droid](
      Field("id", StringType,
        Some("The id of the droid."),
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the droid."),
        resolve = ctx ⇒ Future.successful(ctx.value.name)),
      Field("friends", ListType(Character),
        Some("The friends of the droid, or an empty list if they have none."),
        resolve = ctx ⇒ characters.deferSeqOpt(ctx.value.friends)),
      Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
        Some("Which movies they appear in."),
        resolve = _.value.appearsIn map (e ⇒ Some(e))),
      Field("primaryFunction", OptionType(StringType),
        Some("The primary function of the droid."),
        resolve = _.value.primaryFunction)
    ))
  */

  implicit val DroidType = deriveObjectType[CharacterRepo, Droid](
    ObjectTypeName("Droid"),
    ObjectTypeDescription("A mechanical creature in the Star Wars universe."),
    Interfaces[CharacterRepo, Droid](CharacterType),
    DocumentField("id", "The id of the droid."),
    DocumentField("name", "The name of the droid."),
    DocumentField("friends", "The friends of the droid, or an empty list if they have none."),
    DocumentField("appearsIn", "Which movies they appear in."),
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
