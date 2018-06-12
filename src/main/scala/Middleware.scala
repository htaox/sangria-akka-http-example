import sangria.execution.{FieldTag, Middleware, MiddlewareBeforeField, MiddlewareQueryContext}
import sangria.schema._
import Data._

object Middleware {

  case object Authorised extends FieldTag
  case class Permission(name: String) extends FieldTag

  object SecurityEnforcer extends Middleware[SecureContext] with MiddlewareBeforeField[SecureContext] {
    type QueryVal = Unit
    type FieldVal = Unit

    def beforeQuery(context: MiddlewareQueryContext[SecureContext, _, _]) = ()

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[SecureContext, _, _]) = ()

    def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[SecureContext, _, _], ctx: Context[SecureContext, _]) = {
      val permissions = ctx.field.tags.collect { case Permission(p) ⇒ p }
      val requireAuth = ctx.field.tags contains Authorised
      val securityCtx = ctx.ctx

      if (requireAuth)
        securityCtx.user

      if (permissions.nonEmpty)
        securityCtx.ensurePermissions(permissions)

      continue
    }
  }

}
