package scala.reflect.api

object LiftableCaseClass {

  import scala.reflect.macros._

  def materializeLiftableImpl[T: c.WeakTypeTag](c: WhiteboxContext): c.Tree = {
    import c.universe._

    val Expr(Select(prefix, _)) = c.prefix
    val liftable = Select(prefix, TypeName("Liftable"))

    val tpe = weakTypeOf[T]
    val tpeName = tpe.typeSymbol.name
    val typeModuleSymbol = c.mirror.staticModule(tpe.typeSymbol.fullName)

    val isCaseClass = tpe.typeSymbol.asClass.isCaseClass

    if (!isCaseClass) {
      c.abort(c.enclosingPosition, s"$tpe is not a case class")
    }

    val argName = TermName("v")
    val primaryCtor = tpe.declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }
    val params = primaryCtor
      .get
      .paramss
      .head
      .map(p => (p.name, c.inferImplicitValue(appliedType(typeOf[Liftable[_]], List(p.typeSignature)), silent = true)))
      .map(pair => Apply(Select(pair._2, TermName("apply")), List(Select(Ident(argName), pair._1))))

    val applyMethodBody = Apply(Select(Ident(typeModuleSymbol), TermName("apply")), params)
    val newObjectName = TermName(c.freshName(s"${tpeName}Liftable"))

    q"""
        {
           implicit object $newObjectName extends $liftable[$typeModuleSymbol] {
             def apply($argName: $typeModuleSymbol) = $applyMethodBody
           }
           $newObjectName
        }
        """
  }
}
