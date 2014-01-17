import org.scalacheck._, Prop._, Gen._, Arbitrary._
import scala.reflect.runtime.universe._, Flag._

object LiftableCaseClassProps extends QuasiquoteProperties("liftableCaseClass") {
  property("splice simple case class") = test {
    case class Cls(field: Int)
    val simple = Cls(1)
    assert(q"$simple" ≈ Apply(Ident(TermName("Cls")), List(Literal(Constant(1)))))
  }

  property("splice simple case class") = test {
    case class Cls(field1: Int, field2: String)
    val simple = Cls(1, "two")
    assert(q"$simple" ≈ Apply(Ident(TermName("Cls")), List(Literal(Constant(1)), Literal(Constant("two")))))
  }
}