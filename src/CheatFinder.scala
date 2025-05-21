import scala.meta.Tree

object CheatFinder:
  def findCheats(tree: Tree): List[Cheat] =
    Rule.rules.flatMap { rule =>
      rule.codeMatcher(tree).map { code =>
        Cheat(code = code, link = rule.link, description = rule.description)
      }
    }

  end findCheats

end CheatFinder

case class Rule(
    description: String,
    link: String,
    codeMatcher: Tree => List[String]
)

object Rule:
  import scala.meta.*

  lazy val objectRule = Rule(
    "A singleton object",
    "https://docs.scala-lang.org/tour/singleton-objects.html",
    _.collect { case Defn.Object(_, name, _) =>
      name.toString
    }
  )

  lazy val typeParameterRule = Rule(
    "A type parameter",
    "https://docs.scala-lang.org/tour/generic-classes.html",
    _.collect { case node: Type.Param =>
      node.toString
    }
  )

  lazy val classRule = Rule(
    "A class",
    "https://docs.scala-lang.org/tour/classes.html",
    _.collect { case cls: Defn.Class =>
      cls.name.toString
    }
  )

  lazy val stringRule = Rule(
    "A string",
    "https://docs.scala-lang.org/overviews/collections-2.13/strings.html#inner-main",
    _.collect { case aString: Lit.String =>
      aString.toString
      }
  )

  lazy val rules = List(
    objectRule,
    classRule,
    typeParameterRule,
    stringRule
  )

end Rule
