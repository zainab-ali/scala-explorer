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

  lazy val typeRule = Rule(
    "A Type",
    "https://docs.scala-lang.org/scala3/book/types-introduction.html",
    _.collect { case aType: Type.Name =>
      aType.toString()
    }
  )

  lazy val booleanRule = Rule(
    "A Boolean",
    "https://docs.scala-lang.org/scala3/book/scala-for-javascript-devs.html#boolean-values",
    _.collect { case aBoolean: Lit.Boolean =>
      aBoolean.toString()
    }
  )

  lazy val integerRule = Rule(
    "An Integer",
    "https://docs.scala-lang.org/scala3/book/first-look-at-types.html#scalas-value-types",
    _.collect { case anInteger: Lit.Int =>
      anInteger.toString()
    }
  )

  lazy val importRule = Rule(
    "An import",
    "https://docs.scala-lang.org/tour/packages-and-imports.html#imports",
    _.collect { case anImport: Import =>
      anImport.toString
    }
  )

  lazy val wildcardImportRule = Rule(
    "A wildcard import",
    "https://docs.scala-lang.org/tour/packages-and-imports.html#imports",
    _.collect { case aWildcardImport: Importee.Wildcard =>
      aWildcardImport.toString
    }
  )

  private lazy val givenRule = Rule(
    "A given instance",
    "https://docs.scala-lang.org/scala3/book/ca-context-parameters.html#given-instances-implicit-definitions-in-scala-2",
    _.collect { case aGiven @Defn.GivenAlias(_, name, _, _, _, _) =>
      name.toString
    }
  )

  private lazy val covariantRule = Rule(
    "A covariant type",
    "https://docs.scala-lang.org/tour/variances.html#covariance",
    _.collect { case aThing @ Type.Param(List(Mod.Covariant()), _, _, _, _, _) =>
      aThing.toString
    }
  )

  private lazy val contravariantRule = Rule(
    "An contravariant type",
    "https://docs.scala-lang.org/tour/variances.html#contravariance",
    _.collect { case aThing @ Type.Param(List(Mod.Contravariant()), _, _, _, _, _) =>
      aThing.toString
    }
  )

  private lazy val unitRule = Rule(
    "A Unit",
    "https://www.scala-lang.org/api/current/scala/Unit.html",
    _.collect { case aUnit: Lit.Unit =>
      aUnit.toString
    }
  )

  lazy val rules = List(
    objectRule,
    classRule,
    typeParameterRule,
    stringRule,
    typeRule,
    booleanRule,
    integerRule,
    importRule,
    wildcardImportRule,
    givenRule,
    covariantRule,
    contravariantRule,
    unitRule
  )

end Rule
