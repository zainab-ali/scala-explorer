import com.raquo.airstream.web.WebStorageVar
import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.meta.*
import scala.util.Try

@main def hello =
  val openNodes = WebStorageVar
    .localStorage(key = "scalameta-openNodes", syncOwner = None)
    .withCodec[Set[Int]](
      _.mkString(","),
      a =>
        Try(
          a.split(',').flatMap(_.toIntOption).toSet
        ),
      Try(Set.empty)
    )

  val codeVar = WebStorageVar
    .localStorage(key = "scalameta-code", syncOwner = None)
    .text(CODE)
  val cursorVar = Var(CodeMirrorCursor(0, 0))
  val hoverVar = Var(Option.empty[Int])
  val treeViewVar = Var(Option.empty[TreeView])
  val errorVar = Var(Option.empty[String])
  // val hashVar = Var(Option.empty[String])
  // try
  //   dom.window
  //     .atob(dom.window.location.hash.stripPrefix("#"))
  //     .split(":", 2)
  //     .toList match
  //     case dialect :: code :: Nil =>
  //       allowedDialects
  //         .find(_._1 == dialect)
  //         .map(_._2)
  //         .foreach: dialect =>
  //           dialectVar.set(dialect)
  //           codeVar.set(code)
  //     case _ =>
  // catch
  //   case exc =>
  //     println(s"Error parsing hash: ${exc.getMessage}")
  // end try
  val dialectPicker = DialectPicker()

  val updateHash = codeVar.signal
    .combineWith(dialectPicker.dialectVar.signal)
    .map: (code, dialect) =>
      dom.window.btoa(dialectPicker.serialise(dialect) + ":" + code)
    .--> { hash => // dom.window.location.hash = hash
    }

  def parse(s: String, dialect: Dialect): Either[String, TreeView] =
    dialect.apply(s).parse[scala.meta.Source] match
      case x: Parsed.Success[scala.meta.Source] =>
        Right(
          TreeView(
            x.tree,
            TextIndex.construct(s),
            openNodes,
            cursorVar,
            hoverVar
          )
        )
      case e: Parsed.Error =>
        Left(s"ERROR: $e")
  end parse

  val parsed =
    codeVar.signal.combineWith(dialectPicker.dialectVar.signal).map(parse)

  val updateTreeView =
    parsed.map(_.toOption) --> treeViewVar.writer

  val updateError =
    parsed.map(_.left.toOption) --> errorVar.writer

  val resultNode =
    treeViewVar.signal
      .combineWith(errorVar)
      .map:
        case (None, Some(err)) => p(err)
        case (Some(tv), None)  => tv.root
        case _                 => emptyNode

  val halfsplit =
    Seq(cls := "lg:w-6/12 h-full md:w-full")

  val textEditor =
    CodeMirrorTextArea(
      codeVar,
      cursorVar,
      hoverVar,
      treeViewVar.signal
    )

  val app =
    div(
      updateTreeView,
      updateError,
      updateHash,
      cls := "content mx-auto my-4 w-10/12 bg-white/70 p-6 rounded-xl flex flex-col gap-4 min-h-150",
      div(
        cls := "flex items-center gap-4",
        img(src := "https://scalameta.org/img/scalameta.png", cls := "h-12"),
        h1("Scala AST explorer", cls := "text-4xl font-bold")
      ),
      header,
      div(
        cls := "flex flex-col xs:flex-col md:flex-col sm:flex-col lg:flex-row xl:flex-row 2xl:flex-row  justify-baseline gap-4 w-full",
        div(
          halfsplit,
          dialectPicker.node,
          textEditor.node
        ),
        div(halfsplit, p(code(pre(child <-- resultNode))))
      )
    )

  renderOnDomContentLoaded(dom.document.getElementById("app"), app)
end hello

val basicLink =
  cls := "text-emerald-800 hover:no-underline underline"

val header = div(
  cls := "flex flex-row gap-4 place-content-between w-full",
  p(
    cls := "text-md",
    "Explore the AST of Scala code"
  ),
  p(
    cls := "text-sm",
    a(
      "Github",
      href := "https://github.com/scalameta/ast-explorer",
      basicLink
    ),
    " | ",
    a(
      "Scalameta",
      href := "https://scalameta.org",
      basicLink
    ),
    " | ",
    a(
      "Scala.js",
      href := "https://scala-js.org",
      basicLink
    ),
    " | ",
    a(
      "Laminar",
      href := "https://laminar.dev",
      basicLink
    )
  )
)

val CODE =
  """
object X:
  class Test(a: Int):
    def hello = 25
"""
