import scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.airstream.web.WebStorageVar

import scala.meta.*
import scala.util.Try
import scala.annotation.tailrec

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
  val hashVar = Var(Option.empty[String])

  val (allowedDialects, serialise) =
    import scala.meta.dialects.*
    val l = List(
      "Scala 2.12" -> Scala212,
      "Scala 2.13" -> Scala213,
      "Scala 3" -> Scala3,
      "Scala 3 Future" -> Scala3Future,
      "SBT 1.x" -> Sbt1
    )

    (l, l.map(_.swap).toMap)
  end val

  val dialectVar = WebStorageVar
    .localStorage(key = "scalameta-dialect", syncOwner = None)
    .withCodec[Dialect](
      serialise(_),
      a =>
        Try(
          allowedDialects
            .find(_._1 == a)
            .map(_._2)
            .getOrElse(scala.meta.dialects.Scala3)
        ),
      Try(scala.meta.dialects.Scala3)
    )

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

  val dialectPicker = div(
    cls := "flex flex-row gap-2 text-xs mb-2",
    children <-- dialectVar.signal.map: current =>
      allowedDialects.map: (name, dialect) =>
        button(
          cls := (if dialect != current then
                    "cursor-pointer bg-gray-200 hover:bg-gray-300 rounded-md px-2 py-1 text-xs"
                  else "bg-gray-800 rounded-md px-2 py-1 text-xs text-white"),
          name,
          onClick.mapTo(dialect) --> dialectVar.writer
        )
  )

  val updateHash = codeVar.signal
    .combineWith(dialectVar.signal)
    .map: (code, dialect) =>
      dom.window.btoa(serialise(dialect) + ":" + code)
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

  val textIndex = Var(TextIndex.construct(codeVar.now()))
  val updateTextIndex =
    codeVar.signal.map(TextIndex.construct(_)) --> textIndex.writer

  val parsed = codeVar.signal.combineWith(dialectVar.signal).map(parse)

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

  val app =
    div(
      cls := "content mx-auto my-4 w-10/12 bg-white/70 p-6 rounded-xl flex flex-col gap-4 min-h-150",
      updateTextIndex,
      updateTreeView,
      updateError,
      updateHash,
      h1("Scalameta AST explorer", cls := "text-4xl font-bold"),
      p(
        "This small webapp allows you to explore the AST of Scala code",
        cls := "text-sm"
      ),
      div(
        cls := "flex md:flex-col sm:flex-col lg:flex-row justify-baseline 2xl:flex-row gap-4 w-full",
        div(
          cls := "w-6/12 h-full",
          dialectPicker,
          codeMirrorTextArea(
            codeVar,
            cursorVar,
            hoverVar,
            treeViewVar.signal
          )
        ),
        div(cls := "w-6/12 h-full", p(code(pre(child <-- resultNode))))
      )
    )

  renderOnDomContentLoaded(dom.document.getElementById("app"), app)
end hello

def codeMirrorTextArea(
    target: Var[String],
    cursor: Var[CodeMirrorCursor],
    hover: Var[Option[Int]],
    tv: Signal[Option[TreeView]]
) =
  var instance = Option.empty[CodeMirrorInstance]
  var marker = Option.empty[CodeMirrorMark]
  val highlightText = hover.signal.combineWith(tv) --> {
    (hoverOpt, treeViewOpt) =>
      marker.foreach(_.clear())
      for
        treeview <- treeViewOpt
        hover <- hoverOpt
        inst <- instance
        tree <- treeview.getTree(hover)
      do
        marker = Some(
          inst.markText(
            js.Dynamic
              .literal(line = tree.pos.startLine, ch = tree.pos.startColumn),
            js.Dynamic
              .literal(line = tree.pos.endLine, ch = tree.pos.endColumn),
            js.Dynamic.literal(css =
              "background-color: rgba(255, 204, 0, 0.3);"
            )
          )
        )
      end for
  }

  textArea(
    cls := "h-full",
    onInput.mapToValue --> target,
    value <-- target,
    highlightText,
    onMountCallback(el =>
      instance = Some(
        CodeMirror
          .fromTextArea(
            el.thisNode.ref,
            js.Dictionary(
              "value" -> target.now(),
              "lineNumbers" -> true,
              "mode" -> "text/x-scala"
            )
          )
      )

      instance
        .foreach(_.on("change", value => target.set(value.getValue())))

      instance.foreach(_.setSize("100%", "100%"))
      instance.foreach(
        _.on(
          "cursorActivity",
          codeMirrorInstance =>
            cursor.set(
              codeMirrorInstance.asInstanceOf[CodeMirrorInstance].getCursor()
            )
        )
      )
    )
  )
end codeMirrorTextArea

val CODE =
  """
object X:
  class Test(a: Int):
    def hello = 25
"""
