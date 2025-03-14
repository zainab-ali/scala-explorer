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
    .localStorage(key = "openNodes", syncOwner = None)
    .withCodec[Set[Int]](
      _.mkString(","),
      a =>
        Try(
          a.split(',').flatMap(_.toIntOption).toSet
        ),
      Try(Set.empty)
    )

  val codeVar = WebStorageVar
    .localStorage(key = "code", syncOwner = None)
    .text(CODE)

  val cursorVar = Var(CodeMirrorCursor(0, 0))
  val hoverVar = Var(Option.empty[Int])
  var treeViewVar = Var(Option.empty[TreeView])
  var errorVar = Var(Option.empty[String])

  def parse(s: String): Either[String, TreeView] =
    scala.meta.dialects.Scala3.apply(s).parse[scala.meta.Source] match
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

  val parsed = codeVar.signal.map(parse)

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
      cls := "content mx-auto my-4 w-10/12 bg-white/70 p-6 rounded-xl flex flex-col gap-4",
      updateTextIndex,
      updateTreeView,
      updateError,
      h1("Scalameta AST explorer", cls := "text-4xl font-bold"),
      p(
        "This small webapp allows you to explore the AST of Scala code",
        cls := "text-sm"
      ),
      div(
        cls := "flex md:flex-col sm:flex-col lg:flex-row justify-baseline 2xl:flex-row gap-4 w-full",
        div(
          cls := "w-6/12 h-full",
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
