import scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.airstream.web.WebStorageVar

import scala.meta.*
import scala.util.Try
class TreeView(t: Tree, openNodes: Var[Set[Int]]):
  val toggle = openNodes.updater[Int]: (cur, next) =>
    if cur(next) then cur - next else cur + next
  def isToggled(id: Int) = openNodes.signal.map(_.contains(id))

  lazy val root = div(
    cls := "bg-gray-100 p-4 rounded-lg shadow-md",
    code(encode(t))
  )
  lazy val (direct, reverse) = index(t)

  private def encode(t: Tree): Element =
    val id = reverse(t)
    span(
      span(
        a(
          cls := "text-blue-700 text-sm",
          href := "#",
          child.text <-- isToggled(id).map: b =>
            val moniker =
              if t.children.isEmpty then "  " else if b then "- " else "+ "
            moniker + t.productPrefix
          ,
          onClick.preventDefault.mapToStrict(id) --> toggle,
          t.productPrefix
        ),
        " ",
        i(
          cls := "text-amber-700 text-xs font-mono",
          s"[${t.pos.start};${t.pos.end}]"
        )
      ),
      ul(
        cls := "list-inside list-none ml-4",
        t.children.map(child => li(encode(child))),
        display <-- openNodes.signal
          .map(_.contains(id))
          .map(if _ then "block" else "none")
      )
    )
  end encode

  private def index(t: Tree): (Map[Int, Tree], Map[Tree, Int]) =
    def go(
        next: Seq[Tree],
        n: Int,
        direct: Map[Int, Tree],
        reverse: Map[Tree, Int]
    ): (Map[Int, Tree], Map[Tree, Int]) =
      if next.isEmpty then (direct, reverse)
      else
        val ids = next.zipWithIndex.map { case (tree, idx) =>
          (n + idx) -> tree
        }.toMap
        val reverseIds = ids.map(_.swap).toMap
        go(
          next.flatMap(_.children),
          n + ids.size,
          direct ++ ids,
          reverse ++ reverseIds
        )
    end go
    go(Seq(t), 0, Map.empty, Map.empty)
  end index

end TreeView

def parse(s: String) =
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

  scala.meta.dialects.Scala3.apply(s).parse[Stat] match
    case x: Parsed.Success[Stat] =>
      TreeView(x.tree, openNodes).root
    case e: Parsed.Error =>
      p(s"ERROR: ${e.toString}")
end parse

@main def hello =
  val tg = Var(CODE)
  val parsed = tg.signal.map(parse)
  val app =
    div(
      cls := "content mx-auto w-10/12 bg-white/70 p-6 rounded-xl flex flex-col gap-4 h-full",
      h1("Scalameta AST explorer", cls := "text-4xl font-bold"),
      p(
        "This small webapp allows you to explore the AST of Scala code",
        cls := "text-sm"
      ),
      div(
        cls := "flex md:flex-col sm:flex-col lg:flex-row justify-baseline 2xl:flex-row gap-4 w-full",
        div(cls := "w-6/12 h-full", codeMirrorTextArea(tg)),
        div(cls := "w-6/12 h-full", p(code(pre(child <-- parsed))))
      )
    )

  renderOnDomContentLoaded(dom.document.getElementById("app"), app)
end hello

def codeMirrorTextArea(target: Var[String]) =
  textArea(
    cls := "h-full",
    onInput.mapToValue --> target,
    value <-- target,
    onMountCallback(el =>
      CodeMirror
        .fromTextArea(
          el.thisNode.ref,
          js.Dictionary(
            "value" -> target.now(),
            "lineNumbers" -> true,
            "mode" -> "text/x-scala"
          )
        )
        .on("change", value => target.set(value.getValue()))
    )
  )

val CODE =
  """
object X:
  class Test(a: Int):
    def hello = 25
"""
