import scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement

import scala.meta.*
class TreeView(t: Tree):
  val openNodes = Var(Set.empty[Int])
  val toggle = openNodes.updater[Int]: (cur, next) =>
    if cur(next) then cur - next else cur + next

  private def encode(t: Tree): Element =
    val id = reverse(t)
    ul(
      a(
        href := "#",
        onClick.preventDefault.mapToStrict(id) --> toggle,
        t.productPrefix
      ),
      div(
        t.children.map(child => li(encode(child))),
        display <-- openNodes.signal
          .map(_.contains(id))
          .map(if _ then "block" else "none")
      )
    )
  end encode
  lazy val root = encode(t)
  lazy val (direct, reverse) = index(t)

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
  scala.meta.dialects.Scala3.apply(s).parse[Stat] match
    case x: Parsed.Success[Stat] =>
      TreeView(x.tree).root
    // index(x.tree)._1.toSeq.sortBy(_._1).mkString("\n\n").toString
    case e: Parsed.Error =>
      p(s"ERROR: ${e.toString}")
end parse

@main def hello =
  val tg = Var(CODE)
  val parsed = tg.signal.map(parse)
  val app = div(codeMirrorTextArea(tg), p(code(pre(child <-- parsed))))

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
