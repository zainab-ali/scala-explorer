import scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.airstream.web.WebStorageVar

import scala.meta.*
import scala.util.Try
import scala.annotation.tailrec

case class TreeView(
    tree: Tree,
    textIndex: TextIndex,
    openNodes: Var[Set[Int]],
    cursor: Var[CodeMirrorCursor],
    hover: Var[Option[Int]]
):
  private val toggle = openNodes.updater[Int]: (cur, next) =>
    if cur(next) then cur - next else cur + next

  private val append = openNodes.updater[Set[Int]]: (cur, next) =>
    cur ++ next

  private def isToggled(id: Int) = openNodes.signal.map(_.contains(id))
  private def isOnPath(id: Int) = path.signal.map(_.contains(id))

  private val path = Var(List.empty[Int])

  private lazy val deepestTreeUnderCursor =
    cursor.signal.map: cursor =>
      textIndex
        .posLookup(cursor.line, cursor.ch)
        .map: offset =>
          val deepest = intervalTree
            .resolve(offset)
            .sortBy: treeId =>
              offset - direct(treeId).pos.start

          deepest.headOption match
            case None => Nil
            case Some(value) =>
              @tailrec
              def go(id: Int, cur: List[Int]): List[Int] =
                direct.get(id).flatMap(_.parent).flatMap(reverse.get) match
                  case None         => cur
                  case Some(parent) => go(parent, parent :: cur)

              go(value, List(value)).reverse
          end match
        .getOrElse(Nil)

  lazy val root = div(
    cls := "bg-gray-100 p-4 rounded-lg shadow-md",
    deepestTreeUnderCursor --> path,
    div(
      cls := "flex flex-row gap-2 my-2",
      a(
        href := "#",
        cls := "bg-gray-200 hover:bg-gray-300 rounded-md px-2 py-1 text-xs",
        "collapse all",
        onClick.mapToStrict(Set.empty) --> openNodes,
        onClick.mapToStrict(List.empty) --> path
      ),
      a(
        href := "#",
        cls := "bg-gray-200 hover:bg-gray-300 rounded-md px-2 py-1 text-xs",
        "expand all",
        onClick.mapTo(direct.keySet) --> openNodes
      )
    ),
    code(encode(tree))
  )
  lazy val (direct, reverse) = index(tree)

  lazy val intervalTree = IntervalTree.construct(reverse.map: (tree, id) =>
    OffsetRange(tree.pos.start, tree.pos.end) -> id)

  def getTree(id: Int): Option[Tree] =
    direct.get(id)

  private def getPath(t: Tree): Set[Int] =
    val path = List.newBuilder[Int]
    var current = t
    while current.parent.isDefined do
      current = current.parent.get
      path += reverse(current)
    path.result().toSet

  private def encode(t: Tree): Element =
    val id = reverse(t)
    span(
      span(
        cls.toggle("bg-amber-500") <-- isOnPath(id),
        a(
          cls := "text-blue-700 text-sm",
          href := "#",
          child.text <-- isToggled(id).map: b =>
            val moniker =
              if t.children.isEmpty then "  " else if b then "- " else "+ "
            moniker + t.productPrefix
          ,
          onClick.preventDefault.mapToStrict(id) --> toggle,
          onMouseOver.mapToStrict(id) --> hover.someWriter,
          onMouseOut.mapToStrict(None) --> hover
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
          .combineWithFn(path)(_ ++ _)
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
