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

  val path = Var(List.empty[Int])

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
    path.signal.map(_.toSet) --> append,
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
        cls <-- path.signal.map(p =>
          if p.headOption.contains(id) then
            "bg-amber-500"
          else if p.contains(id) then
            "bg-amber-100"
          else ""
        ),
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
