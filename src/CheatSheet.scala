import com.raquo.laminar.api.L.*

import scala.annotation.tailrec
import scala.meta.*
import com.raquo.airstream.core.Signal

class CheatSheet(
    tree: Tree,
    cursor: Var[CodeMirrorCursor],
    hover: Var[Option[Int]]
):

  def getTree(id: Int): Option[Tree] =
    None

  val items = CheatFinder.findCheats(tree)
  lazy val node = div(
    cls := "bg-gray-100 p-2 rounded-lg shadow-md w-full",
    ol(
      items.map(item =>
        li(
          cls := "grid grid-cols-[2fr_3fr_1fr]",
          code(item.code),
          p(item.description),
          a(href := item.link, target := "_blank", "Read more.")
        )
      )
    )
  )
end CheatSheet

case class Cheat(
    code: String,
    description: String,
    link: String
)
