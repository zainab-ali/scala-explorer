import scala.collection.SortedMap

case class TextIndex private (lines: SortedMap[Int, OffsetRange], text: String):
  private val lookupLineByOffset: IntervalTree[Int] =
    IntervalTree.construct(lines.toMap.map(_.swap))

  def posLookup(line: Int, col: Int): Option[Int] =
    lines.get(line).map { range =>
      range.from + col
    }

  def lineSpan(line: Int): Option[OffsetRange] =
    lines.get(line)

  def lineOf(offset: Int): Option[OffsetRange] =
    val candidates = lookupLineByOffset.resolve(offset)
    candidates.headOption.flatMap(lines.get)
end TextIndex

object TextIndex:
  def construct(text: String): TextIndex =
    val m =
      text.linesWithSeparators.zipWithIndex.foldLeft(
        0 -> List.empty[OffsetRange]
      ) { case ((offset, spans), (line, lineIdx)) =>
        val length = line.length - line.count(c => c == '\n')

        ((offset + line.length), OffsetRange(offset, offset + length) :: spans)
      }

    TextIndex(
      SortedMap(
        m._2.reverse.zipWithIndex.map(_.swap)*
      ),
      text
    )
  end construct
end TextIndex
