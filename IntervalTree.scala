case class OffsetRange(from: Int, to: Int)

trait IntervalTree[T]:
  def resolve(offset: Int): List[T]

object IntervalTree:
  def construct[T](mp: Map[OffsetRange, T]): IntervalTree[T] =
    def split(sortedSpans: Vector[OffsetRange]): Tree =
      if sortedSpans.size == 1 then Tree.Leaf(sortedSpans.head)
      else if sortedSpans.size == 0 then Tree.Empty
      else
        val start = sortedSpans.head.from
        val end = sortedSpans.last.to
        val centerPoint = (start + end) / 2
        val toTheLeft = sortedSpans.takeWhile(_.to < centerPoint)
        val toTheRight = sortedSpans.dropWhile(_.from < centerPoint)
        val overlapping =
          sortedSpans.filter(s => s.from <= centerPoint && s.to >= centerPoint)
        Tree.Split(
          centerPoint,
          split(toTheLeft),
          split(toTheRight),
          overlapping.toList
        )

    val sorted =
      mp.keys.toVector.sortBy(_.from)

    val data = split(sorted)

    Impl(data, mp)
  end construct

  private class Impl[T](tree: Tree, mp: Map[OffsetRange, T])
      extends IntervalTree[T]:
    override def resolve(offset: Int): List[T] =
      import Tree.*
      def go(t: Tree): List[OffsetRange] =
        t match
          case Split(point, left, right, in) =>
            if offset == point then in
            else if offset > point then in.filter(_.to >= offset) ++ go(right)
            else if offset < point then in.filter(_.from <= offset) ++ go(left)
            else Nil
          case Leaf(span) =>
            if span.from > offset || span.to < offset
            then Nil
            else List(span)
          case Empty => Nil

      go(tree).flatMap(mp.get)
    end resolve
  end Impl

  private enum Tree:
    case Split(point: Int, left: Tree, right: Tree, in: List[OffsetRange])
    case Leaf(span: OffsetRange)
    case Empty
end IntervalTree
