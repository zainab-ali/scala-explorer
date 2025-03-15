import scala.annotation.tailrec
case class OffsetRange(from: Int, to: Int):
  override def toString: String = s"[$from-$to]"

trait IntervalTree[T]:
  def resolve(offset: Int): List[T]

private enum Tree:
  case Split(point: Int, left: Tree, right: Tree, in: List[OffsetRange])
  case Leaf(span: OffsetRange)
  case Empty

object IntervalTree:
  def construct[T](mp: Map[OffsetRange, T]): IntervalTree[T] =

    enum WorkType:
      case Process(work: List[OffsetRange])
      case Fuse(centerPoint: Int, overlapping: List[OffsetRange])

    var i = 0

    @tailrec
    def splitTR(
        workStack: List[WorkType],
        stack: List[Tree]
    ): Option[Tree] =
      i += 1
      assert(i < 10_000, "Too many iterations")
      // println(
      //   s"\n$i: workstack:\n  ${workStack
      //       .mkString("\n  ")}\nstack ():\n  ${stack.mkString("\n  ")}"
      // )
      workStack match
        case Nil => stack.headOption
        case WorkType.Fuse(centerPoint, overlapping) :: rest =>
          splitTR(
            rest,
            Tree.Split(
              centerPoint,
              stack.tail.head,
              stack.head,
              overlapping
            ) +: stack.drop(2)
          )
        case WorkType.Process(sortedSpans) :: rest =>
          if sortedSpans.size == 1 then
            splitTR(rest, Tree.Leaf(sortedSpans.head) +: stack)
          else if sortedSpans.size == 0 then splitTR(rest, Tree.Empty +: stack)
          else
            val start = sortedSpans.head.from
            val end = sortedSpans.last.to
            val centerPoint = (start + end) / 2
            // println("centerPoint (scheduling): " + centerPoint)

            val toTheLeft = List.newBuilder[OffsetRange]
            val toTheRight = List.newBuilder[OffsetRange]
            val overlapping = List.newBuilder[OffsetRange]

            sortedSpans.foreach { span =>
              if span.to < centerPoint then toTheLeft += span
              else if span.from > centerPoint then toTheRight += span
              else overlapping += span
            }

            // println(
            //   s"scheduling:\n  left: $toTheLeft\n  right: $toTheRight\n  overlapping: $overlapping"
            // )
            splitTR(
              WorkType.Process(toTheLeft.result()) ::
                WorkType.Process(toTheRight.result()) ::
                WorkType.Fuse(centerPoint, overlapping.result()) ::
                rest,
              stack
            )
      end match
    end splitTR

    def split(sortedSpans: List[OffsetRange]): Tree =
      val result =
        if sortedSpans.size == 1 then Tree.Leaf(sortedSpans.head)
        else if sortedSpans.size == 0 then Tree.Empty
        else
          val start = sortedSpans.head.from
          val end = sortedSpans.last.to
          val centerPoint = (start + end) / 2
          val toTheLeft = sortedSpans.takeWhile(_.to < centerPoint)
          val toTheRight = sortedSpans.dropWhile(_.from < centerPoint)
          val overlapping =
            sortedSpans.filter(s =>
              s.from <= centerPoint && s.to >= centerPoint
            )
          Tree.Split(
            centerPoint,
            split(toTheLeft),
            split(toTheRight),
            overlapping.toList
          )
      println(s"$sortedSpans -- $result")
      result
    end split

    val sorted =
      mp.keys.toList.sortBy(_.from)

    val data =
      splitTR(List(WorkType.Process(sorted)), Nil).getOrElse(Tree.Empty)

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

end IntervalTree
