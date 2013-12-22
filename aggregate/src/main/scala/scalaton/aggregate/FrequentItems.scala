/*
 Copyright 2013 Elliot Chow

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package scalaton.aggregate

import scala.collection.mutable
import scala.collection.mutable.{ DoubleLinkedList => DLL }

/* heavy hitters algo described in https://www.cs.ucsb.edu/research/tech_reports/reports/2005-23.pdf using a slightly modified data structure */
trait StreamSummaryModule {

  private def connect[A](xs: DLL[A], ys: DLL[A]) = {
    if (xs.isEmpty) {
      xs.elem = ys.elem
      xs.next = ys.next
      ys.prev = xs
    } else {
      xs.next = ys
      ys.prev = xs
    }
    xs
  }

  case class StreamSummaryEntry[A](val key: A, private[aggregate] var _count: Long, private[aggregate] var _error: Long) {
    def count = _count

    def error = _error
  }

  case class StreamSummary[A] private[aggregate] (
    val capacity: Int,
    private val lookup: mutable.Map[A,DLL[Option[StreamSummaryEntry[A]]]],
    private val sentinel: DLL[Option[StreamSummaryEntry[A]]],
    private var _min: DLL[Option[StreamSummaryEntry[A]]]
  ) {

    def get(key: A) = for {
      dll <- lookup.get(key)
      entry <- dll.elem
    } yield entry

    def top(k: Int) = elements.take(k)

    def elements = sentinel.toStream.flatten

    def size = elements.map(_.count).sum

    def insert(key: A, count: Long = 1L): StreamSummary[A] = {
      require(count > 0)
      if (lookup.contains(key)) {
        val dll = lookup(key)
        val entry = dll.elem.get
        entry._count += count
        percolateLeft(dll)
        fixMin()
      } else {
        if (lookup.size < capacity) {
          val entry = StreamSummaryEntry(key, count, 0)
          val dll = DLL(Option(entry))
          connect(_min, dll)
          _min = dll
          lookup += key -> dll
          percolateLeft(dll)
          fixMin()
        } else {
          val currentMin = _min.elem.get
          val entry = StreamSummaryEntry(key, currentMin.count + count, currentMin.count)
          val dll = DLL(Option(entry))
          lookup += key -> dll
          lookup -= currentMin.key
          connect(_min.prev, dll)
          _min = dll
          percolateLeft(dll)
          fixMin()
        }
      }

      this
    }

    private def percolateLeft(dll: DLL[Option[StreamSummaryEntry[A]]]): Unit = {
      val prev = dll.prev
      val next = dll.next

      if (prev.elem.nonEmpty && (prev.elem.get.count < dll.elem.get.count)) {
        val prev2 = prev.prev

        connect(prev2, dll)
        connect(prev, next)
        connect(dll, prev)

        percolateLeft(dll)
      }
    }

    private def fixMin(): Unit = if(_min.next.nonEmpty){
      _min = _min.next
      fixMin()
    }

  }

  object StreamSummary{
    def empty[A](capacity: Int) = {
      val init = DLL[Option[StreamSummaryEntry[A]]](None)
      StreamSummary[A](capacity, mutable.Map.empty, init, init)
    }

    def fromData[A](capacity: Int, as: Iterable[A]) = as.foldLeft(empty[A](capacity))((ss, a) => ss.insert(a))
  }
}

object freqitems extends StreamSummaryModule
