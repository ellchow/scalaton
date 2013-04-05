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

package scalaton.util

case class ProductFlatIterator(val p: Product) extends Iterator[Any]{
  private val buf = collection.mutable.Stack(p.productIterator.toSeq : _*)

  def hasNext = buf.nonEmpty

  def next = {
    if(!hasNext) throw new java.util.NoSuchElementException("no more elements")
      do{
        buf.head match{
          case ep: Product => {
            buf.pop
            buf.pushAll(ep.productIterator.toSeq.reverse)
          }
          case _ => None
        }
      }while(buf.head.isInstanceOf[Product])

    buf.pop
  }
}
