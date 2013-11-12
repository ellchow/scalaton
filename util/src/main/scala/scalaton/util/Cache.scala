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

package scalaton.util.caching.mutable

import collection.JavaConversions._

import scalaz._
import Scalaz._

import com.googlecode.concurrentlinkedhashmap.{ConcurrentLinkedHashMap => CLHashMap}

trait Cache[K,V]{

  def apply(key: K): V

  def get(key: K): Option[V]

  def update(key: K, value: V): Cache[K,V]

  def delete(key: K): Option[V]

  def clear()

}

trait CLHashMapBacked[K,V]{
  private[caching] val cache: CLHashMap[K,Entry[V]]

  def clear() = cache.clear

  private[caching] def contains(key: K): Boolean =
    cache containsKey key
}

class LruCache[K,V](val maxCapacity: Int, val initialCapacity: Int = 16)
extends Cache[K,V]
with CLHashMapBacked[K,V]{

  private[caching] val cache = new CLHashMap.Builder[K, Entry[V]]
    .initialCapacity(initialCapacity)
    .maximumWeightedCapacity(maxCapacity)
    .build

  def apply(key: K): V =
    get(key) match {
      case Some(v) => v
      case _ => throw new java.util.NoSuchElementException("key not found: %s" format key)
    }

  def get(key: K): Option[V] = {
    val opt = (contains(key)) ? cache.get(key).some | none

    opt foreach ( _.touch() )

    opt map ( _.value )
  }

  def update(key: K, value: V): Cache[K,V] = {
    cache put (key, Entry[V](value))

    this
  }

  def delete(key: K): Option[V] = {
    val v = get(key)

    v foreach ( _ => cache remove key )

    v
  }

  def keySet = cache.keySet.toSet

}

class ExpiringLruCache[K,V](override val maxCapacity: Int, override val initialCapacity: Int,
                          val timeToLive: Int, val timeToIdle: Int,
                          val minTimeToSweep: Int, val maxTimeToSweep: Int,
                          val sizeToSweep: Double)
extends LruCache[K,V](maxCapacity, initialCapacity){

  private var sweepTime = System currentTimeMillis

  private val sweepSize = (sizeToSweep * maxCapacity) toInt

  override def get(key: K) = {
    val opt =
      if(contains(key)){
        if(!isExpired(key)){
          cache.get(key).some
        }else{
          cache remove key
          none
        }
      }else
        none

    opt foreach ( _.touch() )

    opt map ( _.value )
  }

  override def update(key: K, value: V): Cache[K,V] = {
    val currentTime = System currentTimeMillis

    if((currentTime - sweepTime) > maxTimeToSweep || // exceeds maximum time to sweep
       ((currentTime - sweepTime) > minTimeToSweep && (cache.size > sweepSize))) // exceeds minimum time to sweep and cache size exceeds threshold
      sweep()

    super.update(key, value)
  }

  private def sweep(){
    sweepTime = System currentTimeMillis

    cache.keySet foreach { key =>
      if(isExpired(key))
        cache remove key
    }
  }

  private def isExpired(key: K): Boolean = {
    val e = cache get key

    val currentTime = System currentTimeMillis

    ((currentTime - e.creationTime) > timeToLive) ||
    ((currentTime - e.accessTime) > timeToIdle)
  }

}

private[caching] case class Entry[V](val value: V){
  val creationTime = System currentTimeMillis

  @volatile var accessTime = System currentTimeMillis

  def touch(){
    accessTime = System currentTimeMillis
  }
}
