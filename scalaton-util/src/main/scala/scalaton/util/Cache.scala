package scalaton.util.caching

import scalaz._
import Scalaz._

import com.googlecode.concurrentlinkedhashmap.{ConcurrentLinkedHashMap => CLHashMap}

trait Cache[V]{

  def apply(key: Any): V

  def get(key: Any): Option[V]

  def update(key: Any, value: V): Cache[V]

  def delete(key: Any): Option[V]

  def clear()

}

trait CLHashMapBacked[V]{
  private[caching] val cache: CLHashMap[Any,Entry[V]]

  def clear() = cache.clear

  private[caching] def contains(key: Any): Boolean =
    cache containsKey key
}

class LruCache[V](val maxCapacity: Int, val initialCapacity: Int = 16)
extends Cache[V]
with CLHashMapBacked[V]{

  private[caching] val cache = new CLHashMap.Builder[Any, Entry[V]]
    .initialCapacity(initialCapacity)
    .maximumWeightedCapacity(maxCapacity)
    .build

  def apply(key: Any): V =
    get(key) match {
      case Some(v) => v
      case _ => throw new java.util.NoSuchElementException("key not found: %s" format key)
    }

  def get(key: Any): Option[V] = {
    val opt = (contains(key)) ? cache.get(key).some | none

    opt map ( e => e.accessTime = System.currentTimeMillis )

    opt foreach (e =>  println(e.accessTime) )

    opt map ( _.value )
  }

  def update(key: Any, value: V): Cache[V] = {
    cache put (key, Entry[V](value))

    this
  }

  def delete(key: Any): Option[V] = {
    val v = get(key)

    v map ( _ => cache remove key )

    v
  }

}

class ExpiringLruCache[V](override val maxCapacity: Int, override val initialCapacity: Int = 16,
                          val timeToLive: Int = Int.MaxValue, val timeToIdle: Int = Int.MaxValue)
extends LruCache[V](maxCapacity, initialCapacity){

  override def get(key: Any) = {
    val opt = (contains(key) && !isExpired(key)) ? cache.get(key).some | none

    opt map ( e => e.accessTime = System.currentTimeMillis )

    opt foreach (e =>  println(e.accessTime) )

    opt map ( _.value )
  }

  private def isExpired(key: Any): Boolean = {
    val e = cache get key

    val currentTime = System.currentTimeMillis

    ((currentTime - e.creationTime) > timeToLive) ||
    ((currentTime - e.accessTime) > timeToIdle)
  }

}

private[caching] case class Entry[V](val value: V){
  val creationTime = System.currentTimeMillis
  var accessTime = System.currentTimeMillis

  def touch(){
    accessTime = System.currentTimeMillis
  }
}
