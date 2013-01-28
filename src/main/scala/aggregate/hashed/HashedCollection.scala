package scalaton.aggregate.hashed

import scalaz._
import Scalaz._

import scalaton.util._
import scalaton.util.hashable._


trait HashedCollectionConfig[A,H1,H2]{

  /** Number of hashes used in this collection **/
  val numHashes: Int

  /** Seed of hashing function **/
  val seed: Long

  /** Compute hash of an item **/
  def hashItem(item: A)(implicit h: Hashable[A, H1],
                        hconv: HashCodeConverter[H1, H2]): Iterable[H2 @@ HashCode] =
    hconv.convertSeq(multiHash(item, seed)) take numHashes


}

trait HashModdedCollectionConfig[A,H1] extends HashedCollectionConfig[A,H1,Int]{

  val width: Int

  override def hashItem(item: A)(implicit h: Hashable[A, H1],
                                 hconv: HashCodeConverter[H1, Int]): Iterable[Int @@ HashCode] =
    super.hashItem(item) map { _ % width |> HashCode}

}

trait HashedCollection[A,H1,H2,C <: HashedCollectionConfig[A,H1,H2]]{
  val conf: C
}

trait HashModdedCollection[A,H1,C <: HashModdedCollectionConfig[A,H1]] extends HashedCollection[A,H1,Int,C]


/** **/

trait HashedCollectionOperations[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]]{
  type H = Hashable[A,H1]
  type HC = HashCodeConverter[H1,Int]
}

trait InsertsElement[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]] extends HashedCollectionOperations[A,H1,H2,D,C]{
  def add(d: D, a: A)(implicit h: H, hconv: HC): D
}

trait InsertsElementFunction{
  def add[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]](d: D, a: A)(implicit i: InsertsElement[A,H1,H2,D,C], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D =
    i.add(d, a)
}

trait ChecksMembership[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]] extends HashedCollectionOperations[A,H1,H2,D,C]{
  def contains(d: D, a: A)(implicit h: H, hconv: HC): Boolean
}

trait ChecksMembershipFunction{
  def contains[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]](d: D, a: A)(implicit c: ChecksMembership[A,H1,H2,D,C], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): Boolean =
    c.contains(d,a)
}

trait Sized[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]] extends HashedCollectionOperations[A,H1,H2,D,C]{
  def cardinality(d: D): Long
}

trait SizedFunction{
  def cardinality[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2]](d: D)(implicit s: Sized[A,H1,H2,D,C], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): Long = s.cardinality(d)
}

trait UpdatesElementValue[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2],V1] extends HashedCollectionOperations[A,H1,H2,D,C]{
  def update(d: D, a: A, v1: V1)(implicit h: H, hconv: HC): D
}

trait UpdatesElementValueFunction{
  def update[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2],V1](d: D, a: A, v1: V1)(implicit u: UpdatesElementValue[A,H1,H2,D,C,V1], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): D =
    u.update(d,a,v1)
}

trait LooksUpElementValue[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2],V2] extends HashedCollectionOperations[A,H1,H2,D,C]{
  def lookup(d: D, a: A)(implicit h: H, hconv: HC): V2
}

trait LooksUpElementValueFunction{
  def lookup[A,H1,H2,D,C <: HashedCollectionConfig[A,H1,H2],V2](d: D, a: A)(implicit l: LooksUpElementValue[A,H1,H2,D,C,V2], h: Hashable[A,H1], hconv: HashCodeConverter[H1,Int]): V2 =
    l.lookup(d,a)
}





/*
/** Collections in which you can insert hashed items **/
trait HashedCollection[A,B,C,F]
with Monoid[F]

trait Insertable[A,B,C,F]
extends HashedCollection[A,B,C,F]{
  def insert(collection: F, item: A)(implicit h: Hashable[A, B],
                                     hconv: HashCodeConverter[B, C]): F
}

/** Can instantiate with a single item **/
trait MakesSingleton[A,B,C,F]
extends HashedCollection[A,B,C,F]
with Insertable[A,B,C,F] {
  def singleton(item: A)(implicit i: Insertable[A,B,C,F],
                         h: Hashable[A, B],
                         hconv: HashCodeConverter[B, C]): F =
    insert(zero, item)
}

/** Can check for existence of an item **/
trait MapLike[A,B,C,T,R,F]
extends HashedCollection[A,B,C,F]{
  def get(collection: F, item: A)(implicit v: Value[T,R],
                                  h: Hashable[A, B],
                                  hconv: HashCodeConverter[B, C]): R

  /** take item A, find its T in F, update and insert**/
  def update(collection: F, item: A, u: T)(implicit mon: Monoid[T],
                                           h: Hashable[A, B],
                                           hconv: HashCodeConverter[B, C]): F
}

trait MakesSingletonM[A,B,C,T,R,F]
extends MapLike[A,B,C,T,R,F] {
  def singleton(item: A, u: T)(implicit mon: Monoid[T],
                               h: Hashable[A, B],
                               hconv: HashCodeConverter[B, C]): F =
    update(zero, item, u)
}

/** Can check for existence of an item **/
trait SetLike[A,B,C,F]
extends HashedCollection[A,B,C,F]{
  def contains(collection: F, item: A)(implicit h: Hashable[A, B],
                                       hconv: HashCodeConverter[B, C]): Boolean
}

/** Can check the size/cardinality **/
trait Sized[F]{
  def cardinality(collection: F): Long
}



/**
 * Functions for operating on hashed collections
 **/

trait HashedCollectionFunctions{
  def insert[A,B,C,T,F](collection: F, item: A)(implicit i: Insertable[A,B,C,F],
                                                h: Hashable[A, B],
                                                hconv: HashCodeConverter[B, C]) =
    i.insert(collection, item)
}



trait MakesSingletonFunctions{
  def singleton[A,B,C,F](item: A)(implicit ms: MakesSingleton[A,B,C,F],
                                  h: Hashable[A, B],
                                  hconv: HashCodeConverter[B, C]): F =
    ms.singleton(item)
}

trait MakesSingletonMFunctions{
  def singleton[A,B,C,T,R,F](item: A, u: T)(implicit ms: MakesSingletonM[A,B,C,T,R,F],
                                            mon: Monoid[T],
                                            h: Hashable[A, B],
                                            hconv: HashCodeConverter[B, C]): F =
    ms.singleton(item, u)
}

trait SetLikeFunctions{
  def contains[A,B,C,F](collection: F, item: A)(implicit c: SetLike[A,B,C,F],
                                                h: Hashable[A, B],
                                                hconv: HashCodeConverter[B, C]) =
    c.contains(collection, item)
}

trait MapLikeFunctions{
  def lookup[A,B,C,T,R,F](collection: F, item: A)(implicit m: MapLike[A,B,C,T,R,F],
                                               v: Value[T,R],
                                               h: Hashable[A, B],
                                               hconv: HashCodeConverter[B, C]) =
    m.get(collection, item)

  def update[A,B,C,T,R,F](collection: F, item: A, u: T)(implicit m: MapLike[A,B,C,T,R,F],
                                                        mon: Monoid[T],
                                                        h: Hashable[A, B],
                                                        hconv: HashCodeConverter[B, C]) =
    m.update(collection, item, u)
}

trait SizedFunctions{
  def cardinality[F](collection: F)(implicit s: Sized[F]): Long =
    s.cardinality(collection)
}
*/
