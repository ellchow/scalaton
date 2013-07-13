package scalaton.doo


import com.nicta.scoobi.Scoobi._

import implicits._

import scalaz.{DList => _, _}
import Scalaz._

object Test extends ScoobiApp{
  def run() = {
    val a = DList(1,2,3,4,5).map(x =>  (x % 2, x)).checkpointToSequenceFile("test-cp", false)

    val b = a.groupByKey.combine((_: Int) |+| (_: Int))

    persist(b.toDelimitedTextFile("test-out", overwrite = true))

  }
}
