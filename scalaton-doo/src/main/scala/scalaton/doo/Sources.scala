package scalaton.doo

import com.nicta.scoobi.Scoobi._

import org.apache.hadoop.io.Text

import scalaz.{DList => _, _}
import Scalaz._
import Validation.fromTryCatch

import implicits._

trait Sources{
  def safeTextToWFOption[A,B : Manifest : WireFormat](path: String, toObject: String => A, f: A => B) =
    fromSequenceFile[Text, Text](path) map { case(key, value) =>
                                             val parsed = for{
                                               obj <- fromTryCatch(toObject(value toString)) fold (
                                                 f => "failed to instantiate object from text".failureNel[A],
                                                 s => s.successNel[String]
                                               )
                                               data <- fromTryCatch(f(obj)) fold (
                                                 f => "failed to extract data from %s".format(obj toString).failureNel[B],
                                                 s => s.successNel[String]
                                               )
                                             } yield data

                                             parsed toOption
                                           }

  def safeTextToWF[A,B : Manifest : WireFormat](path: String, toObject: String => A, f: A => B) =
    safeTextToWFOption(path, toObject, f) mapFlatten (x => x)

}

object source extends Sources
