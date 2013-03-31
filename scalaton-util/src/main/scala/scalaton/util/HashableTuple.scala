package scalaton.util

import scalaz._
import Scalaz._

/**
  * Implicits for Hashable Tuples
  * DO NOT EDIT - code generated on 2013-03-30T20:07:41.131. see scalaton/codegen/HashableTupleGen.scala
  **/
  
trait HashableTuple32Instances extends HashFuncs{
  implicit def tuple1Hashable32[A1](implicit h1: Hashable[A1,Int]) = new Hashable[Tuple1[A1],Int]{
      def digest(a: Tuple1[A1], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      HashCode(current)
      }
  }
  implicit def tuple2Hashable32[A1,A2](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int]) = new Hashable[Tuple2[A1,A2],Int]{
      def digest(a: Tuple2[A1,A2], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      HashCode(current)
      }
  }
  implicit def tuple3Hashable32[A1,A2,A3](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int]) = new Hashable[Tuple3[A1,A2,A3],Int]{
      def digest(a: Tuple3[A1,A2,A3], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      HashCode(current)
      }
  }
  implicit def tuple4Hashable32[A1,A2,A3,A4](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int]) = new Hashable[Tuple4[A1,A2,A3,A4],Int]{
      def digest(a: Tuple4[A1,A2,A3,A4], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      HashCode(current)
      }
  }
  implicit def tuple5Hashable32[A1,A2,A3,A4,A5](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int]) = new Hashable[Tuple5[A1,A2,A3,A4,A5],Int]{
      def digest(a: Tuple5[A1,A2,A3,A4,A5], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      HashCode(current)
      }
  }
  implicit def tuple6Hashable32[A1,A2,A3,A4,A5,A6](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int]) = new Hashable[Tuple6[A1,A2,A3,A4,A5,A6],Int]{
      def digest(a: Tuple6[A1,A2,A3,A4,A5,A6], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      HashCode(current)
      }
  }
  implicit def tuple7Hashable32[A1,A2,A3,A4,A5,A6,A7](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int]) = new Hashable[Tuple7[A1,A2,A3,A4,A5,A6,A7],Int]{
      def digest(a: Tuple7[A1,A2,A3,A4,A5,A6,A7], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      HashCode(current)
      }
  }
  implicit def tuple8Hashable32[A1,A2,A3,A4,A5,A6,A7,A8](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int]) = new Hashable[Tuple8[A1,A2,A3,A4,A5,A6,A7,A8],Int]{
      def digest(a: Tuple8[A1,A2,A3,A4,A5,A6,A7,A8], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      HashCode(current)
      }
  }
  implicit def tuple9Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int]) = new Hashable[Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9],Int]{
      def digest(a: Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      HashCode(current)
      }
  }
  implicit def tuple10Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int]) = new Hashable[Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10],Int]{
      def digest(a: Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      HashCode(current)
      }
  }
  implicit def tuple11Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int]) = new Hashable[Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11],Int]{
      def digest(a: Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      HashCode(current)
      }
  }
  implicit def tuple12Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int]) = new Hashable[Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12],Int]{
      def digest(a: Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      HashCode(current)
      }
  }
  implicit def tuple13Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int]) = new Hashable[Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13],Int]{
      def digest(a: Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      HashCode(current)
      }
  }
  implicit def tuple14Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int]) = new Hashable[Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14],Int]{
      def digest(a: Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      HashCode(current)
      }
  }
  implicit def tuple15Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int]) = new Hashable[Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15],Int]{
      def digest(a: Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      HashCode(current)
      }
  }
  implicit def tuple16Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int]) = new Hashable[Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16],Int]{
      def digest(a: Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      HashCode(current)
      }
  }
  implicit def tuple17Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int]) = new Hashable[Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17],Int]{
      def digest(a: Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      HashCode(current)
      }
  }
  implicit def tuple18Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int], h18: Hashable[A18,Int]) = new Hashable[Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18],Int]{
      def digest(a: Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      current = combine32Hashes(current, h18.digest(a._18, seed))
      HashCode(current)
      }
  }
  implicit def tuple19Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int], h18: Hashable[A18,Int], h19: Hashable[A19,Int]) = new Hashable[Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19],Int]{
      def digest(a: Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      current = combine32Hashes(current, h18.digest(a._18, seed))
      current = combine32Hashes(current, h19.digest(a._19, seed))
      HashCode(current)
      }
  }
  implicit def tuple20Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int], h18: Hashable[A18,Int], h19: Hashable[A19,Int], h20: Hashable[A20,Int]) = new Hashable[Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20],Int]{
      def digest(a: Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      current = combine32Hashes(current, h18.digest(a._18, seed))
      current = combine32Hashes(current, h19.digest(a._19, seed))
      current = combine32Hashes(current, h20.digest(a._20, seed))
      HashCode(current)
      }
  }
  implicit def tuple21Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int], h18: Hashable[A18,Int], h19: Hashable[A19,Int], h20: Hashable[A20,Int], h21: Hashable[A21,Int]) = new Hashable[Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21],Int]{
      def digest(a: Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      current = combine32Hashes(current, h18.digest(a._18, seed))
      current = combine32Hashes(current, h19.digest(a._19, seed))
      current = combine32Hashes(current, h20.digest(a._20, seed))
      current = combine32Hashes(current, h21.digest(a._21, seed))
      HashCode(current)
      }
  }
  implicit def tuple22Hashable32[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit h1: Hashable[A1,Int], h2: Hashable[A2,Int], h3: Hashable[A3,Int], h4: Hashable[A4,Int], h5: Hashable[A5,Int], h6: Hashable[A6,Int], h7: Hashable[A7,Int], h8: Hashable[A8,Int], h9: Hashable[A9,Int], h10: Hashable[A10,Int], h11: Hashable[A11,Int], h12: Hashable[A12,Int], h13: Hashable[A13,Int], h14: Hashable[A14,Int], h15: Hashable[A15,Int], h16: Hashable[A16,Int], h17: Hashable[A17,Int], h18: Hashable[A18,Int], h19: Hashable[A19,Int], h20: Hashable[A20,Int], h21: Hashable[A21,Int], h22: Hashable[A22,Int]) = new Hashable[Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22],Int]{
      def digest(a: Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22], seed: Long): Int @@ HashCode = {
      var current: Int = 1
      current = combine32Hashes(current, h1.digest(a._1, seed))
      current = combine32Hashes(current, h2.digest(a._2, seed))
      current = combine32Hashes(current, h3.digest(a._3, seed))
      current = combine32Hashes(current, h4.digest(a._4, seed))
      current = combine32Hashes(current, h5.digest(a._5, seed))
      current = combine32Hashes(current, h6.digest(a._6, seed))
      current = combine32Hashes(current, h7.digest(a._7, seed))
      current = combine32Hashes(current, h8.digest(a._8, seed))
      current = combine32Hashes(current, h9.digest(a._9, seed))
      current = combine32Hashes(current, h10.digest(a._10, seed))
      current = combine32Hashes(current, h11.digest(a._11, seed))
      current = combine32Hashes(current, h12.digest(a._12, seed))
      current = combine32Hashes(current, h13.digest(a._13, seed))
      current = combine32Hashes(current, h14.digest(a._14, seed))
      current = combine32Hashes(current, h15.digest(a._15, seed))
      current = combine32Hashes(current, h16.digest(a._16, seed))
      current = combine32Hashes(current, h17.digest(a._17, seed))
      current = combine32Hashes(current, h18.digest(a._18, seed))
      current = combine32Hashes(current, h19.digest(a._19, seed))
      current = combine32Hashes(current, h20.digest(a._20, seed))
      current = combine32Hashes(current, h21.digest(a._21, seed))
      current = combine32Hashes(current, h22.digest(a._22, seed))
      HashCode(current)
      }
  }
}
trait HashableTuple64Instances extends HashFuncs{
  implicit def tuple1Hashable64[A1](implicit h1: Hashable[A1,Long]) = new Hashable[Tuple1[A1],Long]{
      def digest(a: Tuple1[A1], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      HashCode(current)
      }
  }
  implicit def tuple2Hashable64[A1,A2](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long]) = new Hashable[Tuple2[A1,A2],Long]{
      def digest(a: Tuple2[A1,A2], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      HashCode(current)
      }
  }
  implicit def tuple3Hashable64[A1,A2,A3](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long]) = new Hashable[Tuple3[A1,A2,A3],Long]{
      def digest(a: Tuple3[A1,A2,A3], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      HashCode(current)
      }
  }
  implicit def tuple4Hashable64[A1,A2,A3,A4](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long]) = new Hashable[Tuple4[A1,A2,A3,A4],Long]{
      def digest(a: Tuple4[A1,A2,A3,A4], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      HashCode(current)
      }
  }
  implicit def tuple5Hashable64[A1,A2,A3,A4,A5](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long]) = new Hashable[Tuple5[A1,A2,A3,A4,A5],Long]{
      def digest(a: Tuple5[A1,A2,A3,A4,A5], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      HashCode(current)
      }
  }
  implicit def tuple6Hashable64[A1,A2,A3,A4,A5,A6](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long]) = new Hashable[Tuple6[A1,A2,A3,A4,A5,A6],Long]{
      def digest(a: Tuple6[A1,A2,A3,A4,A5,A6], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      HashCode(current)
      }
  }
  implicit def tuple7Hashable64[A1,A2,A3,A4,A5,A6,A7](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long]) = new Hashable[Tuple7[A1,A2,A3,A4,A5,A6,A7],Long]{
      def digest(a: Tuple7[A1,A2,A3,A4,A5,A6,A7], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      HashCode(current)
      }
  }
  implicit def tuple8Hashable64[A1,A2,A3,A4,A5,A6,A7,A8](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long]) = new Hashable[Tuple8[A1,A2,A3,A4,A5,A6,A7,A8],Long]{
      def digest(a: Tuple8[A1,A2,A3,A4,A5,A6,A7,A8], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      HashCode(current)
      }
  }
  implicit def tuple9Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long]) = new Hashable[Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9],Long]{
      def digest(a: Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      HashCode(current)
      }
  }
  implicit def tuple10Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long]) = new Hashable[Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10],Long]{
      def digest(a: Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      HashCode(current)
      }
  }
  implicit def tuple11Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long]) = new Hashable[Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11],Long]{
      def digest(a: Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      HashCode(current)
      }
  }
  implicit def tuple12Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long]) = new Hashable[Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12],Long]{
      def digest(a: Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      HashCode(current)
      }
  }
  implicit def tuple13Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long]) = new Hashable[Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13],Long]{
      def digest(a: Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      HashCode(current)
      }
  }
  implicit def tuple14Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long]) = new Hashable[Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14],Long]{
      def digest(a: Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      HashCode(current)
      }
  }
  implicit def tuple15Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long]) = new Hashable[Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15],Long]{
      def digest(a: Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      HashCode(current)
      }
  }
  implicit def tuple16Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long]) = new Hashable[Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16],Long]{
      def digest(a: Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      HashCode(current)
      }
  }
  implicit def tuple17Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long]) = new Hashable[Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17],Long]{
      def digest(a: Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      HashCode(current)
      }
  }
  implicit def tuple18Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long], h18: Hashable[A18,Long]) = new Hashable[Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18],Long]{
      def digest(a: Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      current = combine64Hashes(current, h18.digest(a._18, seed))
      HashCode(current)
      }
  }
  implicit def tuple19Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long], h18: Hashable[A18,Long], h19: Hashable[A19,Long]) = new Hashable[Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19],Long]{
      def digest(a: Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      current = combine64Hashes(current, h18.digest(a._18, seed))
      current = combine64Hashes(current, h19.digest(a._19, seed))
      HashCode(current)
      }
  }
  implicit def tuple20Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long], h18: Hashable[A18,Long], h19: Hashable[A19,Long], h20: Hashable[A20,Long]) = new Hashable[Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20],Long]{
      def digest(a: Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      current = combine64Hashes(current, h18.digest(a._18, seed))
      current = combine64Hashes(current, h19.digest(a._19, seed))
      current = combine64Hashes(current, h20.digest(a._20, seed))
      HashCode(current)
      }
  }
  implicit def tuple21Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long], h18: Hashable[A18,Long], h19: Hashable[A19,Long], h20: Hashable[A20,Long], h21: Hashable[A21,Long]) = new Hashable[Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21],Long]{
      def digest(a: Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      current = combine64Hashes(current, h18.digest(a._18, seed))
      current = combine64Hashes(current, h19.digest(a._19, seed))
      current = combine64Hashes(current, h20.digest(a._20, seed))
      current = combine64Hashes(current, h21.digest(a._21, seed))
      HashCode(current)
      }
  }
  implicit def tuple22Hashable64[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit h1: Hashable[A1,Long], h2: Hashable[A2,Long], h3: Hashable[A3,Long], h4: Hashable[A4,Long], h5: Hashable[A5,Long], h6: Hashable[A6,Long], h7: Hashable[A7,Long], h8: Hashable[A8,Long], h9: Hashable[A9,Long], h10: Hashable[A10,Long], h11: Hashable[A11,Long], h12: Hashable[A12,Long], h13: Hashable[A13,Long], h14: Hashable[A14,Long], h15: Hashable[A15,Long], h16: Hashable[A16,Long], h17: Hashable[A17,Long], h18: Hashable[A18,Long], h19: Hashable[A19,Long], h20: Hashable[A20,Long], h21: Hashable[A21,Long], h22: Hashable[A22,Long]) = new Hashable[Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22],Long]{
      def digest(a: Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22], seed: Long): Long @@ HashCode = {
      var current: Long = 1L
      current = combine64Hashes(current, h1.digest(a._1, seed))
      current = combine64Hashes(current, h2.digest(a._2, seed))
      current = combine64Hashes(current, h3.digest(a._3, seed))
      current = combine64Hashes(current, h4.digest(a._4, seed))
      current = combine64Hashes(current, h5.digest(a._5, seed))
      current = combine64Hashes(current, h6.digest(a._6, seed))
      current = combine64Hashes(current, h7.digest(a._7, seed))
      current = combine64Hashes(current, h8.digest(a._8, seed))
      current = combine64Hashes(current, h9.digest(a._9, seed))
      current = combine64Hashes(current, h10.digest(a._10, seed))
      current = combine64Hashes(current, h11.digest(a._11, seed))
      current = combine64Hashes(current, h12.digest(a._12, seed))
      current = combine64Hashes(current, h13.digest(a._13, seed))
      current = combine64Hashes(current, h14.digest(a._14, seed))
      current = combine64Hashes(current, h15.digest(a._15, seed))
      current = combine64Hashes(current, h16.digest(a._16, seed))
      current = combine64Hashes(current, h17.digest(a._17, seed))
      current = combine64Hashes(current, h18.digest(a._18, seed))
      current = combine64Hashes(current, h19.digest(a._19, seed))
      current = combine64Hashes(current, h20.digest(a._20, seed))
      current = combine64Hashes(current, h21.digest(a._21, seed))
      current = combine64Hashes(current, h22.digest(a._22, seed))
      HashCode(current)
      }
  }
}
trait HashableTuple128Instances extends HashFuncs{
  implicit def tuple1Hashable128[A1](implicit h1: Hashable[A1,(Long, Long)]) = new Hashable[Tuple1[A1],(Long, Long)]{
      def digest(a: Tuple1[A1], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      HashCode(current)
      }
  }
  implicit def tuple2Hashable128[A1,A2](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)]) = new Hashable[Tuple2[A1,A2],(Long, Long)]{
      def digest(a: Tuple2[A1,A2], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      HashCode(current)
      }
  }
  implicit def tuple3Hashable128[A1,A2,A3](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)]) = new Hashable[Tuple3[A1,A2,A3],(Long, Long)]{
      def digest(a: Tuple3[A1,A2,A3], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      HashCode(current)
      }
  }
  implicit def tuple4Hashable128[A1,A2,A3,A4](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)]) = new Hashable[Tuple4[A1,A2,A3,A4],(Long, Long)]{
      def digest(a: Tuple4[A1,A2,A3,A4], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      HashCode(current)
      }
  }
  implicit def tuple5Hashable128[A1,A2,A3,A4,A5](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)]) = new Hashable[Tuple5[A1,A2,A3,A4,A5],(Long, Long)]{
      def digest(a: Tuple5[A1,A2,A3,A4,A5], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      HashCode(current)
      }
  }
  implicit def tuple6Hashable128[A1,A2,A3,A4,A5,A6](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)]) = new Hashable[Tuple6[A1,A2,A3,A4,A5,A6],(Long, Long)]{
      def digest(a: Tuple6[A1,A2,A3,A4,A5,A6], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      HashCode(current)
      }
  }
  implicit def tuple7Hashable128[A1,A2,A3,A4,A5,A6,A7](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)]) = new Hashable[Tuple7[A1,A2,A3,A4,A5,A6,A7],(Long, Long)]{
      def digest(a: Tuple7[A1,A2,A3,A4,A5,A6,A7], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      HashCode(current)
      }
  }
  implicit def tuple8Hashable128[A1,A2,A3,A4,A5,A6,A7,A8](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)]) = new Hashable[Tuple8[A1,A2,A3,A4,A5,A6,A7,A8],(Long, Long)]{
      def digest(a: Tuple8[A1,A2,A3,A4,A5,A6,A7,A8], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      HashCode(current)
      }
  }
  implicit def tuple9Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)]) = new Hashable[Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9],(Long, Long)]{
      def digest(a: Tuple9[A1,A2,A3,A4,A5,A6,A7,A8,A9], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      HashCode(current)
      }
  }
  implicit def tuple10Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)]) = new Hashable[Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10],(Long, Long)]{
      def digest(a: Tuple10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      HashCode(current)
      }
  }
  implicit def tuple11Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)]) = new Hashable[Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11],(Long, Long)]{
      def digest(a: Tuple11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      HashCode(current)
      }
  }
  implicit def tuple12Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)]) = new Hashable[Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12],(Long, Long)]{
      def digest(a: Tuple12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      HashCode(current)
      }
  }
  implicit def tuple13Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)]) = new Hashable[Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13],(Long, Long)]{
      def digest(a: Tuple13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      HashCode(current)
      }
  }
  implicit def tuple14Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)]) = new Hashable[Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14],(Long, Long)]{
      def digest(a: Tuple14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      HashCode(current)
      }
  }
  implicit def tuple15Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)]) = new Hashable[Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15],(Long, Long)]{
      def digest(a: Tuple15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      HashCode(current)
      }
  }
  implicit def tuple16Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)]) = new Hashable[Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16],(Long, Long)]{
      def digest(a: Tuple16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      HashCode(current)
      }
  }
  implicit def tuple17Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)]) = new Hashable[Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17],(Long, Long)]{
      def digest(a: Tuple17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      HashCode(current)
      }
  }
  implicit def tuple18Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)], h18: Hashable[A18,(Long, Long)]) = new Hashable[Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18],(Long, Long)]{
      def digest(a: Tuple18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      current = combine128Hashes(current, h18.digest(a._18, seed))
      HashCode(current)
      }
  }
  implicit def tuple19Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)], h18: Hashable[A18,(Long, Long)], h19: Hashable[A19,(Long, Long)]) = new Hashable[Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19],(Long, Long)]{
      def digest(a: Tuple19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      current = combine128Hashes(current, h18.digest(a._18, seed))
      current = combine128Hashes(current, h19.digest(a._19, seed))
      HashCode(current)
      }
  }
  implicit def tuple20Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)], h18: Hashable[A18,(Long, Long)], h19: Hashable[A19,(Long, Long)], h20: Hashable[A20,(Long, Long)]) = new Hashable[Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20],(Long, Long)]{
      def digest(a: Tuple20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      current = combine128Hashes(current, h18.digest(a._18, seed))
      current = combine128Hashes(current, h19.digest(a._19, seed))
      current = combine128Hashes(current, h20.digest(a._20, seed))
      HashCode(current)
      }
  }
  implicit def tuple21Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)], h18: Hashable[A18,(Long, Long)], h19: Hashable[A19,(Long, Long)], h20: Hashable[A20,(Long, Long)], h21: Hashable[A21,(Long, Long)]) = new Hashable[Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21],(Long, Long)]{
      def digest(a: Tuple21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      current = combine128Hashes(current, h18.digest(a._18, seed))
      current = combine128Hashes(current, h19.digest(a._19, seed))
      current = combine128Hashes(current, h20.digest(a._20, seed))
      current = combine128Hashes(current, h21.digest(a._21, seed))
      HashCode(current)
      }
  }
  implicit def tuple22Hashable128[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit h1: Hashable[A1,(Long, Long)], h2: Hashable[A2,(Long, Long)], h3: Hashable[A3,(Long, Long)], h4: Hashable[A4,(Long, Long)], h5: Hashable[A5,(Long, Long)], h6: Hashable[A6,(Long, Long)], h7: Hashable[A7,(Long, Long)], h8: Hashable[A8,(Long, Long)], h9: Hashable[A9,(Long, Long)], h10: Hashable[A10,(Long, Long)], h11: Hashable[A11,(Long, Long)], h12: Hashable[A12,(Long, Long)], h13: Hashable[A13,(Long, Long)], h14: Hashable[A14,(Long, Long)], h15: Hashable[A15,(Long, Long)], h16: Hashable[A16,(Long, Long)], h17: Hashable[A17,(Long, Long)], h18: Hashable[A18,(Long, Long)], h19: Hashable[A19,(Long, Long)], h20: Hashable[A20,(Long, Long)], h21: Hashable[A21,(Long, Long)], h22: Hashable[A22,(Long, Long)]) = new Hashable[Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22],(Long, Long)]{
      def digest(a: Tuple22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22], seed: Long): (Long, Long) @@ HashCode = {
      var current: (Long, Long) = (1L, 1L)
      current = combine128Hashes(current, h1.digest(a._1, seed))
      current = combine128Hashes(current, h2.digest(a._2, seed))
      current = combine128Hashes(current, h3.digest(a._3, seed))
      current = combine128Hashes(current, h4.digest(a._4, seed))
      current = combine128Hashes(current, h5.digest(a._5, seed))
      current = combine128Hashes(current, h6.digest(a._6, seed))
      current = combine128Hashes(current, h7.digest(a._7, seed))
      current = combine128Hashes(current, h8.digest(a._8, seed))
      current = combine128Hashes(current, h9.digest(a._9, seed))
      current = combine128Hashes(current, h10.digest(a._10, seed))
      current = combine128Hashes(current, h11.digest(a._11, seed))
      current = combine128Hashes(current, h12.digest(a._12, seed))
      current = combine128Hashes(current, h13.digest(a._13, seed))
      current = combine128Hashes(current, h14.digest(a._14, seed))
      current = combine128Hashes(current, h15.digest(a._15, seed))
      current = combine128Hashes(current, h16.digest(a._16, seed))
      current = combine128Hashes(current, h17.digest(a._17, seed))
      current = combine128Hashes(current, h18.digest(a._18, seed))
      current = combine128Hashes(current, h19.digest(a._19, seed))
      current = combine128Hashes(current, h20.digest(a._20, seed))
      current = combine128Hashes(current, h21.digest(a._21, seed))
      current = combine128Hashes(current, h22.digest(a._22, seed))
      HashCode(current)
      }
  }
}