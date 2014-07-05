/*
 Copyright 2014 Elliot Chow

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

trait HashableTupleInstances extends HashFunctions {
  /*

   Vector(("32","Int"),("64", "Long"),("128","(Long,Long)")).drop(2).take(1).foreach{ case (yy, t) =>
   println(s"trait Tuple${yy}Instances {")
    (2 to 22).foreach{ i =>
   val tps = (1 to i).map{ k => s"A${k}" }.mkString(",")
   val has = (1 to i).map{ k => s"ha${k}: Hashable[A${k},${t}]" }.mkString(", ")
   val cs = (2 to i).map{ k => s"        h = combine(h, Hash(x._${k}, seed))"}.mkString("\n")
   val z = s"""implicit def Tuple${i}MurmurHash${yy}Hashable[${tps}](implicit ${has}) = new Hashable[(${tps}),${t}] {
   def hash(x: (${tps}), seed: Long) = {
   var h = Hash(x._1, seed)
   ${cs}
   h
   }
   }"""
   println(z)
   }
   println("}")
   }
   */

  trait Tuple32Instances {
    implicit def Tuple2MurmurHash32Hashable[A1,A2](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int]) = new Hashable[(A1,A2),Int] {
      def hash(x: (A1,A2), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h
      }
    }
    implicit def Tuple3MurmurHash32Hashable[A1,A2,A3](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int]) = new Hashable[(A1,A2,A3),Int] {
      def hash(x: (A1,A2,A3), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h
      }
    }
    implicit def Tuple4MurmurHash32Hashable[A1,A2,A3,A4](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int]) = new Hashable[(A1,A2,A3,A4),Int] {
      def hash(x: (A1,A2,A3,A4), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h
      }
    }
    implicit def Tuple5MurmurHash32Hashable[A1,A2,A3,A4,A5](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int]) = new Hashable[(A1,A2,A3,A4,A5),Int] {
      def hash(x: (A1,A2,A3,A4,A5), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h
      }
    }
    implicit def Tuple6MurmurHash32Hashable[A1,A2,A3,A4,A5,A6](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h
      }
    }
    implicit def Tuple7MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h
      }
    }
    implicit def Tuple8MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h
      }
    }
    implicit def Tuple9MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h
      }
    }
    implicit def Tuple10MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h
      }
    }
    implicit def Tuple11MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h
      }
    }
    implicit def Tuple12MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h
      }
    }
    implicit def Tuple13MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h
      }
    }
    implicit def Tuple14MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h
      }
    }
    implicit def Tuple15MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h
      }
    }
    implicit def Tuple16MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h
      }
    }
    implicit def Tuple17MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h
      }
    }
    implicit def Tuple18MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int], ha18: Hashable[A18,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h
      }
    }
    implicit def Tuple19MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int], ha18: Hashable[A18,Int], ha19: Hashable[A19,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h
      }
    }
    implicit def Tuple20MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int], ha18: Hashable[A18,Int], ha19: Hashable[A19,Int], ha20: Hashable[A20,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h
      }
    }
    implicit def Tuple21MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int], ha18: Hashable[A18,Int], ha19: Hashable[A19,Int], ha20: Hashable[A20,Int], ha21: Hashable[A21,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h
      }
    }
    implicit def Tuple22MurmurHash32Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit ha1: Hashable[A1,Int], ha2: Hashable[A2,Int], ha3: Hashable[A3,Int], ha4: Hashable[A4,Int], ha5: Hashable[A5,Int], ha6: Hashable[A6,Int], ha7: Hashable[A7,Int], ha8: Hashable[A8,Int], ha9: Hashable[A9,Int], ha10: Hashable[A10,Int], ha11: Hashable[A11,Int], ha12: Hashable[A12,Int], ha13: Hashable[A13,Int], ha14: Hashable[A14,Int], ha15: Hashable[A15,Int], ha16: Hashable[A16,Int], ha17: Hashable[A17,Int], ha18: Hashable[A18,Int], ha19: Hashable[A19,Int], ha20: Hashable[A20,Int], ha21: Hashable[A21,Int], ha22: Hashable[A22,Int]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22),Int] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h = combine(h, Hash(x._22, seed))
        h
      }
    }
  }

  trait Tuple64Instances {
    implicit def Tuple2MurmurHash64Hashable[A1,A2](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long]) = new Hashable[(A1,A2),Long] {
      def hash(x: (A1,A2), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h
      }
    }
    implicit def Tuple3MurmurHash64Hashable[A1,A2,A3](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long]) = new Hashable[(A1,A2,A3),Long] {
      def hash(x: (A1,A2,A3), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h
      }
    }
    implicit def Tuple4MurmurHash64Hashable[A1,A2,A3,A4](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long]) = new Hashable[(A1,A2,A3,A4),Long] {
      def hash(x: (A1,A2,A3,A4), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h
      }
    }
    implicit def Tuple5MurmurHash64Hashable[A1,A2,A3,A4,A5](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long]) = new Hashable[(A1,A2,A3,A4,A5),Long] {
      def hash(x: (A1,A2,A3,A4,A5), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h
      }
    }
    implicit def Tuple6MurmurHash64Hashable[A1,A2,A3,A4,A5,A6](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h
      }
    }
    implicit def Tuple7MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h
      }
    }
    implicit def Tuple8MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h
      }
    }
    implicit def Tuple9MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h
      }
    }
    implicit def Tuple10MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h
      }
    }
    implicit def Tuple11MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h
      }
    }
    implicit def Tuple12MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h
      }
    }
    implicit def Tuple13MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h
      }
    }
    implicit def Tuple14MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h
      }
    }
    implicit def Tuple15MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h
      }
    }
    implicit def Tuple16MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h
      }
    }
    implicit def Tuple17MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h
      }
    }
    implicit def Tuple18MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long], ha18: Hashable[A18,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h
      }
    }
    implicit def Tuple19MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long], ha18: Hashable[A18,Long], ha19: Hashable[A19,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h
      }
    }
    implicit def Tuple20MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long], ha18: Hashable[A18,Long], ha19: Hashable[A19,Long], ha20: Hashable[A20,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h
      }
    }
    implicit def Tuple21MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long], ha18: Hashable[A18,Long], ha19: Hashable[A19,Long], ha20: Hashable[A20,Long], ha21: Hashable[A21,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h
      }
    }
    implicit def Tuple22MurmurHash64Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit ha1: Hashable[A1,Long], ha2: Hashable[A2,Long], ha3: Hashable[A3,Long], ha4: Hashable[A4,Long], ha5: Hashable[A5,Long], ha6: Hashable[A6,Long], ha7: Hashable[A7,Long], ha8: Hashable[A8,Long], ha9: Hashable[A9,Long], ha10: Hashable[A10,Long], ha11: Hashable[A11,Long], ha12: Hashable[A12,Long], ha13: Hashable[A13,Long], ha14: Hashable[A14,Long], ha15: Hashable[A15,Long], ha16: Hashable[A16,Long], ha17: Hashable[A17,Long], ha18: Hashable[A18,Long], ha19: Hashable[A19,Long], ha20: Hashable[A20,Long], ha21: Hashable[A21,Long], ha22: Hashable[A22,Long]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22),Long] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h = combine(h, Hash(x._22, seed))
        h
      }
    }
  }

  trait Tuple128Instances {
    implicit def Tuple2MurmurHash128Hashable[A1,A2](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)]) = new Hashable[(A1,A2),(Long,Long)] {
      def hash(x: (A1,A2), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h
      }
    }
    implicit def Tuple3MurmurHash128Hashable[A1,A2,A3](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)]) = new Hashable[(A1,A2,A3),(Long,Long)] {
      def hash(x: (A1,A2,A3), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h
      }
    }
    implicit def Tuple4MurmurHash128Hashable[A1,A2,A3,A4](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)]) = new Hashable[(A1,A2,A3,A4),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h
      }
    }
    implicit def Tuple5MurmurHash128Hashable[A1,A2,A3,A4,A5](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h
      }
    }
    implicit def Tuple6MurmurHash128Hashable[A1,A2,A3,A4,A5,A6](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h
      }
    }
    implicit def Tuple7MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h
      }
    }
    implicit def Tuple8MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h
      }
    }
    implicit def Tuple9MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h
      }
    }
    implicit def Tuple10MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h
      }
    }
    implicit def Tuple11MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h
      }
    }
    implicit def Tuple12MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h
      }
    }
    implicit def Tuple13MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h
      }
    }
    implicit def Tuple14MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h
      }
    }
    implicit def Tuple15MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h
      }
    }
    implicit def Tuple16MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h
      }
    }
    implicit def Tuple17MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h
      }
    }
    implicit def Tuple18MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)], ha18: Hashable[A18,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h
      }
    }
    implicit def Tuple19MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)], ha18: Hashable[A18,(Long,Long)], ha19: Hashable[A19,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h
      }
    }
    implicit def Tuple20MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)], ha18: Hashable[A18,(Long,Long)], ha19: Hashable[A19,(Long,Long)], ha20: Hashable[A20,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h
      }
    }
    implicit def Tuple21MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)], ha18: Hashable[A18,(Long,Long)], ha19: Hashable[A19,(Long,Long)], ha20: Hashable[A20,(Long,Long)], ha21: Hashable[A21,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h
      }
    }
    implicit def Tuple22MurmurHash128Hashable[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](implicit ha1: Hashable[A1,(Long,Long)], ha2: Hashable[A2,(Long,Long)], ha3: Hashable[A3,(Long,Long)], ha4: Hashable[A4,(Long,Long)], ha5: Hashable[A5,(Long,Long)], ha6: Hashable[A6,(Long,Long)], ha7: Hashable[A7,(Long,Long)], ha8: Hashable[A8,(Long,Long)], ha9: Hashable[A9,(Long,Long)], ha10: Hashable[A10,(Long,Long)], ha11: Hashable[A11,(Long,Long)], ha12: Hashable[A12,(Long,Long)], ha13: Hashable[A13,(Long,Long)], ha14: Hashable[A14,(Long,Long)], ha15: Hashable[A15,(Long,Long)], ha16: Hashable[A16,(Long,Long)], ha17: Hashable[A17,(Long,Long)], ha18: Hashable[A18,(Long,Long)], ha19: Hashable[A19,(Long,Long)], ha20: Hashable[A20,(Long,Long)], ha21: Hashable[A21,(Long,Long)], ha22: Hashable[A22,(Long,Long)]) = new Hashable[(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22),(Long,Long)] {
      def hash(x: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22), seed: Long) = {
        var h = Hash(x._1, seed)
        h = combine(h, Hash(x._2, seed))
        h = combine(h, Hash(x._3, seed))
        h = combine(h, Hash(x._4, seed))
        h = combine(h, Hash(x._5, seed))
        h = combine(h, Hash(x._6, seed))
        h = combine(h, Hash(x._7, seed))
        h = combine(h, Hash(x._8, seed))
        h = combine(h, Hash(x._9, seed))
        h = combine(h, Hash(x._10, seed))
        h = combine(h, Hash(x._11, seed))
        h = combine(h, Hash(x._12, seed))
        h = combine(h, Hash(x._13, seed))
        h = combine(h, Hash(x._14, seed))
        h = combine(h, Hash(x._15, seed))
        h = combine(h, Hash(x._16, seed))
        h = combine(h, Hash(x._17, seed))
        h = combine(h, Hash(x._18, seed))
        h = combine(h, Hash(x._19, seed))
        h = combine(h, Hash(x._20, seed))
        h = combine(h, Hash(x._21, seed))
        h = combine(h, Hash(x._22, seed))
        h
      }
    }
  }

}
