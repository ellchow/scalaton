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


package scalaton.async

import io.netty.util._
import scala.concurrent._, duration._
import java.util.concurrent.Executors


trait ExecutionContextExtensions {
  object Implicits {
    implicit lazy val defaultHashedWheelTimer = new HashedWheelTimer
    implicit lazy val defaultExecutionContext = ExecutionContext.Implicits.global
  }

  implicit class ExecutionContextOps(val x: ExecutionContext.type) {
    def fixedThreadPool(n: Int) =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(n))

    def forkJoinPool(n: Int) =
      ExecutionContext.fromExecutorService(new forkjoin.ForkJoinPool(n))

    def cachedThreadPool =
      ExecutionContext.fromExecutorService(Executors.newCachedThreadPool)
  }

}
