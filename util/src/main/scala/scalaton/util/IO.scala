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

import java.io._

trait IOModule{
  object reader{

    def file(path: String) = new BufferedReader(new FileReader(path))

    def inputStream(is: InputStream) = new BufferedReader(new InputStreamReader(is))

    def stdin = inputStream(System.in)

  }

  object writer{

    def file(path: String) = new BufferedWriter(new FileWriter(path))

    def outputStream(os: OutputStream) = new BufferedWriter(new OutputStreamWriter(os))

    def stdout = outputStream(System.out)

    def stderr = outputStream(System.err)

  }

}

object io
extends IOModule
