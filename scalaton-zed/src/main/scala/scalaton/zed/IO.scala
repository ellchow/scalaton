package scalaton.zed

import java.io._

trait IOModule{
  object reader{
    def stdin = new BufferedReader(new InputStreamReader(System.in))

    def file(path: String) = new BufferedReader(new FileReader(path))
  }

  object writer{
    def stdout = new BufferedWriter(new OutputStreamWriter(System.out))

    def stderr = new BufferedWriter(new OutputStreamWriter(System.err))

    def file(path: String) = new BufferedWriter(new FileWriter(path))
  }

}

object io
extends IOModule
