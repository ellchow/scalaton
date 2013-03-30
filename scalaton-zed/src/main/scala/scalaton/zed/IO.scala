package scalaton.zed

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
