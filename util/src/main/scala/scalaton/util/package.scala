package scalaton

package object util {
  def mkTempDir(base: String = System.getProperty("java.io.tmpdir"), attempts: Int = 1000): java.io.File = {
    val timestamp = System.currentTimeMillis + "-"

    var i = 0
    var tmp = None: Option[java.io.File]
    while(tmp.isEmpty && i < attempts) {
      i += 1
      val t = new java.io.File(base, timestamp + i)
      if (t.mkdir)
        tmp = Some(t)
    }

    if(tmp.nonEmpty)
      tmp.get
    else
      throw new Exception(s"failed to create temp dir in $base after $attempts attempts")
  }
}
