package bigknife.scalap.interpreter.misc

import org.bouncycastle.jcajce.provider.digest.SHA3

trait Sha3 {
  def sha3(s: Array[Byte]): Array[Byte] = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    new SHA3.Digest256().digest(s)
  }
}
