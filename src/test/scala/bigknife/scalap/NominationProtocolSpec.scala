package bigknife.scalap

import org.scalatest.{FunSuite, GivenWhenThen}

class NominationProtocolSpec extends FunSuite with GivenWhenThen {
  val fixture: TestFixture = new TestFixture {}
  import fixture._

  test("scp ast") {
    info(s"scp is $scp")
  }
}
