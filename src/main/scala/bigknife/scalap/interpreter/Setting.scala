package bigknife.scalap
package interpreter

import bigknife.scalap.world.Connect

case class Setting(
    connect: world.Connect
)

object Setting {
  def default(): Setting = {
    val connect = Connect.dummy
    Setting(connect)
  }
}
