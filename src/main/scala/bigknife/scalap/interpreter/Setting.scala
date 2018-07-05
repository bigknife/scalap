package bigknife.scalap
package interpreter

import bigknife.scalap.ast.types.{NodeID, QuorumSet}
import bigknife.scalap.world.Connect

case class Setting(
    localNodeID: NodeID,
    quorumSet: QuorumSet,
    connect: world.Connect,
    maxTimeoutSeconds: Int
)

object Setting {
  def default(): Setting = {
    val connect = Connect.dummy
    Setting(NodeID.empty, QuorumSet.fake, connect, maxTimeoutSeconds = 30 * 60)
  }
}
