package bloxorz

sealed abstract class Move

case object Left extends Move

case object Right extends Move

case object Up extends Move

case object Down extends Move
