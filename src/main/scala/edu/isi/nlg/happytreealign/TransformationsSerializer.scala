package edu.isi.nlg.happytreealign

import com.thoughtworks.xstream.XStream
import java.util
import java.io.{PrintStream, File, OutputStream}
import Direction._

object TransformationsSerializer {

  private val xStream: XStream = {
    val xStream = new XStream()

    xStream.setMode(XStream.NO_REFERENCES)

    xStream.alias("Optional", classOf[Option[(String, Direction)]])
    xStream.alias("Some", classOf[Some[(String, Direction)]])
    xStream.alias("None", None.getClass)
    xStream.alias("Pair", classOf[(String, Direction)])

    xStream.alias("Direction", classOf[Direction])
    xStream.alias("Left", Left.getClass)
    xStream.alias("Right", Right.getClass)
    xStream.alias("Transformation", classOf[Transformation])

    import trans._
    xStream.alias("Adopt", classOf[Adopt])
    xStream.alias("Articulate", classOf[Articulate])
    xStream.alias("Demote", classOf[Demote])
    xStream.alias("Flatten", classOf[Flatten])
    xStream.alias("Promote", classOf[Promote])
    xStream.alias("Transfer", classOf[Transfer])

    xStream
  }

  def serialize(trans: util.ArrayList[Transformation], outStream: OutputStream) {
    xStream.toXML(trans, outStream)
  }

  def serialize(trans: util.ArrayList[Transformation], file: File) {
    val stream = new PrintStream(file, "UTF-8")
    serialize(trans, stream)
    stream.close()
  }

  def deserialize(file: File) =
    xStream.fromXML(file).asInstanceOf[util.ArrayList[Transformation]]

  def deserialize(s: String) =
    xStream.fromXML(s).asInstanceOf[util.ArrayList[Transformation]]

}
