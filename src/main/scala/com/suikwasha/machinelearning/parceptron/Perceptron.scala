package com.suikwasha.machinelearning.parceptron

import scala.util.Random
import scala.io.Source
import java.io.File

trait Machine {
  def predict(wVec: Seq[Double], xVec: Seq[Double]): Double
  def train(wVec: Seq[Double], xVec: Seq[Double], label: Double): Seq[Double]
}

object Perceptron extends Machine {
  def main(args: Array[String]) = {

    // AND
    val trainData = Map[Seq[Double], Double](
      Seq(0.0, 0.0, 1.0) -> -1.0,
      Seq(0.0, 1.0, 1.0) -> 1.0,
      Seq(1.0, 0.0, 1.0) -> 1.0,
      Seq(1.0, 1.0, 1.0) -> 1.0
    )

    val rnd = new Random
    val stream = Stream.iterate(Seq(-0.5, 0.5, 0.2)){ wVec =>
      val key: Seq[Double] = Seq(rnd.nextInt(2), rnd.nextInt(2), 1.0)
      val label = trainData.get(key).get
      train(wVec, key, label)
    }

    val wVec = stream(1000)
    println(wVec)
    trainData.foreach{
      case (input, label) => println(predict(wVec, input) * label >= 0)
    }
  }

  def predict(wVec: Seq[Double], xVec: Seq[Double]): Double = wVec.zip(xVec).map(t => t._1 * t._2).reduce(_ + _)

  val C = 0.2

  def train(wVec: Seq[Double], xVec: Seq[Double], label: Double): Seq[Double] =
    if (predict(wVec, xVec) * label < 0) {
      // wVec + C * label * xVec
      wVec.zip(xVec.map(_ * C * label)).map(t => t._1 + t._2)
    } else {
      wVec
    }
}

object IrisWithPerceptron {
  def main(args: Array[String]) = {
    val file = new File("src/main/resources/Fisher.csv")
    val source = Source.fromFile(file)
    val data = source.getLines.toSeq.tail.map(_.split(",")).map(_.map(_.toDouble))


    val ans = data.foldLeft(Seq(1.0, -0.5, -0.5, -0.5, -0.5)){ (wVec, line) =>
      Perceptron.train(wVec, 1.0 +: line.tail, line.head)
    }

    println(ans)
    data.foreach{ line =>
      val t = line.head
      val g = line.tail
      println(Perceptron.predict(ans, g))
    }
  }
}
