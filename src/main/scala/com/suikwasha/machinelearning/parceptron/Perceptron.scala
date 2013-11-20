package com.suikwasha.machinelearning.parceptron

import scala.util.Random
import com.suikwasha.machinelearning.Machine
import com.suikwasha.machinelearning.Vector

object Perceptron {
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
}


class Perceptron (val wVec: Vector) extends Machine {

  def predict(vec: Vector): Double = (wVec * vec).reduce(_ + _)

  val C = 0.2

  def train(wVec: Seq[Double], xVec: Seq[Double], label: Double): Seq[Double] =
    if (predict(wVec, xVec) * label < 0) {
      // wVec + C * label * xVec
      wVec.zip(xVec.map(_ * C * label)).map(t => t._1 + t._2)
    } else {
      wVec
    }
}

