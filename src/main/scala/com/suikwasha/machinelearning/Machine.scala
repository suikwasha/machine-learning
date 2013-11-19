package com.suikwasha.machinelearning

/**
 * Created with IntelliJ IDEA.
 * User: suikwasha
 * Date: 2013/11/19
 * Time: 22:59
 * To change this template use File | Settings | File Templates.
 */
trait Machine {
  def predict(wVec: Seq[Double], xVec: Seq[Double]): Double
  def train(wVec: Seq[Double], xVec: Seq[Double], label: Double): Seq[Double]
}
