package com.suikwasha.machinelearning

import scala.collection.SeqProxy

/**
 * Created with IntelliJ IDEA.
 * User: suikwasha
 * Date: 2013/11/19
 * Time: 22:59
 * To change this template use File | Settings | File Templates.
 */
trait Machine {
  def predict(vec: Vector): Double
  def train(vec: Vector, label: Double): Machine = {
    val v = Vector(1.0)
    null
  }
}

case class Vector(elems: Double *) extends Seq[Double](elems: _*) {

  def length: Int = elems.length

  def iterator: Iterator[Double] = elems.iterator

  private def op (vec: Vector)(f: ((Double, Double)) => Double) = elems.zip(vec.elems).map(f)

  def * (vec: Vector): Vector = op(vec)(t => t._1 * t._2)
}