/*
  Archivo:  package.scala
  Autor:           Profesor Juan Francisco Díaz Frias
  Modificado por:  Julian Ernesto Puyo Mora
  Curso:    Fundamentos de Programación Funcional y Concurrente
  Trabajo:  Proyecto Final
  Fecha de entrega: 07/12/2023
*/

import scala.collection.parallel.CollectionConverters._
import org.scalameter._
import Oraculo._
package object Benchmark {
  type AlgoritmoPRC = (Int, Oraculo) => Seq[Char]

  def compararAlgoritmos(a1: AlgoritmoPRC, a2: AlgoritmoPRC)
                        (n: Int, o: Oraculo): (Double, Double, Double) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a1(n, o))

    val timeA2 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer (new Warmer.Default) measure (a2(n, o))

    val speedUp = timeA1.value / timeA2.value
    (timeA1.value, timeA2.value, speedUp)
  }
}
