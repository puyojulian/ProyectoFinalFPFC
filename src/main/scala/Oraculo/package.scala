/*
  Archivo:  package.scala
  Autor:  Profesor Juan Francisco Díaz Frias
  Curso:    Fundamentos de Programación Funcional y Concurrente
  Trabajo:  Proyecto Final
  Fecha de entrega: 07/12/2023
*/

package object Oraculo {
  val alfabeto=Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def crearOraculo(delay:Int)(c:Seq[Char]):Oraculo ={
    def esSubcadena(s:Seq[Char]):Boolean = {
      Thread.sleep(delay)
      c.containsSlice(s)
    }
    esSubcadena
  }
}
