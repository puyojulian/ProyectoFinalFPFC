import common._
import Oraculo._
import ArbolSufijos._

import scala.annotation.meta.param
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

package object ReconstCadenasPar {
  // Ahora versiones paralelas
  def reconstruirCadenaIngenuoPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas

     val alfabeto = Seq('a', 'c', 'g', 't')

    def generarSecuencias(n: Int, alphabet: Seq[Char]): Set[Seq[Char]] = {
      val initialSet: Set[Seq[Char]] = Set(Seq.empty[Char])
      (1 to n).foldLeft(initialSet) { (acc, _) =>
        acc.flatMap(seq => alphabet.map(char => seq :+ char))
      }
    }

    val secuencias = generarSecuencias(n, alfabeto)

    // Función que verifica el oráculo en paralelo utilizando Task
    def checkOraculoParallel(seq: Seq[Char]): Seq[Char] =
      task {
        if (o(seq)) seq else Seq.empty[Char]
      }.join()

    // Convertir secuencias a lista de tareas
    val tasks: List[Seq[Char]] = secuencias.toList.map(checkOraculoParallel)

    // Ejecutar tareas en paralelo con umbral de concurrencia usando Task
    val parallelResult: Seq[Char] = tasks.reduceLeft { (acc, task) =>
      val (result1, result2) = parallel(acc, task)
      // Aquí puedes agregar lógica adicional para combinar los resultados parciales si es necesario
      result1 ++ result2
    }

    parallelResult

  }
  def reconstruirCadenaMejoradoPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos

    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadena(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        common.task(SC.flatMap(seq1 => alfabeto.map(char => seq1 :+ char)).filter(o)).join()
      } else {
        val futures = SC.grouped(umbral).map { group =>
          common.task(group.flatMap(seq1 => alfabeto.map(char => seq1 :+ char)).filter(o)).join()
        }
        val parallelSets = common.task(futures.to(LazyList).flatMap(_.seq).toSet).join()
        parallelSets
      }

      val resultado = common.task(conjSec.to(LazyList).find(_.length == n)).join()
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else common.task(generarCadena(k + 1, conjSec.seq.toSet)).join()
      }
    }

    common.task(generarCadena(1, Set(Seq.empty[Char]))).join()

    /*val alfabeto = Seq('a', 'c', 'g', 't')

    // Función para generar cadenas de manera paralela
    def generarCadena(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      // Expandir las secuencias de manera paralela utilizando el alfabeto
      val conjSec = common.task(SC.flatMap(seq => alfabeto.map(char => seq :+ char)).filter(o)).join()

      // Buscar una secuencia en el conjunto filtrado que tenga longitud igual a n
      val resultado = conjSec.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq // Devolver la secuencia si se encuentra
        case None =>
          if (k > n) Seq.empty[Char] // Si k supera n, devolver una secuencia vacía
          else generarCadena(k + 1, conjSec.seq.toSet) // Llamada recursiva con el conjunto filtrado
      }
    }

    // Llamada inicial con un conjunto que contiene solo la secuencia vacía
    generarCadena(1, Set(Seq.empty[Char]))*/

    /*val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadena(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        SC.flatMap(seq1 => alfabeto.map(char => seq1 :+ char))
      } else {
        val futures = SC.grouped(umbral).map { group =>
          Future(group.flatMap(seq1 => alfabeto.map(char => seq1 :+ char)))
        }
        val parallelSets = Await.result(Future.sequence(futures), Duration.Inf)
        parallelSets.flatten.toSet
      }

      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadena(k + 1, newSC)
      }
    }

    val conjuntoInicial: Set[Seq[Char]] = Set(Seq.empty[Char])
    generarCadena(1, conjuntoInicial)*/

  }

//  def reconstruirCadenaTurboPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa paralelismo de tareas y/o datos
//
//  }
//  def reconstruirCadenaTurboMejoradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa paralelismo de tareas y/o datos
//
//  }
//  def reconstruirCadenaTurboAceleradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
//    // Usa paralelismo de tareas y/o datos
//
//  }


  def reconstruirCadenaTurboParop1(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurboParalelo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val futures = SC.grouped(umbral).map { group =>
          Future(group.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)))
        }
        Await.result(Future.sequence(futures), Duration.Inf).flatten.toSet
      }

      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurboParalelo(k * 2, newSC)
      }
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurboParalelo(2, conjuntoInicial)
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurboPar(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val futures = for (i <- grouped.indices) yield task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        futures.flatMap(future => future.join()).toSet
      }

      val newSC = conjSec.filter(o)
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurboPar(k * 2, newSC)
      }
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurboPar(2, conjuntoInicial)
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurboParalelo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val futures = for (i <- grouped.indices) yield task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        futures.flatMap(future => future.join()).toSet
      }

      val newSC = conjSec.filter(s => o(s))
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurboParalelo(k * 2, filtrar(newSC, k))
      }
    }

    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      SC.filter { s1 =>
        SC.forall { s2 =>
          val s = s1 ++ s2
          SC.exists(w => w.sliding(k, 1).forall(sub => s.containsSlice(sub)))
        }
      }
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurboParalelo(2, conjuntoInicial)
  }

  def reconstruirCadenaTurboMejoradaParWop(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    def generarCadenaTurboParalelo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val futures = SC.grouped(umbral).map { group =>
          Future(group.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)))
        }
        Await.result(Future.sequence(futures), Duration.Inf).flatten.toSet
      }

      val newSC = conjSec.filter(s => o(s))
      val resultado = newSC.to(LazyList).find(_.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurboParalelo(k * 2, filtrar(newSC, k))
      }
    }

    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      SC.filter { s1 =>
        SC.forall { s2 =>
          val s = s1 ++ s2
          SC.exists(w => w.sliding(k, 1).forall(sub => s.containsSlice(sub)))
        }
      }
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurboParalelo(2, conjuntoInicial)
  }
}
