import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._
import ReconstCadenas._
import scala.annotation.meta.param


package object ReconstCadenasPar {
  // Ahora versiones paralelas
  /*def reconstruirCadenaIngenuoPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
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

    // Función que verifica el oráculo en paralelo
    def checkOraculoParallel(seq: Seq[Char]): Future[Option[Seq[Char]]] =
      Future {
        if (o(seq)) Some(seq) else None
      }

    // Convertir secuencias a lista de futuros Future
    val futures: List[Future[Option[Seq[Char]]]] = secuencias.toList.map(checkOraculoParallel)

    // Ejecutar futuros en paralelo con umbral de concurrencia
    val parallelFutures: Future[Option[Seq[Char]]] = Future.sequence(futures).map(_.find(_.isDefined).flatten)

    // Obtener el resultado sincrónicamente
    val resultado: Option[Seq[Char]] = Await.result(parallelFutures, Duration.Inf)

    resultado.getOrElse(Seq.empty[Char])

  } */

  /**
   * @param umbral como un valor que dedice desde cuando se paraleliza
   * @param n como la longitud de la secuencia a recostruir
   * @param o como el  oraculo para esa secuencia
   * @return secuencia reconstruida usando paralelismo de tareas (en su version Turbo)
   */
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

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
    generarCadenaTurboPar(1, conjuntoInicial)
  }


  /**
   * @param umbral como un valor que dedice desde cuando se paraleliza
   * @param n      como la longitud de la secuencia a recostruir
   * @param o      como el  oraculo para esa secuencia
   * @return secuencia reconstruida usando paralelismo de tareas (en su version TurboMejorada)
   */

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    // Función principal para generar la cadena turbo mejorada paralela
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = if (conjSec.size <= umbral) {
        conjSec.filter(o)
      } else {
        val (left, right) = conjSec.splitAt(conjSec.size / 2)
        val (filteredLeft, filteredRight) = parallel(left.filter(o), right.filter(o))
        filteredLeft ++ filteredRight
      }

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada (en su forma paralela)
    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val S = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2)).filter {
          s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k)))
        }
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val tasks = for (i <- grouped.indices) yield common.task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        tasks.flatMap(task => task.join()).toSeq
      }
      val FilterResult = S.filter { s => (0 to s.length - k).forall(i => SC.contains(s.slice(i, i + k))) }
      FilterResult
    }

    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    generarCadenaTurbo(1, conjuntoInicial)
  }

  /*
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    // Función principal para generar la cadena turbo acalerada paralela dependiendo del umbral
    def generarCadenaTurbo(k: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = if (conjSec.size <= umbral) {
        conjSec.filter(o)
      } else {
        val (left, right) = conjSec.splitAt(conjSec.size / 2)
        val (filteredLeft, filteredRight) = parallel(left.filter(o), right.filter(o))
        filteredLeft ++ filteredRight
      }

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada (en su forma paralela)
    def filtrar(SC: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC)
      val (left, right) = SC.splitAt(SC.size / 2)
      val taskLeft = task {
        left.flatMap(seq1 => left.map(seq2 => seq1 ++ seq2)).filter(s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)))
      }
      val taskRight = task {
        right.flatMap(seq1 => right.map(seq2 => seq1 ++ seq2)).filter(s => (0 to s.length - k).forall(i => pertenece(s.slice(i, i + k), trieSC)))
      }
      taskLeft.join() ++ taskRight.join()
    }

    val conjuntoInicial: Seq[Seq[Char]] = alfabeto.map(Seq(_))
    generarCadenaTurbo(1, conjuntoInicial)
  }




  //  def reconstruirCadenaMejoradoPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
  //    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
  //    // y devuelve la secuencia reconstruida
  //    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
  //    // Usa paralelismo de tareas y/o datos
  //
  //  }
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

  //  }
  //  def reconstruirCadenaTurboAceleradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
  //    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
  //    // y devuelve la secuencia reconstruida
  //    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
  //    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
  //    // Usa paralelismo de tareas y/o datos
  //
  //  }*/

  /*
  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    // Función principal para generar la cadena turbo mejorada paralela
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = if (conjSec.size <= umbral) {
        conjSec.filter(o)
      } else {
        val (left, right) = conjSec.splitAt(conjSec.size / 2)
        val (filteredLeft, filteredRight) = parallel(left.filter(o), right.filter(o))
        filteredLeft ++ filteredRight
      }

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(seq) => seq
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada (en su forma paralela)
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val S = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val futures = for (i <- grouped.indices) yield task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        futures.flatMap(future => future.join()).toSet
      }
      val F = S.filter { s => s.sliding(k).forall(w => SC(w)) }
      F
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurbo(1, conjuntoInicial)
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    // Función principal para generar la cadena turbo acalerada paralela dependiendo del umbral
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = if (conjSec.size <= umbral) {
        conjSec.filter(o)
      } else {
        val (left, right) = conjSec.splitAt(conjSec.size / 2)
        val (filteredLeft, filteredRight) = parallel(left.filter(o), right.filter(o))
        filteredLeft ++ filteredRight
      }

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada (en su forma paralela)
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC.toSeq)
      val S = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val futures = for (i <- grouped.indices) yield task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        futures.flatMap(future => future.join()).toSet
      }
      val F = S.filter { s => s.sliding(k).forall(w => pertenece(w, trieSC)) }
      F
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurbo(1, conjuntoInicial)
  } */

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val alfabeto = Seq('a', 'c', 'g', 't')

    // Función principal para generar la cadena turbo acalerada paralela dependiendo del umbral
    def generarCadenaTurbo(k: Int, SC: Set[Seq[Char]]): Seq[Char] = {
      val conjSec = filtrar(SC, k)
      val newSC = if (conjSec.size <= umbral) {
        conjSec.filter(o)
      } else {
        val (left, right) = conjSec.splitAt(conjSec.size / 2)
        val (filteredLeft, filteredRight) = parallel(left.filter(o), right.filter(o))
        filteredLeft ++ filteredRight
      }

      val resultado = newSC.to(LazyList).find(w => w.length == n)
      resultado match {
        case Some(secuencia) => secuencia
        case None =>
          if (k > n) Seq.empty[Char]
          else generarCadenaTurbo(k * 2, newSC)
      }
    }

    // Función de filtrado para eliminar secuencias problemáticas según la descripción dada (en su forma paralela)
    def filtrar(SC: Set[Seq[Char]], k: Int): Set[Seq[Char]] = {
      val trieSC = arbolDeSufijos(SC.toSeq)
      val S = if (SC.size < umbral) {
        SC.flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
      } else {
        val grouped = SC.grouped(umbral).toSeq
        val futures = for (i <- grouped.indices) yield task {
          grouped(i).flatMap(seq1 => SC.map(seq2 => seq1 ++ seq2))
        }
        futures.flatMap(future => future.join()).toSet
      }
      val F = S.filter { s => s.sliding(k).forall(w => pertenece(w, trieSC)) }
      F
    }

    val conjuntoInicial: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    generarCadenaTurbo(1, conjuntoInicial)
  }



}
