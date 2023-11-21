import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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


  }*/
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
//
//  }
//  def reconstruirCadenaTurboAceleradaPar ( umbral : Int ) (n : Int , o : Oraculo ) : Seq [Char]= {
//    // recibela longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
//    // y devuelve la secuencia reconstruida
//    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
//    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
//    // Usa paralelismo de tareas y/o datos
//
//  }*/
}
