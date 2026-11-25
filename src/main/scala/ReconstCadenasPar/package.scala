import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._
import scala.annotation.tailrec

package object ReconstCadenasPar {
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(base: List[Char], k: Int): Iterator[List[Char]] =
      if (k == 0) Iterator(base)
      else generarCadenas(base, k - 1).flatMap(p => alfabeto.iterator.map(c => c :: p))

    val d = math.min(n, math.max(umbral, 0))
    val prefijos: Vector[List[Char]] = generarCadenas(Nil, d).toVector

    def buscar(p: List[Char]): Option[List[Char]] =
      generarCadenas(p, n - d).find(w => o(w))

    val ganador = prefijos.par.find(p => buscar(p).isDefined).get
    buscar(ganador).get
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def filtrar(cs: Vector[Vector[Char]]): Vector[Vector[Char]] =
      if (cs.length <= umbral) cs.filter(w => o(w))
      else {
        val (izq, der) = cs.splitAt(cs.length / 2)
        val (fi, fd) = parallel(filtrar(izq), filtrar(der))
        fi ++ fd
      }

    @tailrec
    def reconstruir(k: Int, sc: Vector[Vector[Char]]): Seq[Char] = {
      val candidatas = for {s <- sc; c <- alfabeto} yield s :+ c
      val validas = filtrar(candidatas)
      if (k == n) validas.head
      else reconstruir(k + 1, validas)
    }

    if (n == 0) Seq.empty[Char] else reconstruir(1, Vector(Vector.empty[Char]))
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }
}
