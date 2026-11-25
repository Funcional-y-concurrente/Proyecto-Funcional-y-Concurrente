import ArbolSufijos._
import Oraculo._
import scala.annotation.tailrec

package object ReconstCadenas {
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(base: List[Char], k: Int): Iterator[List[Char]] =
      if (k == 0) Iterator(base)
      else generarCadenas(base, k - 1).flatMap(p => alfabeto.iterator.map(c => c :: p))

    generarCadenas(Nil, n).find(w => o(w)).get
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def reconstruir(k: Int, sc: Vector[Vector[Char]]): Seq[Char] = {
      val candidatas = for {s <- sc; c <- alfabeto} yield s :+ c
      val validas = candidatas.filter(w => o(w))
      if (k == n) validas.head
      else reconstruir(k + 1, validas)
    }

    if (n == 0) Seq.empty[Char] else reconstruir(1, Vector(Vector.empty[Char]))
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    ???
  }
}
