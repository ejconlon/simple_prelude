package simple_prelude

import scala.language.higherKinds

class StateMonad[S] extends Monad[({type L[A] = State[S, A]})#L] {
  override def fmap[A, B](fa: State[S, A])(f: A => B): State[S, B] = { (s: S) =>
    val (a, t) = fa(s)
    (f(a), t)
  }
  override def pure[A](a: A): State[S, A] = { (s: S) => (a, s) }
  override def joinWith[A, B, Z](fa: State[S, A], fb: State[S, B])(f: (A, B) => Z): State[S, Z] = { (s0: S) =>
    val (a, s1) = fa(s0)
    val (b, s2) = fb(s1)
    val z = f(a, b)
    (z, s2)
  }
  override def sequence[A, Z](fas: Seq[State[S, A]])(f: Seq[A] => Z): State[S, Z] = { (s0: S) =>
    val as = Seq.newBuilder[A]
    var s = s0
    fas.foreach { fa =>
      val (a, t) = fa(s)
      as += a
      s = t
    }
    (f(as.result), s)
  }
  override def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = { (s0: S) =>
    val (a, s1) = fa(s0)
    val fb = f(a)
    fb(s1)
  }
}
