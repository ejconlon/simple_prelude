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

abstract class StateTMonad[S, M[_]](monad: Monad[M]) extends Monad[({type L[A] = StateT[S, M, A]})#L] {
  override def fmap[A, B](fa: StateT[S, M, A])(f: A => B): StateT[S, M, B] = { (s: S) =>
    monad.fmap(fa(s)) { case (a, t) => (f(a), t) }
  }
  override def pure[A](a: A): StateT[S, M, A] = { (s: S) => monad.pure((a, s)) }
  override def joinWith[A, B, Z](fa: StateT[S, M, A], fb: StateT[S, M, B])(f: (A, B) => Z): StateT[S, M, Z] = { (s: S) =>
    throw TodoException // how do you thread state???
  }
  override def sequence[A, Z](fas: Seq[StateT[S, M, A]])(f: Seq[A] => Z): StateT[S, M, Z]
  override def bind[A, B](fa: StateT[S, M, A])(f: A => StateT[S, M, B]): StateT[S, M, B]
}
