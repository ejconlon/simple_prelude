package simple_prelude

import scala.language.higherKinds

class ReaderMonad[R] extends Monad[({type L[A] = Reader[R, A]})#L] {
  override def fmap[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] = { (r: R) => f(fa(r)) }
  override def pure[A](a: A): Reader[R, A] = { (_: R) => a }
  override def joinWith[A, B, Z](fa: Reader[R, A], fb: Reader[R, B])(f: (A, B) => Z): Reader[R, Z] = { (r: R) =>
    val a = fa(r)
    val b = fb(r)
    f(a, b)
  }
  override def sequence[A, Z](fas: Seq[Reader[R, A]])(f: Seq[A] => Z): Reader[R, Z] = { (r: R) =>
    val as = fas map { _(r) }
    f(as)
  }
  override def bind[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = { (r: R) =>
    val a = fa(r)
    val fb = f(a)
    fb(r)
  }
}

class ReaderTMonad[R, F[_]](monad: Monad[F]) extends Monad[({type L[A] = ReaderT[R, F, A]})#L] {
  override def fmap[A, B](fa: ReaderT[R, F, A])(f: A => B): ReaderT[R, F, B] = { (r: R) =>
    monad.fmap(fa(r))(f)
  }
  override def pure[A](a: A): ReaderT[R, F, A] = { (_: R) => monad.pure(a) }
  override def joinWith[A, B, Z](fa: ReaderT[R, F, A], fb: ReaderT[R, F, B])(f: (A, B) => Z): ReaderT[R, F, Z] = { (r: R) =>
    val ga = fa(r)
    val gb = fb(r)
    monad.joinWith(ga, gb)(f)
  }
  override def sequence[A, Z](fas: Seq[ReaderT[R, F, A]])(f: Seq[A] => Z): ReaderT[R, F, Z] = { (r: R) =>
    val gas = fas map { _(r) }
    monad.sequence(gas)(f)
  }
  override def bind[A, Z](fa: ReaderT[R, F, A])(f: A => ReaderT[R, F, Z]): ReaderT[R, F, Z] = { (r: R) =>
    val ga = fa(r)
    monad.bind(ga) { (a: A) => f(a)(r) }
  }
}

class ReaderTMonadTrans[R] extends MonadTrans[({type L[F[_], A] = ReaderT[R, F, A]})#L] {
  override def lift[M[_], A](monad: Monad[M], m: M[A]): ReaderT[R, M, A] = { (_: R) => m }
  override def liftClass[M[_]](monad: Monad[M]): ReaderTMonad[R, M] = new ReaderTMonad[R, M](monad)
}
