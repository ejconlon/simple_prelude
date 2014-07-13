package simple_prelude

import scala.language.higherKinds

object SeqMonad extends Monad[Seq] {
  override def fmap[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  override def pure[A](a: A): Seq[A] = Seq(a)
  // Cartesian join is the one compatible with Monad instance.
  // Zip join is appropriate for streams.
  override def joinWith[A, B, Z](fa: Seq[A], fb: Seq[B])(f: (A, B) => Z): Seq[Z] =
    for {
      a <- fa
      b <- fb
    } yield f(a, b)
  // TODO this doesnt seem right
  override def sequence[A, Z](fas: Seq[Seq[A]])(f: Seq[A] => Z): Seq[Z] = {
    val as = fas.flatten
    if (as.isEmpty) Seq()
    else Seq(f(as))
  }
  override def bind[A, B](fa: Seq[A])(f: A => Seq[B]): Seq[B] =
    for {
      a <- fa
      b <- f(a)
    } yield b
}

class SeqTMonad[M[_]](monad: Monad[M]) extends Applicative[({type L[A] = SeqT[M, A]})#L] {
  override def fmap[A, B](fa: SeqT[M, A])(f: A => B): SeqT[M, B] =
    monad.fmap(fa) { oa => SeqMonad.fmap(oa)(f) }
  override def pure[A](a: A): SeqT[M, A] = monad.pure(SeqMonad.pure(a))
  override def joinWith[A, B, Z](fa: SeqT[M, A], fb: SeqT[M, B])(f: (A, B) => Z): SeqT[M, Z] =
    monad.joinWith(fa, fb) { case (oa, ob) => SeqMonad.joinWith(oa, ob)(f) }
  override def sequence[A, Z](fas: Seq[SeqT[M, A]])(f: Seq[A] => Z): SeqT[M, Z] =
    monad.sequence(fas) { oas => SeqMonad.sequence(oas)(f) }
  /*def bind[A, B](fa: SeqT[M, A])(f: A => SeqT[M, B]): SeqT[M, B] =
    monad.bind(fa) { oa =>
      oa match {
        case None => monad.pure(None)
        case Some(a) => f(a)
      }
    }*/
}
