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
    fa.flatMap(f)
}

class SeqTMonad[M[_]](
  monad: Monad[M]
) extends ComposeApplicative[Seq, M](SeqMonad, monad)
  with Monad[({type L[A] = SeqT[M, A]})#L] {
  override def bind[A, B](fa: SeqT[M, A])(f: A => SeqT[M, B]): SeqT[M, B] =
    throw TodoException
}
