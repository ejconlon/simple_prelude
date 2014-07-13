package simple_prelude

import scala.language.higherKinds

class EitherMonad[E] extends Monad[({type L[A] = Either[E, A]})#L] {
  override def fmap[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
    fa match {
      case l@Left(_) => l.asInstanceOf[Either[E, B]]
      case Right(a) => Right(f(a))
    }
  override def pure[A](a: A): Either[E, A] = Right(a)
  override def joinWith[A, B, Z](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => Z): Either[E, Z] =
    (fa, fb) match {
      case (l@Left(_), _) => l.asInstanceOf[Either[E, Z]]
      case (_, l@Left(e)) => l.asInstanceOf[Either[E, Z]]
      case (Right(a), Right(b)) => Right(f(a, b))
    }
  override def sequence[A, Z](fas: Seq[Either[E, A]])(f: Seq[A] => Z): Either[E, Z] = {
    val as = Seq.newBuilder[A]
    fas.foreach { fa =>
      fa match {
        case l@Left(_) => return l.asInstanceOf[Either[E, Z]]
        case Right(a) => as += a
      }
    }
    Right(f(as.result))
  }
  override def bind[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
    fa match {
      case l@Left(_) => l.asInstanceOf[Either[E, B]]
      case Right(a) => f(a)
    }
}

class EitherTMonad[E, M[_]](monad: Monad[M]) extends Monad[({type L[A] = EitherT[E, M, A]})#L] {
  private[this] val eitherMonad = new EitherMonad[E]
  override def fmap[A, B](fa: EitherT[E, M, A])(f: A => B): EitherT[E, M, B] =
    monad.fmap(fa) { ea => eitherMonad.fmap(ea)(f) }
  override def pure[A](a: A): EitherT[E, M, A] =
    monad.pure(eitherMonad.pure(a))
  override def joinWith[A, B, Z](fa: EitherT[E, M, A], fb: EitherT[E, M, B])(f: (A, B) => Z): EitherT[E, M, Z] =
    monad.joinWith(fa, fb) { case (ea, eb) => eitherMonad.joinWith(ea, eb)(f) }
  override def sequence[A, Z](fas: Seq[EitherT[E, M, A]])(f: Seq[A] => Z): EitherT[E, M, Z] =
    monad.sequence(fas) { eas => eitherMonad.sequence(eas)(f) }
  override def bind[A, B](fa: EitherT[E, M, A])(f: A => EitherT[E, M, B]): EitherT[E, M, B] =
    monad.bind(fa) { ea =>
      ea match {
        case l@Left(_) => monad.pure(l.asInstanceOf[Either[E, B]])
        case Right(a) => f(a)
      }
    }
}
