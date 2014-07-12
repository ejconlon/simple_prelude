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
