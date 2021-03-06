import scala.language.higherKinds

import scala.util.Try

package object simple_prelude {
  case object TodoException extends RuntimeException("TODO")

  type Identity[A] = A

  type Const[X, A] = X

  type Compose[F[_], G[_], A] = G[F[A]]

  sealed trait Product[F[_], G[_], A]
  case class PLeft[F[_], G[_], A](fa: F[A]) extends Product[F, G, A]
  case class PRight[F[_], G[_], A](ga: G[A]) extends Product[F, G, A]

  type Kleisli[M[_], A, B] = A => M[B]

  type CoKleisli[W[_], A, B] = W[A] => B

  type IdentityT[M[_], X] = M[X]

  type ReaderT[R, M[_], X] = R => M[X]

  type WriterT[W, M[_], X] = M[(X, W)]

  type StateT[S, M[_], X] = S => M[(X, S)]

  type OptionT[M[_], X] = M[Option[X]]

  type EitherT[E, M[_], X] = M[Either[E, X]]

  type TryT[M[_], X] = M[Try[X]]

  type SeqT[M[_], X] = M[Seq[X]]

  type Reader[R, A] = R => A

  type Writer[W, A] = (A, W)

  type State[S, A] = S => (A, S)

  type Susp[X] = Reader[Unit, X]

  type SuspT[F[_], X] = ReaderT[Unit, F, X]

  type LensLike[F[_], S, T, A, B] = (A => F[B]) => (S => F[T])

  type Algebra[F[_], X] = F[X] => X

  type Coalgebra[F[_], X] = X => F[X]

  def id[A]: A => A = { (a: A) => a }

  def const[A, B](b: B): A => B = { (_: A) => b }
}
