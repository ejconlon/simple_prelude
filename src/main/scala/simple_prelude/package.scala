package object simple_prelude {
  type Identity[A] = A

  type Const[X, A] = X

  type Kleisli[M[_], A, B] = A => M[B]

  type CoKleisli[W[_], A, B] = W[A] => B

  type Reader[R, A] = R => A

  type Writer[W, A] = (A, W)

  type State[S, A] = S => (A, S)

  type Susp[X] = Reader[Unit, X]

  type ReaderT[R, F[_], X] = R => F[X]

  type SuspT[F[_], X] = ReaderT[Unit, F, X]

  type LensLike[F[_], S, T, A, B] = (A => F[B]) => (S => F[T])

  def id[A]: A => A = { (a: A) => a }

  def const[A, B](b: B): A => B = { (_: A) => b }
}
