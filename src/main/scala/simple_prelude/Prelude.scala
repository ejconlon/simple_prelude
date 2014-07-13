package simple_prelude

import scala.language.higherKinds

object Prelude {

  trait MonadTrans[T[_[_], _]] {
    def lift[M[_], A](m: M[A]): T[M, A]
    def liftClass[M[_]](monad: Monad[M]): Monad[({type L[A] = T[M, A]})#L]
  }

  class ReaderTMonadTrans[R] extends MonadTrans[({type L[F[_], A] = ReaderT[R, F, A]})#L] {
    override def lift[M[_], A](m: M[A]): ReaderT[R, M, A] = { (_: R) => m }
    override def liftClass[M[_]](monad: Monad[M]): ReaderTMonad[R, M] = new ReaderTMonad[R, M](monad)
  }

}
