package simple_prelude

import scala.language.higherKinds

object Prelude {

  //trait MonadTrans[T[_[_], _]] {
  //  def lift[M[_], A](monad: Monad[M])(m: M[A]): T[M, A]
  //}

  // ???
  //class ReaderTMonadTrans[R] extends MonadTrans[({type L[F[_], A] = ReaderT[R, F, A]})#L] {
  //  override def lift[M[_], A](monad: Monad[M])(m: M[A]): ReaderT[R, M, A] = { (_: R) => m }
  //}


}
