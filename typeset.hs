import Control.Monad (guard)
import Control.Applicative (Alternative(..))

data Expr = UnitE | IntE Int | PairE Expr Expr | Proj1E Expr | Proj2E Expr | Inj1E Expr | Inj2E Expr
data Ty = UnitTy | IntTy | ProdTy Ty Ty | SumTy Ty Ty deriving (Show, Eq)

class BooleanAlg a where
    top :: a
    bot :: a
    (\/) :: a -> a -> a
    (/\) :: a -> a -> a

instance BooleanAlg [Ty] where
    -- Sums of types
    a \/ b = do
        x <- a;
        y <- b;
        return $ SumTy x y

    -- Products of types
    a /\ b = do
        x <- a;
        y <- b;
        return $ ProdTy x y

    -- Bottom
    bot = []

    -- Top, the maximal infinite tree
    top = [UnitTy] <|> [IntTy] <|> (top /\ top) <|> (top \/ top)

infer' :: Expr -> [Ty]
infer' (UnitE) = pure $ UnitTy
infer' (IntE _) = pure $ IntTy
infer' (PairE x y) = infer' x /\ infer' y
infer' (Proj1E e) = do
    p <- infer' e;
    x <- top;
    y <- top;
    guard $ p == (ProdTy x y);
    return x
infer' (Proj2E e) = do
    p <- infer' e;
    x <- top;
    y <- top;
    guard $ p == (ProdTy x y);
    return y
infer' (Inj1E e) = do
    x <- infer' e;
    y <- top;
    return $ SumTy x y
infer' (Inj2E e) = do
    x <- top;
    y <- infer' e;
    return $ SumTy x y

-- infer' does not terminate, use [head] to stop it
infer :: Expr -> Ty
infer e = head $ infer' e

typecheck :: Expr -> Ty -> Bool
typecheck e t = t `elem` (infer' e)
