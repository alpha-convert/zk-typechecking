import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Control.Monad (join)

data Expr = IntE Int | PairE Expr Expr | Proj1E Expr | Proj2E Expr

data Ty = IntTy | ProdTy Ty Ty deriving (Show)

data ExprMap a = Emp | Node { intT :: Int -> Maybe a, pairT :: ExprMap (ExprMap a), proj1T :: ExprMap a, proj2T :: ExprMap a}

instance Functor ExprMap where
    fmap f Emp = Emp
    fmap f (Node m1 m2 m3 m4) = Node (fmap f <$> m1) (fmap f <$> m2) (f <$> m3) (f <$> m4)


fmapMaybe :: (a -> Maybe b) -> ExprMap a -> ExprMap b
fmapMaybe f Emp = Emp
fmapMaybe f (Node m1 m2 m3 m4) = Node ((f =<<) <$> m1) (fmapMaybe f <$> m2) (fmapMaybe f m3) (fmapMaybe f m4)

lookup :: ExprMap a -> Expr -> Maybe a
lookup Emp _ = Nothing
lookup (Node m _ _ _) (IntE n) = m n
lookup (Node _ m _ _) (PairE e1 e2) = lookup m e1 >>= (`lookup` e2)
lookup (Node _ _ m _) (Proj1E e) = lookup m e
lookup (Node _ _ _ m) (Proj2E e) = lookup m e

unionWith :: (a -> a -> a) -> ExprMap a -> ExprMap a -> ExprMap a
unionWith _ Emp m = m
unionWith _ m Emp = m
unionWith f (Node m1 m2 m3 m4) (Node m1' m2' m3' m4') = Node (h m1 m1') (unionWith (unionWith f) m2 m2') (unionWith f m3 m3') (unionWith f m4 m4')
    where
        h m m' x = case (m x, m' x) of
                     (Just y,Just y') -> Just (f y y')
                     (Nothing,m'x) -> m'x
                     (mx,Nothing) -> mx

inferMap :: ExprMap Ty
inferMap = Node  {intT = const $ Just IntTy, pairT = pairinferMap, proj1T = fmapMaybe extractPi1 inferMap, proj2T = fmapMaybe extractPi1 inferMap }
    where
        pairinferMap :: ExprMap (ExprMap Ty)
        pairinferMap = (\t1 -> ProdTy t1 <$> inferMap) <$> inferMap
        extractPi1 (ProdTy t _) = Just t
        extractPi1 _ = Nothing
        extractPi2 (ProdTy _ t) = Just t
        extractPi2 _ = Nothing
-- >>> lookup inferMap (PairE (PairE (IntE 4) (IntE 5)) (IntE 3))
-- Just (ProdTy (ProdTy IntTy IntTy) IntTy)

-- >>> lookup inferMap (Proj1E (PairE (PairE (IntE 4) (IntE 5)) (IntE 3)))
-- Just (ProdTy IntTy IntTy)

-- >>> lookup inferMap (Proj2E (Proj1E (PairE (PairE (IntE 4) (IntE 5)) (IntE 3))))
-- Just IntTy

-- SHOULD NOT TYPECHECK
-- >>> lookup inferMap (Proj1E (IntE 4))
-- Nothing
