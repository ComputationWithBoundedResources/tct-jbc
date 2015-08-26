module Tct.Jbc.Encoding.ArgumentFiltering (mkFilter) where


import qualified Data.IntSet                        as IS (fromAscList, intersection)

import qualified Data.Rewriting.Rule                as RT hiding (vars)

import qualified Tct.Trs.Encoding.ArgumentFiltering as AF


mkFilter :: Ord f => (RT.Term f t -> Bool) -> [RT.Rule f t] -> AF.ArgumentFiltering f
mkFilter f = foldr k AF.empty where
  k (RT.Rule (RT.Fun l ls) (RT.Fun r rs)) af = AF.alter (add ls) l $ AF.alter (add rs) r af
  k _ af                                     = af
  add ts (Just (AF.Filtering is)) = Just . AF.Filtering $ is `IS.intersection` univs ts
  add ts _                        = Just . AF.Filtering $ univs ts
  univs = IS.fromAscList . fst . unzip . filter (f . snd) . zip [(1::Int)..]
