module GameStateUtil (
) where
import GameStateTypes

data Distance = Dist Int Int

verb :: Entity -> Entity -> Distance
verb e1 e2 = Dist (posX e1 - posX e2) (posY e1 - posY e2)

--A `closerThan` 10 `to` B

closerThan = (,)
to (x,dist) y = x `verb` y `closerThan'` dist

closerThan' :: Distance -> Int -> Bool
closerThan' (Dist x y) thres = x^2 + y^2 <= thres^2

query :: GameState -> (Entity -> Bool) -> [Entity]
query gs pred = filter pred (entities gs)

getEntitiesCloseToWithMinDist :: GameState -> Entity -> Int -> [Entity]
getEntitiesCloseToWithMinDist gs e dist = gs `query` ((/= e) &&& (e `closerThan` dist `to`))

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&&) preda predb val = preda val && predb val
