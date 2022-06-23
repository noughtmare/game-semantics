module Game where


data Arena a = MkArena { enabling :: a -> a -> Bool }

type Play a = [a]

data NatMove = Q | N Int

natArena :: Arena NatMove
natArena = MkArena natEnabling where
  natEnabling Q (N _) = True
  natEnabling _ _ = False
