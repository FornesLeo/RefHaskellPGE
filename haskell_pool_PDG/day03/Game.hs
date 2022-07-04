import Data.Maybe

data Item = Sword | Bow | MagicWand
    deriving (Eq)

instance Show Item where
    show = showItem

showItem :: Item -> String
showItem Sword = "sword"
showItem Bow = "bow"
showItem MagicWand = "magic wand"

data Mob = Mummy | Skeleton Item | Witch (Maybe Item)
    deriving (Eq)

showMob :: Mob -> String
showMob Mummy = "mummy"
showMob (Skeleton Bow) = "doomed archer"
showMob (Skeleton Sword) = "dead knight"
showMob (Skeleton MagicWand) = "skeleton holding a " ++ show MagicWand
showMob (Witch Nothing) = "witch"
showMob (Witch (Just MagicWand)) = "sorceress"
showMob (Witch (Just Bow)) = "witch holding a " ++ show Bow
showMob (Witch (Just Sword)) = "witch holding a " ++ show Sword

instance Show Mob where
    show = showMob

createMummy :: Mob -- a Mummy
createMummy = Mummy
createArcher :: Mob -- a Skeleton holding a Bow
createArcher = Skeleton Bow
createKnight :: Mob -- a Skeleton holding a Sword
createKnight = Skeleton Sword
createWitch :: Mob -- a Which holding Nothing
createWitch = Witch Nothing
createSorceress :: Mob -- a Which holding a MagicWand
createSorceress = Witch (Just MagicWand)

create :: String -> Maybe Mob
create "mummy" = Just createMummy
create "doomed archer" = Just createArcher
create "dead knight" = Just createKnight
create "witch" = Just createWitch
create "sorceress" = Just createSorceress
create _ = Nothing

equip :: Item -> Mob -> Maybe Mob
equip _ Mummy = Nothing
equip x (Skeleton y) = Just (Skeleton x)
equip x (Witch y) =  Just (Witch (Just x))

class HasItem a where
    getItem :: a -> Maybe Item;
    hasItem :: a -> Bool;
    hasItem a = case getItem a of
        Nothing -> False
        Just _ -> True

instance HasItem Mob where
    getItem Mummy = Nothing
    getItem (Skeleton item) = Just item
    getItem (Witch item) = item

