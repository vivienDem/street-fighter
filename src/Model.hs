module Model where

import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq
import qualified SDL as SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Data.Foldable (toList)

-- taille de la zone de jeu
largeurZone :: Integer
largeurZone = 640

hauteurZone :: Integer 
hauteurZone = 480

-- donnees
data Zone = Zone Integer Integer deriving Show
data Coord = Coord Integer Integer deriving (Show, Eq)
data Direction = H | B | G | D deriving (Show, Eq)
data Mouvement = Mouv Direction Integer deriving Show
data Hitbox =
    Rect Coord Integer Integer
    | Composite (Seq Hitbox)
    deriving (Show, Eq)

data EtatCombattant = Ko
    | Ok Integer -- sa sante actuelle
    deriving (Show)

data Combattant =
    Comb {
    hitboxc :: Hitbox,
    facec :: Direction ,
    etatx :: EtatCombattant,
    hitTimer :: Integer, 
    dammage :: Integer,
    techniquec :: Technique,
    hitStun :: Integer,
    combo :: Integer
    }
    deriving(Show)

data Jeu =
    GameOver Integer -- le numero du joueur vainqueur
    | EnCours {
        joueur1 :: Combattant ,
        joueur2 :: Combattant ,
        zoneJeu :: Zone,
        projectiles :: Seq Projectile
    }
    deriving(Show)

data Technique = Rien
                |CoupDePoing
                |CoupDePied
                |Protection
                |Encaisse
                |Accroupi
                |Lancer
                deriving (Eq, Show)

data Projectile =
    Proj {
        proprietaire :: Integer,
        hitbox :: Hitbox,
        puissance :: Integer,
        mouvp :: Mouvement
    }
    deriving(Show)

-- _invariants

prop_Zone_inv :: Zone -> Bool -- Une zone a une largeur et une hauteur positive.
prop_Zone_inv (Zone l h) = l >= 0 && h >= 0

prop_Coord_inv :: Coord -> Zone -> Bool -- Une coordonnee est contenue dans la zone de jeu. (0,0) le coin superieur gauche de notre repere.
prop_Coord_inv (Coord x y) (Zone l h) = x >= 0 && y >= 0 && x <= l && y <= h

prop_Mouvement_inv :: Mouvement -> Bool -- Un deplacement est positif. On ne peut pas reculer.
prop_Mouvement_inv (Mouv dir dep) = dep >= 0

prop_Hitbox_inv :: Hitbox -> Zone -> Bool -- Une hitbox est entierement contenue dans la zone de jeu et possede une aire positive non nulle.
prop_Hitbox_inv (Rect (Coord x y) l h) (Zone lz hz) = (prop_Coord_inv (Coord x y) (Zone lz hz)) && x+l <= lz && y+h <= hz && l*h > 0
prop_Hitbox_inv (Composite hbs) z =
    foldr (\x acc -> acc && prop_Hitbox_inv x z) True hbs

prop_EtatCombattant_inv :: EtatCombattant -> Bool -- L'etat d'un combattant en vie correspond a un nombre de point de vie strictement positif.
prop_EtatCombattant_inv Ko = True
prop_EtatCombattant_inv (Ok n) = n > 0

prop_Combattant_inv :: Combattant -> Zone -> Bool -- Un combattant possede une hitbox valide, un etat valide. Il ne peut pas causer de dommage negatif
                                         -- ni avoir un hitTimer ou un hitStun negatif.
prop_Combattant_inv (Comb hitboxc facec etatx hitTimer damage _ hitStun cbo) z =
  (prop_Hitbox_inv hitboxc z) && (prop_EtatCombattant_inv etatx) && hitTimer >= 0 && damage >= 0 && hitStun >= 0 && cbo >= 0 

prop_Jeu_inv :: Jeu -> Bool -- Si le jeu est fini, le jeu indique si c'est le joueur 1 qui a perdu ou le joueur 2
                           -- Sinon, les deux joueurs doivent etre en vie, la zone de jeu doit etre valide, les projectiles doivent etre valides et verifier leur invariant
prop_Jeu_inv (GameOver n) = n == 1 || n == 2
prop_Jeu_inv (EnCours c1 c2 z projectiles) = (en_vie c1) && (en_vie c2) && (prop_Zone_inv z) && (Seq.length projectiles <= 2) 
                                            && (prop_Combattant_inv c1 z) && (prop_Combattant_inv c2 z)
                                            && (foldr (\proj acc -> acc && (prop_Projectile_inv proj zone)) True projectiles)

prop_Projectile_inv :: Projectile -> Zone -> Bool -- Un projectile doit etre detenu par le joueur 1 ou le joueur 2
                                                 -- Le projectile doit se situer dans la zone 
                                                 -- Sa puissance doit etre strictement superieure a 0
                                                 -- Sa direction doit etre G ou D et son deplacement doit etre strictement positif
prop_Projectile_inv (Proj prop hb puiss mouv@(Mouv direction deplacement)) zone = 
    (prop == 1 || prop == 2) && prop_Hitbox_inv hb zone && puiss > 0 && (direction == G || direction == D) && prop_Mouvement_inv mouv

-- pre_-conditions

prop_pre_Appartient :: Coord -> Hitbox -> Zone -> Bool
prop_pre_Appartient c h z = prop_Coord_inv c z && prop_Hitbox_inv h z

prop_pre_BougeHitboxSafe :: Hitbox -> Mouvement -> Zone -> Bool
prop_pre_BougeHitboxSafe h m z =
    prop_Zone_inv z && prop_Hitbox_inv h z && prop_Mouvement_inv m 

prop_pre_Jeu :: Jeu -> Bool
prop_pre_Jeu (GameOver n) = False
prop_pre_Jeu (EnCours c1 c2 z _) = (en_vie c1) && (en_vie c2)

prop_pre_Collision :: Hitbox -> Hitbox -> Zone -> Bool
prop_pre_Collision h1 h2 z = (prop_Hitbox_inv h1 z) && (prop_Hitbox_inv h2 z)

prop_pre_Initjoueur1 :: Integer -> Integer -> Integer -> Integer -> Technique -> Integer -> Direction -> Integer -> Integer -> Zone -> Bool
prop_pre_Initjoueur1 x y l h tech pv direction ht hs (Zone lz hz) = 
    pv > 0 && ht >= 0 && hs >= 0 && x >= 0 && y >= 0 && x < lz && y < hz && l*h > 0

prop_pre_Initjoueur2 :: Integer -> Integer -> Integer -> Integer -> Technique -> Integer -> Direction -> Integer -> Integer -> Zone -> Bool
prop_pre_Initjoueur2 x y l h tech pv direction ht hs zone = prop_pre_Initjoueur1 x y l h tech pv direction ht hs zone

prop_pre_InitHitboxProj :: Integer -> Bool
prop_pre_InitHitboxProj num = num == 1 || num == 2

prop_pre_PositionJoueurX :: Combattant -> Zone -> Bool
prop_pre_PositionJoueurX c zone = prop_Combattant_inv c zone

prop_pre_PositionJoueurY :: Combattant -> Zone -> Bool
prop_pre_PositionJoueurY c zone = prop_Combattant_inv c zone

prop_pre_PositionProjectileX :: Projectile -> Zone -> Bool
prop_pre_PositionProjectileX projectile zone = prop_Projectile_inv projectile zone

prop_pre_PositionProjectileY :: Projectile -> Zone -> Bool
prop_pre_PositionProjectileY projectile zone = prop_Projectile_inv projectile zone

prop_pre_ActualiserFace :: Integer -> Jeu -> Bool
prop_pre_ActualiserFace num jeu = (num == 1 || num == 2) && prop_Jeu_inv jeu 

prop_pre_BougeJoueur :: Integer -> Mouvement -> Jeu -> Bool 
prop_pre_BougeJoueur num mouv jeu = (num == 1 || num == 2) && prop_Jeu_inv jeu && prop_Mouvement_inv mouv

prop_pre_BougeJoueurJeu :: Integer -> Mouvement -> Jeu -> Bool
prop_pre_BougeJoueurJeu num mouv jeu = prop_pre_BougeJoueur num mouv jeu

prop_pre_bougeProjectilesJeu :: Jeu -> Bool
prop_pre_bougeProjectilesJeu jeu = prop_Jeu_inv jeu

prop_pre_techniqueJeu :: Integer -> Bool
prop_pre_techniqueJeu num = num == 1 || num == 2

prop_pre_ajouteProjectile :: Integer -> Jeu -> Bool
prop_pre_ajouteProjectile num jeu = (num == 1 || num == 2) && prop_Jeu_inv jeu

prop_pre_changerposture :: Integer -> Bool
prop_pre_changerposture num = num == 1 || num == 2

-- post-conditions

prop_post_BougeCoordSafe :: Coord -> Mouvement -> Zone -> Bool
prop_post_BougeCoordSafe c m z =
    case (bougeCoordSafe c m z) of
        Nothing -> True
        Just c' -> prop_Coord_inv c' z

prop_post_BougeHitboxSafe :: Hitbox -> Mouvement -> Zone -> Bool
prop_post_BougeHitboxSafe h m z = case (bougeHitboxSafe h m z) of
    Nothing -> True
    Just hb -> prop_Hitbox_inv hb z

prop_post_bougeCoord :: Coord -> Integer -> Bool
prop_post_bougeCoord c deplacement =
    c == bougeCoord (bougeCoord c (Mouv G deplacement)) (Mouv D deplacement)

prop_post_Initjoueur1 :: Integer -> Integer -> Integer -> Integer -> Technique -> Integer -> Direction -> Integer -> Integer -> Integer -> Zone -> Bool
prop_post_Initjoueur1 x y l h tech pv direction ht hs cbo zone = 
    case initJoueur1 x y l h tech pv direction ht hs cbo  of 
        c@(Comb hb dir etat ht' deg tech' hs' cbo') -> prop_Hitbox_inv hb zone && prop_Combattant_inv c zone && dir == direction && ht == ht' && hs == hs' && cbo == cbo'

prop_post_Initjoueur2 :: Integer -> Integer -> Integer -> Integer -> Technique -> Integer -> Direction -> Integer -> Integer -> Integer -> Zone -> Bool
prop_post_Initjoueur2 x y l h tech pv direction ht hs cbo zone = prop_post_Initjoueur1 x y l h tech pv direction ht hs cbo zone

prop_post_PositionJoueurX :: Combattant -> Zone -> Bool
prop_post_PositionJoueurX c zone = prop_Combattant_inv c zone

prop_post_PositionJoueurY :: Combattant -> Zone -> Bool
prop_post_PositionJoueurY c zone = prop_Combattant_inv c zone

prop_post_PositionProjectileX :: Projectile -> Zone -> Bool
prop_post_PositionProjectileX projectile zone = prop_Projectile_inv projectile zone

prop_post_PositionProjectileY :: Projectile -> Zone -> Bool
prop_post_PositionProjectileY projectile zone = prop_Projectile_inv projectile zone

prop_post_ActualiserFace :: Integer -> Mouvement -> Jeu -> Bool
prop_post_ActualiserFace num mouv (GameOver gagnant) = True 
prop_post_ActualiserFace num mouv jeu = 
    let jeu'@(EnCours c1 c2 _ _) = actualiserFace num mouv jeu in
        prop_Jeu_inv jeu' && (facec c1 == D || facec c1 == G) && (facec c2 == D || facec c2 == G)

prop_post_PointsDeVie :: Combattant -> Bool
prop_post_PointsDeVie c = pointsDeVie c >= 0

prop_post_BougeJoueur :: Integer -> Hitbox -> Mouvement -> Zone -> Combattant -> Bool
prop_post_BougeJoueur num hbAdv mouv zone c = prop_Combattant_inv (bougeJoueur num hbAdv mouv zone c) zone

prop_post_BougeJoueurJeu :: Integer -> Mouvement -> Jeu -> Bool
prop_post_BougeJoueurJeu num mouv jeu = prop_Jeu_inv (bougeJoueurJeu num mouv jeu)

prop_post_bougeProjectile :: Projectile -> Combattant -> Zone -> Bool
prop_post_bougeProjectile proj c zone = 
    let (proj', c') = bougeProjectile proj c zone in
        case proj' of 
            Nothing -> prop_Combattant_inv c' zone
            Just proj'' -> prop_Combattant_inv c' zone && prop_Projectile_inv proj'' zone

prop_post_bougeProjectilesJeu :: Jeu -> Bool
prop_post_bougeProjectilesJeu jeu = prop_Jeu_inv (bougeProjectilesJeu jeu)

prop_post_techniqueJeu :: Integer -> Technique -> Jeu -> Bool 
prop_post_techniqueJeu num tech jeu = prop_Jeu_inv (techniqueJeu num tech jeu)

prop_post_ajouteProjectile :: Integer -> Jeu -> Bool 
prop_post_ajouteProjectile num jeu = 
    case ajouteProjectile num jeu of
        GameOver _ -> True 
        EnCours _ _ _ projectiles -> Seq.length projectiles <= 2

prop_post_changerposture :: Integer -> Technique -> Combattant -> Zone -> Bool
prop_post_changerposture num tech c zone = prop_Combattant_inv c zone

prop_post_degats :: Combattant -> Integer -> Bool
prop_post_degats c d = 
    let pv = pointsDeVie c in 
        techniquec (degats c d) == Encaisse && pointsDeVie c == max 0 (pv - d)

prop_post_prochaineDirection :: Combattant -> Combattant -> Bool
prop_post_prochaineDirection c1 c2 = 
    let direction = prochaineDirection c1 c2 in 
        direction == D || direction == G

prop_post_gameStep :: Jeu -> Keyboard -> Bool
prop_post_gameStep jeu kbd = prop_Jeu_inv (gameStep jeu kbd)

prop_post_gameStepRandom :: Integer -> Jeu -> Keyboard -> Bool
prop_post_gameStepRandom rand jeu kbd = prop_Jeu_inv (gameStep jeu kbd)

-- Constructeurs

initZone l h =
    let z = (Zone l h) in
        case (prop_Zone_inv z) of
            True -> z
            _ -> error "_invalid Zone"


initSimpleHitbox :: Coord -> Integer -> Integer -> Zone -> Hitbox 
initSimpleHitbox c l h z =           -- On construit une hitbox comme étant un rectangle valide..
    let hb = (Rect c l h) in
        case (prop_Hitbox_inv hb z) of
            True -> hb
            False -> error ("_invalid hitbox : " ++ (show c) ++ " " ++ (show l) ++ " " ++ (show h))

initHitbox :: Seq (Coord, Integer, Integer) -> Zone -> Hitbox 
initHitbox s z = case (length s) of  -- ..ou une sequence de rectangles valides
    0 -> error ("_invalid hitbox : " ++ (show s))
    1 -> let (c,l,h) = Seq.index s 0 in
        initSimpleHitbox c l h z
    _ -> Composite (fmap f s) where
        f (c, l, h) = initSimpleHitbox c l h z

initEtat :: EtatCombattant
initEtat = Ok 100

initJoueur1 :: Integer -> Integer -> Integer -> Integer -> Technique -> Integer -> Direction -> Integer -> Integer -> Integer -> Combattant
initJoueur1 x y l h Rien pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min y (hauteurZone - 119))), l, h)]) (initZone 640 480)) direction (Ok pv) ht 0 Rien hs cbo
initJoueur1 x y l h CoupDePoing pv D ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x + l) (y + 22)), 36, 12)]) (initZone 640 480)) D (Ok pv) ht 2 CoupDePoing hs cbo
initJoueur1 x y l h CoupDePoing pv G ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x - 36) (y + 22)), 36, 12)]) (initZone 640 480)) G (Ok pv) ht 2 CoupDePoing hs cbo
initJoueur1 x y l h CoupDePied pv D ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x + l) (y + 30)), 54, 11)]) (initZone 640 480)) D (Ok pv) ht 5 CoupDePied hs cbo
initJoueur1 x y l h CoupDePied pv G ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x - 54) (y + 30)), 54, 11)]) (initZone 640 480)) G (Ok pv) ht 5 CoupDePied hs cbo
initJoueur1 x y l h Protection pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), (l-1), h)]) (initZone 640 480)) direction (Ok pv) ht 0 Protection hs cbo
initJoueur1 x y l h Encaisse pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), (l+4), (h-4))]) (initZone 640 480)) direction (Ok pv) ht 0 Encaisse hs cbo
initJoueur1 x y l h Accroupi pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min (y + 51) (hauteurZone - 68))), 80, 68)]) (initZone 640 480)) direction (Ok pv) ht 0 Accroupi hs cbo
initJoueur1 x y l h Lancer pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min y (hauteurZone - 119))), (l + 55), h)]) (initZone 640 480)) direction (Ok pv) ht 0 Lancer hs cbo

initJoueur2 :: Integer -> Integer -> Integer -> Integer -> Technique ->  Integer -> Direction -> Integer -> Integer -> Integer -> Combattant
initJoueur2 x y l h Rien pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min y (hauteurZone - 126))), l, h)]) (initZone 640 480)) direction (Ok pv) ht 0 Rien hs cbo
initJoueur2 x y l h CoupDePoing pv G ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x - 55) (y + 26)), 55, 12)]) (initZone 640 480)) G (Ok pv) ht 5 CoupDePoing hs cbo
initJoueur2 x y l h CoupDePoing pv D ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x + l) (y + 26)), 55, 12)]) (initZone 640 480)) D (Ok pv) ht 5 CoupDePoing hs cbo
initJoueur2 x y l h CoupDePied pv G ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x - 72) (y + 45)), 72, 13)]) (initZone 640 480)) G (Ok pv) ht 2 CoupDePied hs cbo
initJoueur2 x y l h CoupDePied pv D ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), l, h), ((Coord  (x + l) (y + 45)), 72, 13)]) (initZone 640 480)) D (Ok pv) ht 2 CoupDePied hs cbo
initJoueur2 x y l h Protection pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), (l+13), h)]) (initZone 640 480)) direction (Ok pv) ht 0 Protection hs cbo
initJoueur2 x y l h Encaisse pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x y), (l+6), (h+18))]) (initZone 640 480)) direction (Ok pv) ht 0 Encaisse hs cbo
initJoueur2 x y l h Accroupi pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min (y + 42) (hauteurZone - 84))), (l+32), (h-42))]) (initZone 640 480)) direction (Ok pv) ht 0 Accroupi hs cbo
initJoueur2 x y l h Lancer pv direction ht hs cbo = Comb (initHitbox (Seq.fromList [ ((Coord x (min y (hauteurZone - 126))), (l + 64), h)]) (initZone 640 480)) direction (Ok pv) ht 0 Lancer hs cbo

initHitboxProj :: Integer -> Combattant -> Hitbox
initHitboxProj 1 c = 
    let x = if (facec c == D) 
            then (positionJoueurX c) + 119
            else (positionJoueurX c) - 52
    in 
    Rect (Coord x (positionJoueurY c + 11)) 52 33  
initHitboxProj 2 c = 
    let x = if (facec c == D) 
            then (positionJoueurX c) + 127
            else (positionJoueurX c) - 52
    in 
    Rect (Coord x (positionJoueurY c + 25)) 52 33 

initJeu :: Jeu
initJeu = EnCours (initJoueur1 150 (hauteurZone - 119) 64 119 Rien 100 D 0 0 0) (initJoueur2 450 (hauteurZone - 126) 63 126 Rien 100 G 0 0 0) (initZone 640 480) Seq.Empty

-- Coordonnees
bougeCoord :: Coord -> Mouvement -> Coord
bougeCoord (Coord x y) (Mouv H dy) = Coord x (y - dy)
bougeCoord (Coord x y) (Mouv B dy) = Coord x (y + dy)
bougeCoord (Coord x y) (Mouv G dx) = Coord (x - dx) y
bougeCoord (Coord x y) (Mouv D dx) = Coord (x + dx) y


bougeCoordSafe :: Coord -> Mouvement -> Zone -> Maybe Coord
bougeCoordSafe c m (Zone largeur hauteur) =
    let (Coord x y) = (bougeCoord c m) in
        if (x < 0 || x > largeur || y < 0 || y > hauteur)
             then Nothing
             else Just (Coord x y)

appartient :: Coord -> Hitbox -> Bool
appartient (Coord px py) (Rect (Coord x y) largeur hauteur) =
    px <= (x + largeur) && px >= x && py >= y && py <= (y + hauteur)
appartient c (Composite hbs) = foldr aux False hbs where
        aux :: Hitbox -> Bool -> Bool
        aux elem acc = acc || (appartient c elem)

bougeHitbox :: Hitbox -> Mouvement -> Hitbox
bougeHitbox (Rect c l h) m = (Rect (bougeCoord c m) l h)
bougeHitbox (Composite hbs) m = Composite (fmap (\x -> bougeHitbox x m) hbs)

bougeHitboxSafe :: Hitbox -> Mouvement -> Zone -> Maybe Hitbox
bougeHitboxSafe (Rect c l h) m (Zone largeur hauteur) =
    let (Rect (Coord px py) l' h') = bougeHitbox (Rect c l h) m in
        let zone = (Rect (Coord 0 0) largeur hauteur) in
        if ((appartient (Coord px py) zone)
            && (appartient (Coord (px + l') py) zone)
            && (appartient (Coord px (py + h')) zone)
            && (appartient (Coord (px + l') (py + h')) zone))
            then Just ((Rect (Coord px py) l' h'))
            else Nothing

bougeHitboxSafe (Composite hbs) m (Zone largeur hauteur) =
    let (Composite hbs') = bougeHitbox (Composite hbs) m in
        let zone = Rect (Coord 0 0) largeur hauteur in
            let mouvementValide = foldr f True hbs' where
                f :: Hitbox -> Bool -> Bool
                f elem acc = case elem of
                    (Rect (Coord px py) l h) -> acc && (appartient (Coord px py) zone)
                        && (appartient (Coord (px + l) py) zone)
                        && (appartient (Coord px (py + h)) zone)
                        && (appartient (Coord (px + l) (py + h)) zone)
                    (Composite hbs'') -> acc && (foldr f acc hbs'')
            in
                if (mouvementValide)
                    then Just (Composite hbs')
                    else Nothing


collision :: Hitbox -> Hitbox -> Bool
collision (Rect (Coord x1 y1) l1 h1) (Rect (Coord x2 y2) l2 h2) =
    not (x1+l1<x2 || x2+l2<x1 || y1+h1<y2 || y2+h2<y1)
collision (Rect (Coord x1 y1) l1 h1) (Composite hbs) =
    foldr (\x acc -> acc || collision x (Rect (Coord x1 y1) l1 h1)) False hbs
collision (Composite hbs) (Rect c l1 h1) =
    collision (Rect c l1 h1) (Composite hbs)
collision (Composite hbs1) (Composite hbs2) =
    foldr (\x acc -> acc || collision x (Composite hbs2)) False hbs1


positionJoueurX :: Combattant -> Integer
positionJoueurX (Comb (Rect (Coord x y) l _) _ _ _ _ _ _ _) = x
positionJoueurX c1@(Comb (Composite hbs) _ _ _ _ _ _ _) = 
    case Seq.lookup 0 hbs of
        Nothing -> 0
        Just h -> positionJoueurX c1{hitboxc = h}

positionJoueurY :: Combattant -> Integer
positionJoueurY (Comb (Rect (Coord x y) l _) _ _ _ _ _ _ _) = y
positionJoueurY c1@(Comb (Composite hbs) _ _ _ _ _ _ _) = 
    case Seq.lookup 0 hbs of
        Nothing -> 0
        Just h -> positionJoueurY c1{hitboxc = h}

positionProjectileX :: Projectile -> Integer
positionProjectileX projectile@(Proj _ (Rect (Coord x y) l _) _ _) = x
positionProjectileX projectile@(Proj _ (Composite hbs) _ _) = 
    case Seq.lookup 0 hbs of
        Nothing -> 0
        Just h -> positionProjectileX projectile{hitbox = h}

positionProjectileY :: Projectile -> Integer
positionProjectileY projectile@(Proj _ (Rect (Coord x y) l _) _ _) = y
positionProjectileY projectile@(Proj _ (Composite hbs) _ _) = 
    case Seq.lookup 0 hbs of
        Nothing -> 0
        Just h -> positionProjectileY projectile{hitbox = h}

actualiserFace :: Integer -> Mouvement -> Jeu -> Jeu 
actualiserFace 1 (Mouv direction _) (EnCours joueur1 joueur2 zone projectiles) = 
    let d = if (direction == B || direction == H || (hitTimer joueur1 /= 0) || (hitStun joueur1 /= 0))
            then facec joueur1
            else direction
    in
    EnCours joueur1{facec = d} joueur2 zone projectiles
actualiserFace 2 (Mouv direction _) (EnCours joueur1 joueur2 zone projectiles) = 
    let d = if (direction == B || direction == H || (hitTimer joueur2 /= 0) || (hitStun joueur2 /= 0))
            then facec joueur2
            else direction
    in
    EnCours joueur1 joueur2{facec = d} zone projectiles
actualiserFace _ _ jeu = jeu


messageDeFin :: Jeu -> String
messageDeFin (GameOver num) = "Le joueur " ++ (show num) ++ " a gagné"
messageDeFin _ = "Egalité"

en_vie :: Combattant -> Bool
en_vie c = case (etatx c) of
  Ko -> False
  Ok n -> n > 0

pointsDeVie :: Combattant -> Integer 
pointsDeVie c = case (etatx c) of
    Ko -> 0
    Ok n -> n


bougeJoueur :: Integer -> Hitbox -> Mouvement -> Zone -> Combattant -> Combattant
bougeJoueur num hbAdversaire m@(Mouv direction pixels) z c1@(Comb h _ _ hitTimer _ _ hitStun _)=
    if direction /= B && (hitTimer > 0 || hitStun > 0) then c1
    else 
        case bougeHitboxSafe h m z of
            Nothing -> c1
            Just hb' ->  if collision hb' hbAdversaire
                                then (changerposture num Rien c1)
                                else (changerposture num Rien c1{hitboxc = hb'})

bougeJoueurJeu :: Integer -> Mouvement -> Jeu -> Jeu 
bougeJoueurJeu 1 m (EnCours c1 c2 zone projectiles) = actualiserFace 1 m (EnCours (bougeJoueur 1 (hitboxc c2) m zone c1) c2 zone projectiles)
bougeJoueurJeu 2 m (EnCours c1 c2 zone projectiles) = actualiserFace 2 m (EnCours c1 (bougeJoueur 2 (hitboxc c1) m zone c2) zone projectiles)
bougeJoueurJeu _ _ jeu = jeu

bougeProjectile :: Projectile -> Combattant -> Zone -> (Maybe Projectile, Combattant)
bougeProjectile projectile@(Proj prop hb puiss mouv) adv zone = 
    let adv' = if collision (hitboxc adv) hb && (hitStun adv == 0) 
             then (degats adv puiss)
             else adv
    in 
        case bougeHitboxSafe hb mouv zone of 
            Nothing -> (Nothing, adv')
            Just hb' -> ((Just projectile{hitbox = hb'}), adv')

bougeProjectilesJeu :: Jeu -> Jeu
bougeProjectilesJeu jeu@(EnCours c1 c2 zone projectiles) =
    foldr aux jeu{projectiles = Seq.Empty} projectiles where
        aux proj@(Proj prop hb puiss mouv) j@(EnCours c1' c2' _ projectiles') = 
            if (prop == 1) 
            then let (proj', c2'') = bougeProjectile proj c2' zone in
                case proj' of 
                    Nothing -> j{joueur2 = c2''}
                    Just proj'' -> j{joueur2 = c2'', projectiles = projectiles' Seq.|> proj''}
            else let (proj', c1'') = bougeProjectile proj c1' zone in
                case proj' of 
                    Nothing -> j{joueur1 = c1''}
                    Just proj'' -> j{joueur1 = c1'', projectiles = projectiles' Seq.|> proj''}
bougeProjectilesJeu jeu = jeu


technique :: Combattant -> Combattant -> (Combattant, Combattant)
technique c1@(Comb (Composite hbs) _ _ 0 dammage CoupDePoing _ _) c2 = 
    case Seq.lookup 1 hbs of
        Nothing -> error "Le joueur n'a pas de bras"
        Just h -> case collision h (hitboxc c2) of
            False -> (c1{techniquec = CoupDePoing, hitTimer = 15}, c2)
            True -> if (techniquec c2 == Protection) 
                    then (c1{techniquec = CoupDePoing, hitTimer = 15}, c2)
                    else (c1{hitTimer = 5, techniquec = CoupDePoing}, (degats c2 dammage))
technique c1@(Comb (Composite hbs) _ _ 0 dammage CoupDePied _ _) c2 = 
    case Seq.lookup 1 hbs of
        Nothing -> error "Le joueur n'a pas de jambe"
        Just h -> case collision h (hitboxc c2) of
            False -> (c1{techniquec = CoupDePied, hitTimer = 15}, c2)
            True -> if (techniquec c2 == Protection) 
                    then (c1{techniquec = CoupDePied, hitTimer = 15}, c2)
                    else (c1{hitTimer = 15, techniquec = CoupDePied}, (degats c2 dammage))

technique c1@(Comb _ _ _ ht _ _ hs _) c2 = (c1{hitTimer = max (ht - 1) 0, hitStun = max (hs - 1) 0}, c2)

techniqueJeu :: Integer -> Technique -> Jeu -> Jeu -- Le joueur ne peut pas déclencher cette technique s'il sort des limites
techniqueJeu 1 CoupDePied (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c1) - 62 <= 0 && (facec c1) == G) || ((positionJoueurX c1) + 63 + 62 >= largeurZone && (facec c1) == D)   
                                                then (EnCours c1 c2 zone projectiles) 
                                                else
                                                    let (c1', c2') = (technique (changerposture 1 CoupDePied c1) c2) in 
                                                        if not (en_vie c2') 
                                                        then GameOver 1
                                                        else (EnCours c1' c2' zone projectiles) 

techniqueJeu 1 CoupDePoing (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c1) - 36) <= 0 && (facec c1) == G || (positionJoueurX c1) + 63 + 36 >= largeurZone && (facec c1) == D
                                                  then (EnCours c1 c2 zone projectiles) 
                                                  else
                                                      let (c1', c2') = (technique (changerposture 1 CoupDePoing c1) c2) in 
                                                      if not (en_vie c2') 
                                                      then GameOver 1
                                                      else (EnCours c1' c2' zone projectiles) 

techniqueJeu 1 Accroupi (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c1) + 80 >= largeurZone)   
                                                then (EnCours c1 c2 zone projectiles) 
                                                else
                                                    let (c1', c2') = (technique (changerposture 1 Accroupi c1) c2) in 
                                                        if not (en_vie c2') 
                                                        then GameOver 1
                                                        else (EnCours c1' c2' zone projectiles) 

techniqueJeu 2 Accroupi (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c2) + 95 >= largeurZone)   
                                                then (EnCours c1 c2 zone projectiles) 
                                                else
                                                    let (c2', c1') = (technique (changerposture 2 Accroupi c2) c1) in 
                                                      if not (en_vie c1') 
                                                      then GameOver 2
                                                      else (EnCours c1' c2' zone projectiles) 

techniqueJeu 2 CoupDePied (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c2) - 72 <= 0 && (facec c2) == G) || ((positionJoueurX c2) + 63 + 72 >= largeurZone && (facec c2) == D)   
                                                then (EnCours c1 c2 zone projectiles) 
                                                else
                                                    let (c2', c1') = (technique (changerposture 2 CoupDePied c2) c1) in 
                                                      if not (en_vie c1') 
                                                      then GameOver 2
                                                      else (EnCours c1' c2' zone projectiles) 

techniqueJeu 2 CoupDePoing (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c2) - 54) <= 0 && (facec c2) == G || ((positionJoueurX c2) + 63 + 54 >= largeurZone && (facec c2) == D) 
                                                  then (EnCours c1 c2 zone projectiles)  
                                                  else
                                                      let (c2', c1') = (technique (changerposture 2 CoupDePoing c2) c1) in 
                                                      if not (en_vie c1') 
                                                      then GameOver 2
                                                      else (EnCours c1' c2' zone projectiles) 

techniqueJeu 2 Protection (EnCours c1 c2 zone projectiles) = if ((positionJoueurX c2) + 76 >= largeurZone) 
                                                  then (EnCours c1 c2 zone projectiles)  
                                                  else
                                                      let (c2', c1') = (technique (changerposture 2 Protection c2) c1) in 
                                                      if not (en_vie c1') 
                                                      then GameOver 2
                                                      else (EnCours c1' c2' zone projectiles) 

                                                   

techniqueJeu 1 t (EnCours c1 c2 zone projectiles) = let (c1', c2') = (technique (changerposture 1 t c1) c2) in 
    if not (en_vie c2') 
    then GameOver 1
    else (EnCours c1' c2' zone projectiles) 
techniqueJeu 2 t (EnCours c1 c2 zone projectiles) = let (c2', c1') = (technique (changerposture 2 t c2) c1) in
    if not (en_vie c1')
    then GameOver 2
    else (EnCours c1' c2' zone projectiles) 
techniqueJeu _ _ jeu = jeu

projectileDejaLance :: Integer ->  [Projectile] -> Bool
projectileDejaLance num [] = False
projectileDejaLance num (x:xs) = proprietaire x == num || (projectileDejaLance num xs)

ajouteProjectile :: Integer -> Jeu -> Jeu
ajouteProjectile 1 jeu@(EnCours c1 c2 zone proj) =
    if (positionJoueurX c1 + 119 >= largeurZone) || (projectileDejaLance 1 (toList proj))
    then jeu
    else 
    jeu{joueur1 = (changerposture 1 Lancer c1){hitTimer = 35}, projectiles = proj Seq.|>(Proj 1 (initHitboxProj 1 c1)) 1 (Mouv (facec c1) 10)}
ajouteProjectile 2 jeu@(EnCours c1 c2 zone proj) =
    if (positionJoueurX c2 + 127 >= largeurZone) || (projectileDejaLance 2 (toList proj))
    then jeu
    else 
    jeu{joueur2 = (changerposture 2 Lancer c2){hitTimer = 35}, projectiles = proj Seq.|>(Proj 2 (initHitboxProj 2 c2)) 1 (Mouv (facec c2) 10)}

changerposture :: Integer -> Technique -> Combattant -> Combattant
changerposture 1 t c@(Comb _ _ _ 0 _ _ 0 cbo) = let nouv_cbo = if (t /= Encaisse) then 0 else cbo in
     (initJoueur1 (positionJoueurX c) (positionJoueurY c) 64 119 t (pointsDeVie c) (facec c) 0 0 nouv_cbo) 
changerposture 2 t c@(Comb _ _ _ 0 _ _ 0 cbo) = let nouv_cbo = if (t /= Encaisse) then 0 else cbo in
    (initJoueur2 (positionJoueurX c) (positionJoueurY c) 63 126 t (pointsDeVie c) (facec c) 0 0 nouv_cbo) 
changerposture _ _ c = c

degats :: Combattant -> Integer -> Combattant
degats c@(Comb _ _ (Ok pointsDeVie) _ _ _ _ cbo) d = 
    let pointsDeVie' = (pointsDeVie - d) in
        if pointsDeVie' < 0 
        then c{etatx = Ko, techniquec = Encaisse}
        else c{etatx = (Ok pointsDeVie'), techniquec = Encaisse, hitStun = 10, combo = (cbo + 1)} 
degats c _ = c{techniquec = Encaisse}


deplacementJoueur :: Integer 
deplacementJoueur = 5      

-- fonctions pour l'IA

estAuSol :: Combattant -> Integer -> Bool
estAuSol c taille = (positionJoueurY c) >= hauteurZone - taille

estAuDessusOuDessous :: Combattant -> Combattant -> Bool
estAuDessusOuDessous c1 c2 = 
    ((estAuSol c1 119) /= (estAuSol c2 126)) && abs ((positionJoueurX c1) - (positionJoueurX c2)) < 130

toucheBordure :: Combattant -> Bool
toucheBordure c = 
    if (positionJoueurX c) <= 10 || (positionJoueurX c) + 63 >= (largeurZone - 10) then True else False

oppose :: Direction -> Direction
oppose G = D
oppose D = G
oppose _ = error "Impossible"

prochaineDirection :: Combattant -> Combattant -> Direction
prochaineDirection c1 c2 =
    if (estAuDessusOuDessous c1 c2) 
        then if (toucheBordure c2) then (oppose (facec c2)) else (facec c2)
        else 
            if (positionJoueurX c1 > (positionJoueurX c2) + 63)
            then D
            else G 

aPortee :: Combattant -> Combattant -> Bool
aPortee c1 c2 = abs ((positionJoueurX c1) - (positionJoueurX c2)) <= 117

gameStep :: Jeu -> Keyboard -> Jeu -- Mode PvP
gameStep jeu@(EnCours c1 c2 _ projectiles) kbd = 
    let modification =  
                        (if K.keypressed SDL.KeycodeS kbd && (hitTimer c1 == 0) && (hitStun c1 == 0) && (estAuSol c1 119) && (not (estAuDessusOuDessous c1 c2))
                        then techniqueJeu 1 Accroupi
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeDown kbd && (hitTimer c2 == 0) && (hitStun c2 == 0) && (estAuSol c2 126) && (not (estAuDessusOuDessous c1 c2)) 
                        then techniqueJeu 2 Accroupi
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeD kbd
                        then bougeJoueurJeu 1 (Mouv D deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeQ kbd 
                        then bougeJoueurJeu 1 (Mouv G deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeZ kbd && positionJoueurY c1 == hauteurZone - 119
                        then bougeJoueurJeu 1 (Mouv H 250)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeA kbd && (hitTimer c1 == 0) && (hitStun c1 == 0) && (combo c2 < 5)
                        then techniqueJeu 1 CoupDePoing
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeE kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)
                        then techniqueJeu 1 CoupDePied
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeF kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)
                        then ajouteProjectile 1
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeRight kbd
                        then bougeJoueurJeu 2 (Mouv D deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeLeft kbd 
                        then bougeJoueurJeu 2 (Mouv G deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeUp kbd && (positionJoueurY c2 == hauteurZone - 126)
                        then bougeJoueurJeu 2 (Mouv H 250)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeReturn kbd && (hitTimer c2 == 0) && (hitStun c2 == 0) && (combo c1 <= 5)
                        then techniqueJeu 2 CoupDePoing
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeRShift kbd && (hitTimer c2 == 0) && (hitStun c2 == 0)
                        then techniqueJeu 2 CoupDePied
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeP kbd && (hitTimer c2 == 0) && (hitStun c2 == 0)
                        then ajouteProjectile 2
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeR kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)
                        then techniqueJeu 1 Protection
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeM kbd && (hitTimer c2 == 0) && (hitStun c2 == 0)
                        then techniqueJeu 2 Protection
                        else id)
                        -- Les attaques ne durent qu'un tour
                        .
                        techniqueJeu 1 Rien
                        .
                        techniqueJeu 2 Rien
                        .
                        -- Bouger les projectiles 
                        bougeProjectilesJeu
                        .
                        -- Faire redescendre les joueurs en cas de saut
                        bougeJoueurJeu 1 (Mouv B 10)
                        .
                        bougeJoueurJeu 2 (Mouv B 10)
                        
    
    in modification jeu
gameStep jeu _ = jeu

gameStepRandom :: Float -> Jeu -> Keyboard -> Jeu -- Mode PvE
gameStepRandom _ (GameOver num) _ = (GameOver num)
gameStepRandom rand jeu@(EnCours c1 c2 _ projectiles) kbd = 
    let modification =  
                        (if K.keypressed SDL.KeycodeD kbd
                        then bougeJoueurJeu 1 (Mouv D deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeQ kbd 
                        then bougeJoueurJeu 1 (Mouv G deplacementJoueur)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeZ kbd && positionJoueurY c1 == hauteurZone - 119
                        then bougeJoueurJeu 1 (Mouv H 250)
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeA kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)  && (combo c2 < 5)
                        then techniqueJeu 1 CoupDePoing
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeE kbd && (hitTimer c1 == 0) && (hitStun c1 == 0) 
                        then techniqueJeu 1 CoupDePied
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeF kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)
                        then ajouteProjectile 1
                        else id)
                        .
                        (let direction = prochaineDirection c1 c2 in
                        if rand < 0.5 || (not (estAuDessusOuDessous c1 c2))
                        then bougeJoueurJeu 2 (Mouv direction deplacementJoueur)
                        else id)
                        .
                        (if rand > 0.5 && rand < 0.55 && (positionJoueurY c2 == hauteurZone - 126) && (not (estAuDessusOuDessous c1 c2))
                        then bougeJoueurJeu 2 (Mouv H 250)
                        else id)
                        .
                        (if (aPortee c1 c2) && rand < 0.75 && rand > 0.55 && (hitTimer c2 == 0) && (hitStun c2 == 0) && (not (estAuDessusOuDessous c1 c2)) && (combo c2 < 5)
                        then techniqueJeu 2 CoupDePoing
                        else id)
                        .
                        (if (aPortee c1 c2) && rand < 0.95 && rand > 0.75 && (hitTimer c2 == 0) && (hitStun c2 == 0) && (not (estAuDessusOuDessous c1 c2))
                        then techniqueJeu 2 CoupDePied
                        else id)
                        .
                        (if rand > 0.55 && rand < 0.95 && (hitTimer c2 == 0) && (hitStun c2 == 0) && not (aPortee c1 c2) && (not (estAuDessusOuDessous c1 c2))
                        then ajouteProjectile 2
                        else id)
                        .
                        (if K.keypressed SDL.KeycodeR kbd && (hitTimer c1 == 0) && (hitStun c1 == 0)
                        then techniqueJeu 1 Protection
                        else id)
                        .
                         (if K.keypressed SDL.KeycodeS kbd && (hitTimer c1 == 0) && (hitStun c1 == 0) && (estAuSol c1 119) && (not (estAuDessusOuDessous c1 c2)) 
                        then techniqueJeu 1 Accroupi
                        else id)
                        .
                        (if (aPortee c1 c2) && rand > 0.95 && (hitTimer c2 == 0) && (hitStun c2 == 0) && (not (estAuDessusOuDessous c1 c2))
                        then techniqueJeu 2 Protection
                        else id)
                        -- Les attaques ne durent qu'un tour
                        .
                        techniqueJeu 1 Rien
                        .
                        techniqueJeu 2 Rien
                        .
                        -- Bouger les projectiles 
                        bougeProjectilesJeu
                        -- Faire redescendre les joueurs en cas de saut
                        .
                        bougeJoueurJeu 1 (Mouv B 10)
                        .
                        bougeJoueurJeu 2 (Mouv B 10)
                        
    
    in modification jeu


-- Exemples
hitbox1 = initSimpleHitbox (Coord 10 10) 5 5 zone
hitbox2 = initHitbox (Seq.fromList [ ((Coord 80 90), 8, 12), ((Coord 40 40), 10, 10)]) zone
hitbox3 = initHitbox (Seq.fromList [((Coord 35 24), 8, 12), ((Coord 24 13), 10, 10)]) zone
zone = initZone 640 480

joueur1Poing = initJoueur1 150 (hauteurZone - 119) 64 119 CoupDePoing 100 D 60
joueur2Normal = initJoueur2 251 (hauteurZone - 126) 63 126 Rien 100 G 0
joueur2Poing = initJoueur2 251 (hauteurZone - 126) 63 126 CoupDePoing 100 G 60
