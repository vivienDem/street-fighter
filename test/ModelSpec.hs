module ModelSpec where

import Test.QuickCheck
import System.Random (Random)
import Control.Applicative (liftA2)
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq
import Test.Hspec
import Model
import Keyboard as K
import SDL as SDL
import Control.Monad

genTechnique :: Gen Technique 
genTechnique = frequency [ 
                           (40, pure Rien), 
                           (10, pure CoupDePied),
                           (10, pure CoupDePoing),
                           (10, pure Protection),
                           (10, pure Encaisse),
                           (10, pure Accroupi),
                           (10, pure Lancer)
                         ]

genDirection :: Gen Direction 
genDirection = frequency [ 
                            (50, pure D),
                            (50, pure G)
                         ]

genCombattant1 :: Gen Combattant
genCombattant1 = do
    technique <- genTechnique
    direction <- genDirection
    hitTimer <- choose (0, 60)
    hitStun <- choose (0, 60)
    combo <- choose(0, 5)
    let l = 64 
    let h = 119
    pv <- choose (1, 100)
    x <- choose (54, (largeurZone - 127))
    y <- choose (0, (hauteurZone - 121 - 250))
    return (initJoueur1 x y l h technique pv direction hitTimer hitStun combo)

genCombattant2 :: Gen Combattant
genCombattant2 = do
    technique <- genTechnique
    direction <- genDirection
    hitTimer <- choose (0, 60)
    hitStun <- choose (0, 60)
    combo <- choose (0, 5)
    let l = 63 
    let h = 129
    pv <- choose (1, 100)
    x <- choose (72, (largeurZone - 135))
    y <- choose (0, (hauteurZone - 130 - 250))
    return (initJoueur2 x y l h technique pv direction hitTimer hitStun combo)

genJeuEnCours :: Gen Jeu 
genJeuEnCours = do 
    c1 <- genCombattant1
    c2 <- genCombattant2 
    return (EnCours c1 c2 zone Seq.Empty)

genJeu :: Gen Jeu 
genJeu = do 
    frequency [
                (10, GameOver <$> (choose (1, 2))),
                (90, genJeuEnCours)
              ]

genKeycode :: Gen SDL.Keycode
genKeycode = do 
    frequency [
                (50, pure KeycodeQ),
                (50, pure KeycodeD),
                (50, pure KeycodeLeft),
                (50, pure KeycodeRight),
                (5, pure KeycodeZ),
                (5, pure KeycodeUp),
                (5, pure KeycodeS),
                (5, pure KeycodeDown),
                (33, pure KeycodeA),
                (33, pure KeycodeReturn),
                (33, pure KeycodeE),
                (33, pure KeycodeRShift),
                (33, pure KeycodeF),
                (33, pure KeycodeP)
              ]
    
instance Arbitrary Zone where 
    arbitrary = pure zone 

instance Arbitrary Combattant where 
    arbitrary = do 
        frequency [
                    (50, genCombattant1),
                    (50, genCombattant2)
                  ]

instance Arbitrary Jeu where
    arbitrary = genJeu

instance Arbitrary SDL.Keycode where
    arbitrary = genKeycode

coordSpec :: SpecWith ()
coordSpec = do 
    describe "bougeCoord" $ do 
        let coordonnees = Coord 50 50 
            distance = 10
        it "revient aux coordonnées de départ en se déplaçant de la même distance à gauche puis à droite" $ do 
            bougeCoord (bougeCoord coordonnees (Mouv G distance)) (Mouv D distance) `shouldBe` coordonnees
        it "revient aux coordonnées de départ en se déplaçant de la même distance à droite puis à gauche" $ do 
            bougeCoord (bougeCoord coordonnees (Mouv D distance)) (Mouv G distance) `shouldBe` coordonnees
        it "revient aux coordonnées de départ en se déplaçant de la même distance en haut puis en bas" $ do 
            bougeCoord (bougeCoord coordonnees (Mouv H distance)) (Mouv B distance) `shouldBe` coordonnees
        it "revient aux coordonnées de départ en se déplaçant de la même distance en bas puis en haut" $ do 
            bougeCoord (bougeCoord coordonnees (Mouv B distance)) (Mouv H distance) `shouldBe` coordonnees
    describe "bougeCoordSafe" $ do 
        let coordonnees = Coord 50 50 
        it "déplace les coordonnées lorsque le mouvement ne fait pas sortir le point de la zone de jeu" $ do
            bougeCoordSafe coordonnees (Mouv G 10) zone `shouldBe` Just (Coord 40 50)
        it "renvoie Nothing si le mouvement fait sortir le point de la zone de jeu" $ do 
            bougeCoordSafe coordonnees (Mouv G 51) zone `shouldBe` Nothing

zoneSpec :: SpecWith ()
zoneSpec = do 
    describe "Zone" $ do
        it "vérifie son invariant" $ do
            prop_Zone_inv zone `shouldBe` True

combattantSpec :: SpecWith ()
combattantSpec = do
    describe "initJoueur1 et initJoueur2" $ do
        it "préservent l'invariant de combattant" $ do
            quickCheck prop_Combattant_inv 
    describe "actualiserFace" $ do 
        let distance = 10
        it "renvoie D ou G si le joueur 1 se déplace vers le haut" $ do 
            quickCheck (prop_post_ActualiserFace 1 (Mouv H distance)) 
        it "renvoie D ou G si le joueur 1 se déplace vers le bas" $ do 
            quickCheck (prop_post_ActualiserFace 1 (Mouv B distance)) 
        it "renvoie D ou G si le joueur 1 se déplace vers la gauche" $ do 
            quickCheck (prop_post_ActualiserFace 1 (Mouv G distance)) 
        it "renvoie D ou G si le joueur 1 se déplace vers la droite" $ do 
            quickCheck (prop_post_ActualiserFace 1 (Mouv D distance))        
        it "renvoie D ou G si le joueur 2 se déplace vers le haut" $ do 
            quickCheck (prop_post_ActualiserFace 2 (Mouv H distance)) 
        it "renvoie D ou G si le joueur 2 se déplace vers le bas" $ do 
            quickCheck (prop_post_ActualiserFace 2 (Mouv B distance)) 
        it "renvoie D ou G si le joueur 2 se déplace vers la gauche" $ do 
            quickCheck (prop_post_ActualiserFace 2 (Mouv G distance)) 
        it "renvoie D ou G si le joueur 2 se déplace vers la droite" $ do 
            quickCheck (prop_post_ActualiserFace 2 (Mouv D distance)) 
    describe "pointsDeVie" $ do 
        it "renvoie toujours une valeur entre 0 et 100" $ do 
            quickCheck prop_post_PointsDeVie

jeuSpec :: SpecWith ()
jeuSpec = do 
    describe "Jeu" $ do
        it "vérifie son invariant" $ do
            quickCheck prop_Jeu_inv
    describe "gameStep" $ do 
        it "préserve l'invariant" $ do 
            quickCheck prop_post_gameStep
    describe "gameStepRandom" $ do 
        it "préserve l'invariant" $ do 
            quickCheck prop_post_gameStepRandom        
            
modelSpec :: SpecWith ()
modelSpec = do 
    coordSpec
    zoneSpec
    jeuSpec
    combattantSpec