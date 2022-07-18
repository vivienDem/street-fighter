-- Adan Bougherara et Vivien Demeulenaere
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (Jeu)
import qualified Model as M
import GHC.Base (when, IO())

import System.Random

assetId :: Integer -> M.Technique -> M.Direction -> String
assetId 1 M.Rien M.D = "joueur1"
assetId 2 M.Rien M.G = "joueur2"
assetId 1 M.CoupDePoing M.D = "joueur1Poing"
assetId 2 M.CoupDePoing M.G = "joueur2Poing"
assetId 1 M.CoupDePied M.D = "joueur1Pied"
assetId 2 M.CoupDePied M.G = "joueur2Pied"
assetId 1 M.Protection M.D = "joueur1Protection"
assetId 2 M.Protection M.G = "joueur2Protection"
assetId 1 M.Encaisse M.D = "joueur1Encaisse"
assetId 2 M.Encaisse M.G = "joueur2Encaisse"
assetId 1 M.Accroupi M.D = "joueur1Accroupi"
assetId 2 M.Accroupi M.G = "joueur2Accroupi"
assetId 1 M.Lancer M.D = "joueur1Lancer"
assetId 2 M.Lancer M.G = "joueur2Lancer"
assetId 1 M.Rien _ = "joueur1Inv"
assetId 2 M.Rien _ = "joueur2Inv"
assetId 1 M.CoupDePoing _ = "joueur1PoingInv"
assetId 2 M.CoupDePoing _ = "joueur2PoingInv"
assetId 1 M.CoupDePied M.G = "joueur1PiedInv"
assetId 2 M.CoupDePied M.D = "joueur2PiedInv"
assetId 1 M.Protection M.G = "joueur1ProtectionInv"
assetId 2 M.Protection M.D = "joueur2ProtectionInv"
assetId 1 M.Encaisse M.G = "joueur1EncaisseInv"
assetId 2 M.Encaisse M.D = "joueur2EncaisseInv"
assetId 1 M.Accroupi M.G = "joueur1AccroupiInv"
assetId 2 M.Accroupi M.D = "joueur2AccroupiInv"
assetId 1 M.Lancer M.G = "joueur1LancerInv"
assetId 2 M.Lancer M.D = "joueur2LancerInv"

assetIdProj :: Integer -> M.Mouvement -> String
assetIdProj 1 (M.Mouv M.D _) = "proj1D"
assetIdProj 1 (M.Mouv M.G _) = "proj1G"
assetIdProj 2 (M.Mouv M.D _)= "proj2D"
assetIdProj 2 (M.Mouv M.G _) = "proj2G"

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadJoueur1 :: String -> Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadJoueur1 "joueur1" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1") (S.mkArea 0 0 64 119)
  let smap' = SM.addSprite (SpriteId "joueur1") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Poing" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Poing") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Poing") (S.mkArea 0 0 98 121)
  let smap' = SM.addSprite (SpriteId "joueur1Poing") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Pied" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Pied") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Pied") (S.mkArea 0 0 127 118)
  let smap' = SM.addSprite (SpriteId "joueur1Pied") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Protection" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Protection") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Protection") (S.mkArea 0 0 63 119)
  let smap' = SM.addSprite (SpriteId "joueur1Protection") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Encaisse" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Encaisse") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Encaisse") (S.mkArea 0 0 63 95)
  let smap' = SM.addSprite (SpriteId "joueur1Encaisse") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Accroupi" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Accroupi") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Accroupi") (S.mkArea 0 0 80 68)
  let smap' = SM.addSprite (SpriteId "joueur1Accroupi") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Lancer" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Lancer") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Lancer") (S.mkArea 0 0 119 119)
  let smap' = SM.addSprite (SpriteId "joueur1Lancer") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1Inv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1Inv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1Inv") (S.mkArea 0 0 64 119)
  let smap' = SM.addSprite (SpriteId "joueur1Inv") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1PoingInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1PoingInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1PoingInv") (S.mkArea 0 0 98 121)
  let smap' = SM.addSprite (SpriteId "joueur1PoingInv") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1PiedInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1PiedInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1PiedInv") (S.mkArea 0 0 255 119)
  let smap' = SM.addSprite (SpriteId "joueur1PiedInv") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1ProtectionInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1ProtectionInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1ProtectionInv") (S.mkArea 0 0 63 119)
  let smap' = SM.addSprite (SpriteId "joueur1ProtectionInv") sprite smap
  return (tmap', smap')
loadJoueur1 "joueur1EncaisseInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1EncaisseInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1EncaisseInv") (S.mkArea 0 0 63 95)
  let smap' = SM.addSprite (SpriteId "joueur1EncaisseInv") sprite smap
  return (tmap', smap') 
loadJoueur1 "joueur1AccroupiInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1AccroupiInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1AccroupiInv") (S.mkArea 0 0 80 68)
  let smap' = SM.addSprite (SpriteId "joueur1AccroupiInv") sprite smap
  return (tmap', smap') 
loadJoueur1 "joueur1LancerInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur1LancerInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur1LancerInv") (S.mkArea 0 0 119 119)
  let smap' = SM.addSprite (SpriteId "joueur1LancerInv") sprite smap
  return (tmap', smap')

loadJoueur2 :: String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadJoueur2 "joueur2" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2") (S.mkArea 0 0 63 126)
  let smap' = SM.addSprite (SpriteId "joueur2") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Poing" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Poing") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Poing") (S.mkArea 0 0 117 130)
  let smap' = SM.addSprite (SpriteId "joueur2Poing") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Pied" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Pied") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Pied") (S.mkArea 0 0 117 130)
  let smap' = SM.addSprite (SpriteId "joueur2Pied") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Protection" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Protection") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Protection") (S.mkArea 0 0 76 126)
  let smap' = SM.addSprite (SpriteId "joueur2Protection") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Encaisse" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Encaisse") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Encaisse") (S.mkArea 0 0 76 113)
  let smap' = SM.addSprite (SpriteId "joueur2Encaisse") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Accroupi" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Accroupi") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Accroupi") (S.mkArea 0 0 95 84)
  let smap' = SM.addSprite (SpriteId "joueur2Accroupi") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Lancer" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Lancer") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Lancer") (S.mkArea 0 0 127 126)
  let smap' = SM.addSprite (SpriteId "joueur2Lancer") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2Inv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2Inv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2Inv") (S.mkArea 0 0 63 126)
  let smap' = SM.addSprite (SpriteId "joueur2Inv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2PoingInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2PoingInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2PoingInv") (S.mkArea 0 0 98 121)
  let smap' = SM.addSprite (SpriteId "joueur2PoingInv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2PiedInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2PiedInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2PiedInv") (S.mkArea 0 0 135 132)
  let smap' = SM.addSprite (SpriteId "joueur2PiedInv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2ProtectionInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2ProtectionInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2ProtectionInv") (S.mkArea 0 0 63 126)
  let smap' = SM.addSprite (SpriteId "joueur2ProtectionInv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2EncaisseInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2EncaisseInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2EncaisseInv") (S.mkArea 0 0 76 113)
  let smap' = SM.addSprite (SpriteId "joueur2EncaisseInv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2AccroupiInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2AccroupiInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2AccroupiInv") (S.mkArea 0 0 95 84)
  let smap' = SM.addSprite (SpriteId "joueur2AccroupiInv") sprite smap
  return (tmap', smap')
loadJoueur2 "joueur2LancerInv" rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "joueur2LancerInv") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "joueur2LancerInv") (S.mkArea 0 0 127 126)
  let smap' = SM.addSprite (SpriteId "joueur2LancerInv") sprite smap
  return (tmap', smap')

loadProjectile :: String -> Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadProjectile asset rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId asset) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId asset) (S.mkArea 0 0 52 33)
  let smap' = SM.addSprite (SpriteId asset) sprite smap
  return (tmap', smap')


choixMenu :: IO String
choixMenu  = do
  putStrLn $ "Menu :\n1 - Mode PvP \n2 - Mode PvE \n(Mode PvP par défaut)"
  str <- getLine
  return str

main :: IO ()
main = do
  initializeAll
  choix <- choixMenu
  window <- createWindow "Street Fighter" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement des joueurs
  (tmap', smap') <- loadJoueur1 "joueur1" renderer "assets/perso 1/images choisies/gauche/posture_baseG.bmp" tmap smap
  (tmap', smap') <- loadJoueur2 "joueur2" renderer "assets/perso 2/images choisies/droite/posture_baseD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Poing" renderer "assets/perso 1/images choisies/gauche/attaque_poingG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Poing" renderer "assets/perso 2/images choisies/droite/attaque_poingD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Pied" renderer "assets/perso 1/images choisies/gauche/attaque_jambeG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Pied" renderer "assets/perso 2/images choisies/droite/attaque_jambeD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Protection" renderer "assets/perso 1/images choisies/gauche/se_protegeG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Protection" renderer "assets/perso 2/images choisies/droite/se_protegeD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Encaisse" renderer "assets/perso 1/images choisies/gauche/encaisseG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Encaisse" renderer "assets/perso 2/images choisies/droite/encaisseD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Accroupi" renderer "assets/perso 1/images choisies/gauche/accroupiG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Accroupi" renderer "assets/perso 2/images choisies/droite/accroupiD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Lancer" renderer "assets/perso 1/images choisies/gauche/lanceProjectileG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Lancer" renderer "assets/perso 2/images choisies/droite/lanceProjectileD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1Inv" renderer "assets/perso 1/images choisies/droite/posture_baseD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2Inv" renderer "assets/perso 2/images choisies/gauche/posture_baseG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1PoingInv" renderer "assets/perso 1/images choisies/droite/attaque_poingD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2PoingInv" renderer "assets/perso 2/images choisies/gauche/attaque_poingG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1PiedInv" renderer "assets/perso 1/images choisies/droite/attaque_jambeD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2PiedInv" renderer "assets/perso 2/images choisies/gauche/attaque_jambeG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1ProtectionInv" renderer "assets/perso 1/images choisies/droite/se_protegeD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2ProtectionInv" renderer "assets/perso 2/images choisies/gauche/se_protegeG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1EncaisseInv" renderer "assets/perso 1/images choisies/droite/encaisseD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2EncaisseInv" renderer "assets/perso 2/images choisies/gauche/encaisseG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1AccroupiInv" renderer "assets/perso 1/images choisies/droite/accroupiD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2AccroupiInv" renderer "assets/perso 2/images choisies/gauche/accroupiG.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur1 "joueur1LancerInv" renderer "assets/perso 1/images choisies/droite/lanceProjectileD.bmp" tmap' smap'
  (tmap', smap') <- loadJoueur2 "joueur2LancerInv" renderer "assets/perso 2/images choisies/gauche/lanceProjectileG.bmp" tmap' smap'
  (tmap', smap') <- loadProjectile "proj1D" renderer "assets/perso 1/images choisies/gauche/fireG.bmp" tmap' smap'
  (tmap', smap') <- loadProjectile "proj2G" renderer "assets/perso 2/images choisies/droite/fireD.bmp" tmap' smap'
  (tmap', smap') <- loadProjectile "proj1G" renderer "assets/perso 1/images choisies/droite/fireD.bmp" tmap' smap'
  (tmap', smap') <- loadProjectile "proj2D" renderer "assets/perso 2/images choisies/gauche/fireG.bmp" tmap' smap'
  -- initialisation de l'état du jeu
  let jeu = M.initJeu
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd jeu (choix == "2")

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Jeu -> Bool -> IO ()
gameLoop frameRate renderer tmap smap kbd jeu modePvE = do
  rand <- randomIO :: IO Float
  startTime <- time
  events <- pollEvents
  let (kbd', mouse) = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  
  --- display perso 
  let x1 = if (M.techniquec (M.joueur1 jeu) == M.CoupDePied) && M.facec (M.joueur1 jeu) == M.G
           then (fromIntegral (M.positionJoueurX (M.joueur1 jeu)) - 64)  
           else 
             if (M.techniquec (M.joueur1 jeu) == M.CoupDePoing) && M.facec (M.joueur1 jeu) == M.G
             then (fromIntegral (M.positionJoueurX (M.joueur1 jeu)) - 36)  
             else (fromIntegral (M.positionJoueurX (M.joueur1 jeu)))
  
  let x2 = if (M.techniquec (M.joueur2 jeu) == M.CoupDePied) && M.facec (M.joueur2 jeu) == M.G
           then (fromIntegral (M.positionJoueurX (M.joueur2 jeu)) - 72)  
           else 
             if (M.techniquec (M.joueur2 jeu) == M.CoupDePoing) && M.facec (M.joueur2 jeu) == M.G
             then (fromIntegral (M.positionJoueurX (M.joueur2 jeu)) - 55)  
             else (fromIntegral (M.positionJoueurX (M.joueur2 jeu)))

  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (assetId 1 (M.techniquec (M.joueur1 jeu)) (M.facec (M.joueur1 jeu)))) smap)
                                x1
                                 (fromIntegral (M.positionJoueurY (M.joueur1 jeu)))) 
  
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (assetId 2 (M.techniquec (M.joueur2 jeu)) (M.facec (M.joueur2 jeu)))) smap)
                                 x2
                                 (fromIntegral (M.positionJoueurY (M.joueur2 jeu)))) 

  foldr (\proj acc -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (assetIdProj (M.proprietaire proj) (M.mouvp proj))) smap)
                                (fromIntegral (M.positionProjectileX proj))
                                (fromIntegral (M.positionProjectileY proj)))) (return ()) (M.projectiles jeu)         
  ---
  SDL.rendererDrawColor renderer $= (V4 255 0 0 0)
  TM.loadHealthState renderer 1
  TM.loadHealthState renderer 2
  TM.fillHealthState renderer 1 (fromIntegral (M.pointsDeVie (M.joueur1 jeu)))
  TM.fillHealthState renderer 2 (fromIntegral (M.pointsDeVie (M.joueur2 jeu)))

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  
  --- update du game state
  
  let jeu' = if modePvE then M.gameStepRandom rand jeu kbd' else M.gameStep jeu kbd'

  let msg = M.messageDeFin jeu'
  
  if not (K.keypressed KeycodeEscape kbd' || (M.messageDeFin jeu') /= "Egalité") then 
    (do
        gameLoop frameRate renderer tmap smap kbd' jeu' modePvE
    )
  else 
    putStrLn $ msg
  
    