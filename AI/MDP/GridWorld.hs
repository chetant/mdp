module AI.MDP.GridWorld
(
 GridWorld
,GridVal
,GridAction(..)

-- Debug/Visualization Funcs
,showRewards
,showAbsorbs
,showTransition

-- common action results
,deterministicActions
,maybeActions
,maybeReverseActions
,scatterActions

,BlockedCells
,MoveCost

,gridWorld
,reward
,absorb
,initVals
,iterVals
) where

import Text.Printf
import qualified Data.Vector as V
import Data.Vector(Vector, (//), (!))

import AI.MDP

data GridWorld = GridWorld { g_width :: !Int
                           , g_height :: !Int
                           , g_blocked :: Vector Bool
                           , g_mdp :: MDP
                           } deriving (Show, Eq)

data GridVal = GridVal { gv_gw :: GridWorld 
                       , gv_vals :: !Values
                       }

renderGrid f g gv@(GridVal gw vs) = vbar ++ concatMap mkRow [0..(height-1)]
    where mkRow i = showRow (V.slice (width*i) width (V.zip bs (f gv))) ++ vbar
          showRow = (++ "\n") . foldl (\s x -> s ++ buildVal x ++ "|") "|" . V.toList
          buildVal (False, v) = take valCellWidth $ g v ++ repeat ' '
          buildVal (True, _) = take valCellWidth $ repeat '#'
          valCellWidth = 5
          vbar = replicate (1 + (valCellWidth+1) * width) '=' ++ "\n"
          width = g_width gw
          height = g_height gw
          bs = g_blocked gw

showRewards gv = putStr $ renderGrid (rewards . g_mdp . gv_gw) (printf "%3.1f") gv
showAbsorbs gv = putStr $ renderGrid (absState . g_mdp . gv_gw) (\x -> case x of { True -> "X"; False -> " "; }) gv
showTransition (x, y) a gv = putStr $ renderGrid getTrans (printf "%3.1f") gv
    where getTrans = ((! i) . (! actionI a) . transitions . g_mdp . gv_gw)
          gw = gv_gw gv
          w = g_width gw
          h = g_height gw
          i = y*w + x

instance Show GridVal where
    show = renderGrid gv_vals (printf "%3.1f")

data GridAction = MoveW | MoveE | MoveN | MoveS | MoveNE | MoveNW | MoveSE | MoveSW | MoveNone deriving (Show, Eq, Ord)

actionI :: GridAction -> Int
actionI MoveW = 0
actionI MoveE = 1
actionI MoveN = 2
actionI MoveS = 3
actionI MoveNE = 4
actionI MoveNW = 5
actionI MoveSE = 6
actionI MoveSW = 7
actionI MoveNone = 8

-- ^ Gives the allowed actions and the results of executing it along with their probabilities
type ActionResult = [(GridAction, Probability)]
type ActionResults = [ActionResult] -- ^ implicit understanding: action results are in order

checkActionResult :: ActionResults -> Bool
checkActionResult = all (\n -> abs (n-1) < 1E10) . map (sum . map snd)

deterministicActions :: ActionResults
deterministicActions = [[(MoveW, 1)]
                       ,[(MoveE, 1)]
                       ,[(MoveN, 1)]
                       ,[(MoveS, 1)]
                       ,[(MoveNE, 1)]
                       ,[(MoveNW, 1)]
                       ,[(MoveSE, 1)]
                       ,[(MoveSW, 1)]]
maybeReverseActions :: Probability -> ActionResults
maybeReverseActions p = [[(MoveW, p), (MoveE, p2)]
                        ,[(MoveE, p), (MoveW, p2)]
                        ,[(MoveN, p), (MoveS, p2)]
                        ,[(MoveS, p), (MoveN, p2)]
                        ,[(MoveNE, p), (MoveSW, p2)]
                        ,[(MoveNW, p), (MoveSE, p2)]
                        ,[(MoveSE, p), (MoveNW, p2)]
                        ,[(MoveSW, p), (MoveNE, p2)]]
    where p2 = 1-p
maybeActions :: Probability -> ActionResults
maybeActions p = [[(MoveW, p), (MoveNone, p2)]
                 ,[(MoveE, p), (MoveNone, p2)]
                 ,[(MoveN, p), (MoveNone, p2)]
                 ,[(MoveS, p), (MoveNone, p2)]
                 ,[(MoveNE, p), (MoveNone, p2)]
                 ,[(MoveNW, p), (MoveNone, p2)]
                 ,[(MoveSE, p), (MoveNone, p2)]
                 ,[(MoveSW, p), (MoveNone, p2)]]
    where p2 = 1-p
scatterActions :: Probability -> ActionResults
scatterActions p = [[(MoveW, p), (MoveNW, p2), (MoveSW, p2)]
                   ,[(MoveE, p), (MoveNE, p2), (MoveSE, p2)]
                   ,[(MoveN, p), (MoveNE, p2), (MoveNW, p2)]
                   ,[(MoveS, p), (MoveSE, p2), (MoveSW, p2)]
                   ,[(MoveNE, p), (MoveE, p2), (MoveN, p2)]
                   ,[(MoveNW, p), (MoveW, p2), (MoveN, p2)]
                   ,[(MoveSE, p), (MoveE, p2), (MoveS, p2)]
                   ,[(MoveSW, p), (MoveW, p2), (MoveS, p2)]]
    where p2 = (1-p)/2

type BlockedCells = [(Int, Int)]
type MoveCost = Double
gridWorld :: (Int, Int) -> Discount -> MoveCost -> ActionResults -> BlockedCells -> GridWorld
gridWorld (w, h) gamma moveCost actionRes blockedCells = GridWorld w h bs mdp
    where numStates = w * h
          mdp = MDP ts rs as gamma
          rs = V.replicate numStates moveCost
          as = V.replicate numStates False
          nullT = V.replicate numStates 0.0
          ts = V.fromList $ map mkActTrans actionRes
          bs = V.replicate numStates False // map (\(x,y) -> (x*w + y, True)) blockedCells
          mkActTrans ar = V.generate numStates (makeTrans w h bs ar nullT)

makeTrans :: Int -> Int -> Vector Bool -> ActionResult -> Vector Probability -> State -> Vector Probability
makeTrans w h bs ar vs s = V.accum (+) vs (map (\(a,p) -> (getI w h bs s a, p)) ar)

getI :: Int -> Int -> Vector Bool -> State -> GridAction -> State
getI w _ bs i MoveN | i < w || bs ! (i-w)   = i
                    | otherwise             = i - w

getI w h bs i MoveS | i >= (w*(h-2)) || bs ! (i+w) = i
                    | otherwise                    = i + w

getI w _ bs i MoveW | (i `mod` w) == 0 || bs ! (i-1) = i
                    | otherwise                      = i - 1

getI w _ bs i MoveE | (i `mod` w) == (w-1) || bs ! (i+1) = i
                    | otherwise                          = i + 1

getI w h bs i MoveNE = getI w h bs (getI w h bs i MoveN) MoveE
getI w h bs i MoveNW = getI w h bs (getI w h bs i MoveN) MoveW
getI w h bs i MoveSE = getI w h bs (getI w h bs i MoveS) MoveE
getI w h bs i MoveSW = getI w h bs (getI w h bs i MoveS) MoveW
getI _ _ _ i MoveNone = i

reward :: (Int, Int) -> Reward -> GridWorld -> GridWorld
reward (x, y) r g = g { g_mdp = mdp' }
    where mdp = g_mdp g
          w = g_width g
          mdp' = mdp { rewards = rewards mdp // [((y * w + x), r)] }

absorb :: (Int, Int) -> Reward -> GridWorld -> GridWorld
absorb (x, y) r g = g { g_mdp = mdp' }
    where mdp = g_mdp g
          w = g_width g
          mdp' = mdp { rewards = rewards mdp // [((y * w + x), r)]
                     , absState = absState mdp // [((y * w + x), True)] }

initVals :: GridWorld -> GridVal
initVals gw = GridVal gw vs
    where vs = V.replicate (w*h) 0.0
          w = g_width gw
          h = g_height gw

type NumIters = Int
iterVals :: NumIters -> GridVal -> GridVal
iterVals 0 gv = gv
iterVals n (GridVal gw vs) = iterVals (n-1) $ GridVal gw vs'
    where vs' = V.fromList $ map iter_ [0..(V.length vs -1)]
          mdp = g_mdp gw
          iter_ i = let r = rewards mdp ! i
                        g = discount mdp
                        t = transitions mdp
                        isAbsorbing = absState mdp ! i
                        doAction ts = V.sum (V.zipWith (*) (ts ! i) vs)
                    in if isAbsorbing 
                       then r 
                       else r + g * V.maximum (V.map doAction t)
