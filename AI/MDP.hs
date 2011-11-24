module AI.MDP where

import Data.Vector(Vector)

type Probability = Double
type Reward = Double
type Discount = Double
type State = Int
type Transitions = Vector (Vector (Vector Probability))

data MDP = MDP { transitions ::  Transitions -- P(s'|s,a)
               , rewards :: Vector Reward -- R(s)
               , absState :: Vector Bool -- s == Absorbing state?
               , discount :: !Discount -- gamma
               } deriving (Show, Eq)

type Values = Vector Double

