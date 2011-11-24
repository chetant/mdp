import AI.MDP
import AI.MDP.GridWorld

gv = initVals $ absorb (2,2) 100 $ gridWorld (10, 10) 0.5 0 scatterActions (map (\x -> (x, 6)) [0..6])

main = do
  putStrLn "Rewards:"
  showRewards gv
  putStrLn "\n\nInit Values:"
  print gv
  putStrLn "\n\nValues after 100 Iterations:"
  print $ iterVals 100 gv
