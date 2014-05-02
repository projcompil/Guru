import Data.List
import System.Random

nw = 300000 -- borne maximale sur les w
np = 10000 -- idem sur les p 


generec lw lp leps = let (w:xw, p:xp, eps:xeps) = (lw, lp, leps) in
			((2+w, 2+p, eps), (xw, xp, xeps))

calct pi [] = 0.0
calct pi ((w,p,eps):l) = w/pi + eps * (calct (pi + p) l) + (1 - eps) * (calct pi l)

generep n 0 = [replicate n False]
generep 0 k = [[]]
generep n k
	| n == k = [replicate n True]
	| otherwise = (map (False:) $ generep (n-1) k ) ++ (map (True:) $ generep (n-1) (k-1))


estime_moyenne calcul m p l  =
	let 
	auxi 0 acc = acc
	auxi i acc = auxi (i-1) ((calcul p l)/m)
	in auxi m 0
	


main = do
	gpi:gw:gp:geps:_ <- sequence (replicate 4 newStdGen)
	let lpi = randomRs (0, np) gpi :: [Int]
	let lw = randomRs (0, nw) gw :: [Int]
	let lp = randomRs (0, np) gp :: [Int]
	let leps = randomRs (0, 1) geps :: [Double]
	print (head lp)
	print (head lpi) 
