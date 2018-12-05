module MyAnimation where

import Animation
import Data.List

-- File path of electron using equation of ellipse (x^2/a^2 + y^2/b^2 = 1)
findPath a b  = zip [-a..a] [ sqrt(b**2*(1-(xs**2/a**2))) | xs <- [-a..a] ] 
				++ zip (reverse [-a..a]) [ -sqrt(b**2*(1-(xs**2/a**2))) | xs <- reverse [-a..a] ]

-- Array of different parameters of 3 rectangles
-- [inner rect, middle rect, outer rect]
colors = [navy,black,black]              				  -- array of colors of rectangles
translationsRect = [(125,125),(50,50),(10,10)]			  -- array of transtations of rectangles
dim_xs = [350,500,580]                                    -- array of x-dimension of rectangles 
dim_ys = [350,500,580]									  -- array of y-dimension of rectangles
opacities = [0.4,1,1]									  -- array of opacities of boundries of rectangles
thicknesses = [1,5,5]									  -- array of thickness of boundries of rectangles

-- draw all 4 rectangkes
rectangles :: Animation
rectangles = (combine                                                            -- combine all stationary rectangles
			   [translate (always trans) 										 -- translate each rectangle from origin to 'trans' coordinate
			 	(withGenBorder (always color) (always opacity) (always thickness)-- make border of rect using 'color', 'opacity','thickness' from 6-tuple
			 	(withoutPaint (rect (always dim_x) (always dim_y))) ) | (trans,color,opacity,thickness,dim_x,dim_y) <-       -- make rect using 'dim_x' and 'dim_y' from 6-tuple
	  			  zipWith6 (\tr c o th x y -> (tr,c,o,th,x,y)) translationsRect colors opacities thicknesses dim_xs dim_ys ])-- This will make 6-tuple having different properties of each rect.
		     `plus`
	     	 (translate (cycleSmooth 2 [(0,0),(350,0),(350,350),(0,350)] )      -- This will draw a moving rectangle. 
	     	   (translate (always (115,115)) (rect (always 20) (always 20))))

-- Array of different parameters of all 9 balls(3 balls and six electrons)
-- [nucleus,left ball, right ball,e1,e2,e3,e4,e5,e6]
translationsCir = [[(0,0),(0,0)],[(30-300,30-300),(30-300,570-300)],[(270,-270),(270,270)]] ++ replicate 6 (findPath 200 50)      -- cyclic path of each circle
radii = [30,16,16] ++ replicate 6 7										      -- Radius of each circle
colorCycle = [[red,maroon,gray],[red,yellow,gray,black,gray],[red,yellow,gray,black,gray]] ++ replicate 6 [aqua,gray,black,gray]  -- changing color patern of each circle
scaleCycle = [[(1,1),(1.1,1.1),(1,1),(0.9,0.9)],[(1,1),(1,1)],[(1,1),(1,1)]] ++ replicate 6 [(1,1),(1.4,1.4),(1.8,1.8),(1.4,1.4)] -- changing size scale of each circle
rotateAngle = [0,0,0,0,30,60,90,120,150]                                      -- Rotation angle of each circle
speed = (replicate 3 2.1) ++ [0.0052,0.0048,0.0061,0.0057,0.0061,0.0045]      -- speed of each circle

-- draw all 9 circles
circles :: Animation
circles = translate (always (300,300))									-- Translate all six circles from origin to the (300,300) coordinate
		  (combine 														-- Combine all 9 circles into one animation
		  	[rotate (always angle) pic | (angle,pic) <- 				-- Rotate each circle 'pic' at required angle 'angle'
		  	zip rotateAngle                                             -- makes tuple of each circle with its rotation angle
		  	([translate (cycleSmooth spd trans )        				   -- this moves each circle in path 'trans' with speed 'spd'
	      	(withPaint (cycleSmooth 1.7 color)							   -- cyclic color change of each circle using 'color' from 5-tuple
	      	  (scale (cycleSmooth 0.5 scl)                                 -- cyclic scaling of each circle using 'scl' from 5-tuple
	      	  (circle (always radius)))) | (spd,trans,color,scl,radius) <- -- Draw circle using 'radius' from 5-tuple i.e. (spd,trans,color,scl,radius)
	      	  	zipWith5 (\ sp t c s r -> (sp,t,c,s,r)) speed translationsCir colorCycle scaleCycle radii])  ]) -- this makes 5-tuple of different parameters of required circles.

-- draw all 6 orbits
orbits :: Animation
orbits = translate (always (300,300)) 										-- Translate all six orbits from origin to the (300,300) coordinate
		 (combine                     										-- Combine all six animations into one.  
		 	[rotate (always angle) (orbit) | angle <-  [0,30,60,90,120,150],-- Rotate each 'orbit' at given 'angle'
		 	let orbit = withGenBorder (always navy) (always 0.4) (always 1) (withoutPaint (ellipse (always 200) (always 50))) ])	
		 	-- The above line will draw the orbits. 


-- combine all three types of pictures 
picture :: Animation
picture = rectangles`plus`circles `plus` orbits

-- piece of magic
test :: IO ()
test = writeFile "Pendulum.svg" (svg 800 600  picture)
