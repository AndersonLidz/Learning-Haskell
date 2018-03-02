{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

ourPicture :: Picture
ourPicture = colored green (translated 0 (-2) (solidCircle 0.7))
green_light True = colored green (translated 0 (-2) (solidCircle 0.7))
green_light False = colored white (translated 0 (-2) (solidCircle 0.7))
yellow_light True = colored yellow (translated 0 0 (solidCircle 0.7))
yellow_light False = colored white (translated 0 0 (solidCircle 0.7))
red_light True = colored red (translated 0 2 (solidCircle 0.7))
red_light False = colored white (translated 0 2 (solidCircle 0.7))
mypicture = green_light True & yellow_light True & red_light True


--light_change::Picture->Picture



next_light::Picture->Picture
next_light green_light = yellow_light True
next_light yellow_light = red_light True
next_light red_light = green_light True
light_change_green time
  |mod time 3 == 0 = light_change_yellow time
  |otherwise = green_light True
light_change_yellow time
  |mod time 3 == 0 = light_change_red time
  |otherwise = yellow_light True
light_change_red time
  |mod time 3 == 0 = light_change_green time
  |otherwise = red_light True

light_change_time time
  |mod (round(time)) 9 == 0 = red_light True
  |mod (round(time+1)) 9 ==0 = red_light True
  |mod (round(time+2)) 9 ==0 = red_light True
  |mod (round(time)) 6 ==0 = yellow_light True
  |mod (round(time+1)) 6 ==0 = yellow_light True
  |mod (round(time+2)) 6 ==0 = yellow_light True
  |otherwise = green_light True

--mypicture = colored yellow (solidRectangle 10 10) & solidCircle 10
main :: IO ()
--main = drawingOf mypicture

main = animationOf light_change_time
