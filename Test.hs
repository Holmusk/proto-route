module Test where

import Brick

ui :: Widget ()
ui = str "Sup world"

main :: IO ()
main = simpleMain ui
