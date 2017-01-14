{-# LANGUAGE OverloadedStrings #-}
module PageStyle where

import Clay

style :: Css
style = body ? do
  margin (px 0) (px 0) (px 0) (px 0)
  nav ? do
    backgroundColor (Rgba 200 200 200 0.5)
    borderBottom solid (px 1) black
    padding (px 5) (px 5) (px 5) (px 5)
    overflow hidden
    ul ? do
      margin (px 0) (px 0) (px 0) (px 0)
      listStyleType none
      li ? do
        float floatLeft
        border dotted (px 1) black
  div # ".content" ? do
    padding (px 10) (px 10) (px 10) (px 10)
    div # "#questions" ?
      div # ".question" ? do
        marginTop (em 0.5)
        backgroundColor (Rgba 200 200 200 0.5)
        border solid (px 1) black
