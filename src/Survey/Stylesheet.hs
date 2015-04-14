module Survey.Stylesheet where

import Stitch
import Stitch.Combinators

style :: CSS
style = do
  "body" ? do
    "background-color" .= "#3498db"
  "ul" ? do
    "background-color" .= "rgba(255,255,255,0.75)"
    "margin" .= "16px auto"
    "padding" .= "24px"
    "max-width" .= "40rem"
    "border-radius" .= "3px"
    "box-shadow" .= "0 2px 4px rgba(0,0,0,0.25)"
    "li" ? do
      "margin" .= "8px"
      "input[type=checkbox]" ? do
        "margin" .= "4px"
      ".row_name" ? do
        "opacity" .= "0.5"
