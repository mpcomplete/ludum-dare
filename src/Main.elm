module Main exposing (Model, Msg(..), init, main, subs, tick, update, view)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, rectangle)
import Html exposing (..)
import Keyboard
import Keyboard.Arrows
import Debug

-- Every cell in the game of life is on or off
-- Rendering that cell is a function which is either a recursive
-- call to game of life or a coloring function 
type Msg
  = Tick Float
  | Input Keyboard.Msg

type CellState
  = On
  | Off

type CellType
  = GameOfLife (List Cell)
  | Base

type alias Cell = (CellState, CellType)

type alias Model = {
  world: Cell,
  frame: Float
  }

init : () -> (Model, Cmd Msg)
init _ = 
  let world0 = { world = (On, Base), frame = 0 }
  in  (world0, Cmd.none)

subs : a -> Sub msg
subs _ = Sub.batch [ Sub.map Input Keyboard.subscriptions, onAnimationFrameDelta Tick ]

update : Msg -> Model -> (Model, Cmd msg)
update msg model = case msg of 
  Tick dt        -> (tick (dt / 1000) model, Cmd.none)
  Input inputMsg ->
    let inputs = Keyboard.update inputMsg model.inputs
        modeln = { model | inputs = inputs }
    in  (modeln, Cmd.none)

tick : Float -> Model
tick dt model = 
  let arrows = Keyboard.Arrows.arrows model.inputs
  in  Debug.log "arrows=" arrows

view : Model -> Html Msg
view m =
  let params = Game.renderCentered { time = 0, camera = Camera.fixedHeight 7 ( 0, 1.5 ), size = ( 800, 600 ) }
      root = Render.shape rectangle { color = Color.green, position = (0,0) size = (100,100) }
  in  Game.renderCentered params [ root ]

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subs
        }
