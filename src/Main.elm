module Main exposing (Model, Msg(..), init, main, subs, update, view)

import Array exposing (Array)
import List
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color exposing (Color)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, shape, circle, rectangle)
import Html exposing (..)
import Keyboard
import Keyboard.Arrows
import Game.TwoD.Render exposing (Float2)
import Game.TwoD.Shaders exposing (colorToRGBAVector)

type Msg
  = Tick Float
  | Input Keyboard.Msg

type CellState
  = On
  | Off

type CellType
  = GameOfLife (Array Cell)
  | Base

type alias Cell = 
  (CellState, CellType)

type alias Model = {
  world: Cell,
  frame: Int,
  elapsed: Float,
  inputs: List Keyboard.Key
  }

initialCellStates : Array CellState
initialCellStates = Array.fromList <| List.reverse [
  On, On, On, On, On, On, On, On, On, Off,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, Off, On,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, On, On,
  On, On, On, On, On, On, On, On, On, On,
  Off, On, On, On, On, On, On, On, On, On
  ]

subs : a -> Sub Msg
subs _ = Sub.batch [ 
  Sub.map Input Keyboard.subscriptions, 
  onAnimationFrameDelta Tick
  ]

-- spatial existence checks
northedge (w,h) i = i // w == (w - 1)
southedge (w,h) i = i // w == 0
westedge  (w,h) i = modBy w i == (w - 1)
eastedge  (w,h) i = modBy w i == 0

-- cardinal directions to index offsets
north (w,h) = w
northwest (w,h) = w + 1
west (w,h) = 1
southwest (w,h) = -w + 1
south (w,h) = -w
southeast (w,h) = -w - 1
east (e,h) = -1
northeast (w,h) = w - 1

-- sugar for monadic if-then-else checks
unless b v = if b then v else Nothing

-- generic neighbor checking function
neighbor checks cardinal dimensions cells index cell = 
  let runCheck check = check dimensions index
  in  if (checks |> List.any runCheck)
        then (cells |> Array.get (index + cardinal dimensions))
        else Nothing

northneighbor     = neighbor [ northedge ]           north
northwestneighbor = neighbor [ northedge, westedge ] northwest
westneighbor      = neighbor [ westedge ]            west
southwestneighbor = neighbor [ southedge, westedge ] west
southneighbor     = neighbor [ southedge ]           south
southeastneighbor = neighbor [ southedge, eastedge ] southeast
eastneighbor      = neighbor [ eastedge ]            east
northeastneighbor = neighbor [ northedge, eastedge ] northeast

updateCell : Cell -> Cell
updateCell cell = case cell of
  (On, GameOfLife innerCells) -> (On, GameOfLife <| Array.map updateCell innerCells)
  (On, Base)                  -> cell
  _                           -> cell

-- utility for converting 2d coordinates to normalized floating point offsets
indexToOffset (w,_) index = (index // w |> toFloat, modBy w index |> toFloat)

-- utility for basic math on tuples... jesus christ
scale s (x,y) = (s * x, s * y)
add (u,v) (x,y) = (u + x, v + y)

mkTransform : Float -> (Int,Int) -> Int -> Float2
mkTransform size dimensions index = 
  let offset = indexToOffset dimensions index
  in  scale size offset

-- determine color of cells based on neighorhood properties
colorCell dimensions cells index cell = 
  if (northedge dimensions index) then Color.red else
  if (westedge dimensions index)  then Color.green else
  if (southedge dimensions index) then Color.blue else 
  if (eastedge dimensions index)  then Color.yellow else
  Color.black

renderCell : Color -> Float2 -> Float -> Cell -> List Renderable
renderCell color origin size cell = case cell of
  (Off, _) -> 
    []
  (On, Base) ->
    [ shape rectangle { color = color, position = origin, size = (size,size) } ]
  (On, GameOfLife cells) -> 
    cells |>
    Array.toList |>
    List.indexedMap (\i c -> renderCell (colorCell (10,10) cells i c) (add origin (mkTransform (size / 10) (10,10) i)) (size / 10) c) |>
    List.concat

init : () -> (Model, Cmd Msg)
init _ = 
  let innercells = Array.map (\cs -> (cs, Base)) initialCellStates
      cells      = Array.map (\cs -> (cs, GameOfLife innercells)) initialCellStates
      world      = { world = (On, GameOfLife cells), frame = 0, elapsed = 0, inputs = [] }
      world0     = { world = (On, Base), frame = 0, elapsed = 0, inputs = [] }
  in  (world, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
  Tick dt ->
    let frame   = model.frame + 1
        elapsed = model.elapsed + dt
        world   = updateCell model.world
    in  ({ model | frame = frame, elapsed = elapsed, world = world }, Cmd.none)
  Input inputMsg ->
    let inputs = Keyboard.update inputMsg model.inputs
        model1 = { model | inputs = inputs }
    in  (model1, Cmd.none)
  
view : Model -> Html Msg
view model =
  let length = 1000
      dimensions = (10,10)
      area = length * length 
      position = (length / 2, length / 2)
      size = (length, length)
      index = 0
      origin = mkTransform length dimensions index
      camera = Camera.fixedArea area position
      params = { time = 0, camera = camera, size = size }
      renderables = renderCell Color.black origin length model.world
  in  Game.renderCentered params renderables

main : Program () Model Msg
main = Browser.element {
  view = view,
  update = update,
  init = init,
  subscriptions = subs
  }