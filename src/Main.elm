module Main exposing (Model, Msg(..), init, main, subs, tick, update, view)

--

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

type alias Input = Keyboard.Key
type alias InputMsg = Keyboard.Msg


type alias Model =
    { position : ( Float, Float )
    , velocity : ( Float, Float )
    , inputs : List Input
    }


type Msg
    = Tick Float
    | Input InputMsg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { position = ( 0, 3 )
      , velocity = ( 0, 0 )
      , inputs = []
      }
    , Cmd.none
    )


subs m =
    Sub.batch
    [ Sub.map Input Keyboard.subscriptions
    , onAnimationFrameDelta Tick]

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Tick dt ->
            (tick (dt / 1000) model, Cmd.none)

        Input inputMsg ->
            let
                inputs = Keyboard.update inputMsg model.inputs
            in
            ( { model | inputs = inputs }, Cmd.none )


tick dt model =
    let
        arrows =
            Keyboard.Arrows.arrows model.inputs
    in
    model
    |> movement (Debug.log "arrows=" arrows)
    |> physics dt

movement arrows model =
    let
        vx = toFloat arrows.x * 2.0
        vy = if arrows.y > 0 && Tuple.second model.velocity == 0 then 4.0 else Tuple.second model.velocity
    in
    { model | velocity = (vx, vy) }


physics dt model =
    let
        ( ( x, y ), ( vx, vy ) ) =
            ( model.position, model.velocity )

        vy_ =
            vy - 9.81 * dt

        ( newP, newV ) =
            if y <= 0 then
                ( ( x + vx*dt, 0.00001 ), ( vx, -vy_ * 0.9 ) )

            else
                ( ( x + vx*dt, y + vy_ * dt ), ( vx, vy_ ) )
    in
    { model | position = newP, velocity = newV }


view : Model -> Html Msg
view m =
    Game.renderCentered { time = 0, camera = Camera.fixedHeight 7 ( 0, 1.5 ), size = ( 800, 600 ) }
        [ Render.shape rectangle { color = Color.green, position = ( -10, -10 ), size = ( 20, 10 ) }
        , Render.shape circle { color = Color.blue, position = m.position, size = ( 0.5, 0.5 ) }
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = init
        , subscriptions = subs
        }
