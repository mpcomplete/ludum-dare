module Main exposing (Model, Msg(..), init, main, subs, tick, update, view)

--

import AABB exposing (AABB, intersects)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color
import Debug
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, rectangle)
import Html exposing (..)
import Keyboard
import Keyboard.Arrows


type alias Input =
    Keyboard.Key


type alias InputMsg =
    Keyboard.Msg


type alias Vec2 =
    ( Float, Float )


type alias Unit =
    { position : Vec2
    , velocity : Vec2
    }


type alias Model =
    { player : Unit
    , inputs : List Input
    }


type alias Obstacle =
    AABB


type Msg
    = Tick Float
    | Input InputMsg


makeObstacle : Vec2 -> Float -> AABB
makeObstacle position width =
    AABB ( Tuple.first position - width / 2, Tuple.second position ) ( width, 0.5 )


obstacles_ =
    [ makeObstacle ( 0, 0 ) 2
    , makeObstacle ( -3, -4 ) 2
    , makeObstacle ( 3, -4 ) 2
    , makeObstacle ( 2, -10 ) 3
    , makeObstacle ( 0, -14 ) 4
    ]


obstacles =
    List.concatMap
        (\o ->
            let
                ( x, y ) =
                    o.position
            in
            [ AABB ( x, y ) o.size
            , AABB ( x, y - 20 ) o.size
            , AABB ( x, y - 40 ) o.size
            , AABB ( x, y - 60 ) o.size
            , AABB ( x, y - 100 ) o.size
            , AABB ( x, y - 150 ) o.size
            , AABB ( x, y - 350 ) o.size
            , AABB ( x, y - 600 ) o.size
            ]
        )
        obstacles_


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player =
            { position = ( 0, 3 )
            , velocity = ( 0, 0 )
            }
      , inputs = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick dt ->
            let
                player =
                    tick (dt / 1000) model
            in
            ( { model | player = player }, Cmd.none )

        Input inputMsg ->
            let
                inputs =
                    Keyboard.update inputMsg model.inputs
            in
            ( { model | inputs = inputs }, Cmd.none )


tick dt model =
    let
        arrows =
            Keyboard.Arrows.arrows model.inputs
    in
    model.player
        |> movement arrows
        |> gravity dt


movement arrows player =
    let
        vx =
            toFloat arrows.x * 4.0

        vy =
            if arrows.y > 0 && Tuple.second player.velocity == 0 then
                4.0

            else
                Tuple.second player.velocity
    in
    { player | velocity = ( vx, vy ) }


gravity dt player =
    let
        ( ( x, y ), ( vx, vy ) ) =
            ( player.position, player.velocity )

        vy_ =
            vy - 9.81 * dt

        ( position, velocity ) =
            if collides player.position then
                ( ( x + vx * dt, y + 0.1 ), ( vx, -vy_ * 0.6 ) )

            else
                ( ( x + vx * dt, y + vy_ * dt ), ( vx, vy_ ) )
    in
    { player | position = position, velocity = velocity }


collides position =
    let
        box =
            AABB position ( 0.5, 0.5 )
    in
    List.any (\o -> AABB.intersects o box) obstacles


view : Model -> Html Msg
view m =
    Game.renderCentered
        { time = 0, camera = Camera.fixedHeight 11 ( 0, Tuple.second m.player.position - 4 ), size = ( 800, 600 ) }
        (Render.shape rectangle { color = Color.blue, position = m.player.position, size = ( 0.5, 0.5 ) }
            :: renderObstacles
        )


renderObstacles =
    List.map
        (\o -> Render.shape rectangle { color = Color.blue, position = o.position, size = o.size })
        obstacles


subs m =
    Sub.batch
        [ Sub.map Input Keyboard.subscriptions
        , onAnimationFrameDelta Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }
