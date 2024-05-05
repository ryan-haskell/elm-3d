module Elm3d.Window exposing
    ( Window
    , fullscreen, fixed
    , toSize
    )

{-|

@docs Window
@docs fullscreen, fixed
@docs toSize

-}


type Window
    = Fullscreen
    | Fixed ( Int, Int )


fullscreen : Window
fullscreen =
    Fullscreen


fixed : ( Int, Int ) -> Window
fixed =
    Fixed


toSize : ( Int, Int ) -> Window -> ( Int, Int )
toSize ( vw, vh ) window =
    case window of
        Fullscreen ->
            ( vw, vh )

        Fixed ( w, h ) ->
            ( w, h )
