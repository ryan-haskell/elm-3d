module Elm3d.Window exposing
    ( Window
    , fullscreen, fullscreenAspect, fixed
    , isFullscreen, isFullscreenAspect
    , toSize
    )

{-|

@docs Window
@docs fullscreen, fullscreenAspect, fixed

@docs isFullscreen, isFullscreenAspect
@docs toSize

-}


{-| -}
type Window
    = Fullscreen
    | FullscreenAspect Float
    | Fixed ( Int, Int )


fullscreen : Window
fullscreen =
    Fullscreen


fullscreenAspect : Float -> Window
fullscreenAspect =
    FullscreenAspect


fixed : ( Int, Int ) -> Window
fixed =
    Fixed


isFullscreen : Window -> Bool
isFullscreen window =
    case window of
        Fullscreen ->
            True

        _ ->
            False


isFullscreenAspect : Window -> Bool
isFullscreenAspect window =
    case window of
        FullscreenAspect _ ->
            True

        _ ->
            False


toSize : ( Int, Int ) -> Window -> ( Int, Int )
toSize ( vw, vh ) window =
    case window of
        Fullscreen ->
            ( vw, vh )

        FullscreenAspect ratio ->
            if toFloat vw / toFloat vh > ratio then
                ( round (toFloat vh * ratio)
                , vh
                )

            else
                ( vw
                , round (toFloat vw / ratio)
                )

        Fixed ( w, h ) ->
            ( w, h )
