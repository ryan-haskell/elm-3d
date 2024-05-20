module Elm3d.Viewport exposing
    ( Viewport
    , fullscreen, fullscreenAspect, fixed
    , isFullscreen, isFullscreenAspect
    , toSize
    )

{-| This module allows you to control how much space your Elm3d program should take up on the page.


# **Creating viewports**

@docs Viewport
@docs fullscreen, fullscreenAspect, fixed


# **Reading data**

@docs isFullscreen, isFullscreenAspect
@docs toSize

-}


{-| Represents how much space your 3D element should take up.
-}
type Viewport
    = Fullscreen
    | FullscreenAspect Float
    | Fixed ( Int, Int )


{-| Always take up the entire window, without a fixed aspect ratio.

    toSize ( 1920, 1080 ) fullscreen
        == ( 1920, 1080 )

    toSize ( 800, 600 ) fullscreen
        == ( 800, 600 )

    toSize ( 480, 480 ) fullscreen
        == ( 480, 480 )

-}
fullscreen : Viewport
fullscreen =
    Fullscreen


{-| Take up as much of the window as possible, but stay **centered on the page**
and **maintain the provided aspect ratio** (16:9, 4:3, etc).

    toSize ( 1920, 1080 ) (fullscreenAspect (16 / 9))
        == ( 1920, 1080 )

    toSize ( 800, 600 ) (fullscreenAspect (16 / 9))
        == ( 800, 450 )

    toSize ( 480, 480 ) (fullscreenAspect (16 / 9))
        == ( 480, 180 )

-}
fullscreenAspect : Float -> Viewport
fullscreenAspect =
    FullscreenAspect


{-| Regardless of window size, always be a fixed amount of pixels. If the window is smaller than that fixed size, users will see a scrollbar.

    toSize ( 1920, 1080 ) (fixed ( 400, 300 ))
        == ( 400, 300 )

    toSize ( 800, 600 ) (fixed ( 400, 300 ))
        == ( 400, 300 )

    toSize ( 480, 480 ) (fixed ( 400, 300 ))
        == ( 400, 300 )

-}
fixed : ( Int, Int ) -> Viewport
fixed =
    Fixed


{-| Return `True` if this is using `fullscreen`

    isFullscreen fullscreen
        == True

    isFullscreen (fullscreenAspect (16 / 9))
        == False

    isFullscreen (fixed ( 400, 300 ))
        == False

-}
isFullscreen : Viewport -> Bool
isFullscreen window =
    case window of
        Fullscreen ->
            True

        _ ->
            False


{-| Return `True` if this is using `fullscreenAspect`

    isFullscreen fullscreen
        == False

    isFullscreen (fullscreenAspect (16 / 9))
        == True

    isFullscreen (fixed ( 400, 300 ))
        == False

-}
isFullscreenAspect : Viewport -> Bool
isFullscreenAspect window =
    case window of
        FullscreenAspect _ ->
            True

        _ ->
            False


{-| When given the browser window's dimensions, this returns the size the rendered 3D viewport should be.

    toSize ( 800, 600 ) fullscreen
        == ( 800, 600 )

    toSize ( 800, 600 ) (fullscreenAspect (16 / 9))
        == ( 800, 450 )

    toSize ( 800, 600 ) (fixed ( 400, 300 ))
        == ( 400, 300 )

-}
toSize : ( Int, Int ) -> Viewport -> ( Int, Int )
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
