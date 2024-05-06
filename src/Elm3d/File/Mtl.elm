module Elm3d.File.Mtl exposing
    ( Data
    , Error(..)
    )


type alias Data =
    { url : String
    , raw : String
    }


type Error
    = Error
