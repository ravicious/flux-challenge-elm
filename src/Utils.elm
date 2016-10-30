module Utils exposing (applyNTimes)

{-| A module for utils used throughout the project.
-}


{-| Applies a function to a value n times.
-}
applyNTimes : Int -> (a -> a) -> a -> a
applyNTimes n f val =
    if n <= 0 then
        val
    else
        applyNTimes (n - 1) f (f val)
