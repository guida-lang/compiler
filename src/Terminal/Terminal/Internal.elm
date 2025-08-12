module Terminal.Terminal.Internal exposing
    ( ArgError(..)
    , Args(..)
    , Command(..)
    , CompleteArgs(..)
    , Error(..)
    , Expectation(..)
    , Flag(..)
    , FlagError(..)
    , Flags(..)
    , Parser(..)
    , RequiredArgs(..)
    , Summary(..)
    , toName
    )

import Task exposing (Task)
import Text.PrettyPrint.ANSI.Leijen exposing (Doc)



-- COMMAND


type Command
    = Command String Summary String Doc Args Flags (List String -> Result Error (Task Never ()))


toName : Command -> String
toName (Command name _ _ _ _ _ _) =
    name


{-| The information that shows when you run the executable with no arguments.
If you say it is `Common`, you need to tell people what it does. Try to keep
it to two or three lines. If you say it is `Uncommon` you can rely on `Details`
for a more complete explanation.
-}
type Summary
    = Common String
    | Uncommon



-- FLAGS


type Flags
    = FDone
    | FMore Flags Flag


type Flag
    = Flag String Parser String
    | OnOff String String



-- PARSERS


type Parser
    = Parser
        { singular : String
        , plural : String

        -- ,parser : String -> Maybe a
        , suggest : String -> Task Never (List String)
        , examples : String -> Task Never (List String)
        }



-- ARGS


type Args
    = Args (List CompleteArgs)


type CompleteArgs
    = Exactly RequiredArgs
    | Multiple RequiredArgs Parser


type RequiredArgs
    = Done
    | Required RequiredArgs Parser



-- ERROR


type Error
    = BadArgs (List ArgError)
    | BadFlag FlagError


type ArgError
    = ArgMissing Expectation
    | ArgBad String Expectation
    | ArgExtras (List String)


type FlagError
    = FlagWithValue String String
    | FlagWithBadValue String String Expectation
    | FlagWithNoValue String Expectation
    | FlagUnknown String Flags


type Expectation
    = Expectation String (Task Never (List String))
