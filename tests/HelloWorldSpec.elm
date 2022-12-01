module HelloWorldSpec exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "App"
        [ Test.test "works" <|
            \_ -> Expect.pass
        ]
