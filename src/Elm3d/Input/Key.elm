module Elm3d.Input.Key exposing
    ( Key(..)
    , fromCode, toCode
    )

{-|

@docs Key
@docs fromCode, toCode

-}

import Dict exposing (Dict)


{-| Represents a key on your keyboard
-}
type Key
    = KEY_BACKSPACE
    | KEY_TAB
    | KEY_ENTER
    | KEY_SHIFT
    | KEY_CTRL
    | KEY_ALT
    | KEY_PAUSE_BREAK
    | KEY_CAPS_LOCK
    | KEY_ESCAPE
    | KEY_PAGE_UP
    | KEY_SPACE
    | KEY_PAGE_DOWN
    | KEY_END
    | KEY_HOME
    | KEY_ARROW_LEFT
    | KEY_ARROW_UP
    | KEY_ARROW_RIGHT
    | KEY_ARROW_DOWN
    | KEY_PRINT_SCREEN
    | KEY_INSERT
    | KEY_DELETE
    | KEY_0
    | KEY_1
    | KEY_2
    | KEY_3
    | KEY_4
    | KEY_5
    | KEY_6
    | KEY_7
    | KEY_8
    | KEY_9
    | KEY_A
    | KEY_B
    | KEY_C
    | KEY_D
    | KEY_E
    | KEY_F
    | KEY_G
    | KEY_H
    | KEY_I
    | KEY_J
    | KEY_K
    | KEY_L
    | KEY_M
    | KEY_N
    | KEY_O
    | KEY_P
    | KEY_Q
    | KEY_R
    | KEY_S
    | KEY_T
    | KEY_U
    | KEY_V
    | KEY_W
    | KEY_X
    | KEY_Y
    | KEY_Z
    | KEY_LEFT_WINDOW_KEY
    | KEY_RIGHT_WINDOW_KEY
    | KEY_SELECT_KEY
    | KEY_NUMPAD_0
    | KEY_NUMPAD_1
    | KEY_NUMPAD_2
    | KEY_NUMPAD_3
    | KEY_NUMPAD_4
    | KEY_NUMPAD_5
    | KEY_NUMPAD_6
    | KEY_NUMPAD_7
    | KEY_NUMPAD_8
    | KEY_NUMPAD_9
    | KEY_MULTIPLY
    | KEY_ADD
    | KEY_SUBTRACT
    | KEY_DECIMAL_POINT
    | KEY_DIVIDE
    | KEY_F1
    | KEY_F2
    | KEY_F3
    | KEY_F4
    | KEY_F5
    | KEY_F6
    | KEY_F7
    | KEY_F8
    | KEY_F9
    | KEY_F10
    | KEY_F11
    | KEY_F12
    | KEY_NUM_LOCK
    | KEY_SCROLL_LOCK
    | KEY_MY_COMPUTER
    | KEY_MY_CALCULATOR
    | KEY_SEMI_COLON
    | KEY_EQUAL_SIGN
    | KEY_COMMA
    | KEY_DASH
    | KEY_PERIOD
    | KEY_FORWARD_SLASH
    | KEY_OPEN_BRACKET
    | KEY_BACK_SLASH
    | KEY_CLOSE_BRACKET
    | KEY_SINGLE_QUOTE


{-| Convert a `Key` to it's character code.

    toCode KEY_ENTER == 13

    toCode KEY_ARROW_DOWN == 40

-}
toCode : Key -> Int
toCode key =
    case key of
        KEY_BACKSPACE ->
            8

        KEY_TAB ->
            9

        KEY_ENTER ->
            13

        KEY_SHIFT ->
            16

        KEY_CTRL ->
            17

        KEY_ALT ->
            18

        KEY_PAUSE_BREAK ->
            19

        KEY_CAPS_LOCK ->
            20

        KEY_ESCAPE ->
            27

        KEY_PAGE_UP ->
            33

        KEY_SPACE ->
            32

        KEY_PAGE_DOWN ->
            34

        KEY_END ->
            35

        KEY_HOME ->
            36

        KEY_ARROW_LEFT ->
            37

        KEY_ARROW_UP ->
            38

        KEY_ARROW_RIGHT ->
            39

        KEY_ARROW_DOWN ->
            40

        KEY_PRINT_SCREEN ->
            44

        KEY_INSERT ->
            45

        KEY_DELETE ->
            46

        KEY_0 ->
            48

        KEY_1 ->
            49

        KEY_2 ->
            50

        KEY_3 ->
            51

        KEY_4 ->
            52

        KEY_5 ->
            53

        KEY_6 ->
            54

        KEY_7 ->
            55

        KEY_8 ->
            56

        KEY_9 ->
            57

        KEY_A ->
            65

        KEY_B ->
            66

        KEY_C ->
            67

        KEY_D ->
            68

        KEY_E ->
            69

        KEY_F ->
            70

        KEY_G ->
            71

        KEY_H ->
            72

        KEY_I ->
            73

        KEY_J ->
            74

        KEY_K ->
            75

        KEY_L ->
            76

        KEY_M ->
            77

        KEY_N ->
            78

        KEY_O ->
            79

        KEY_P ->
            80

        KEY_Q ->
            81

        KEY_R ->
            82

        KEY_S ->
            83

        KEY_T ->
            84

        KEY_U ->
            85

        KEY_V ->
            86

        KEY_W ->
            87

        KEY_X ->
            88

        KEY_Y ->
            89

        KEY_Z ->
            90

        KEY_LEFT_WINDOW_KEY ->
            91

        KEY_RIGHT_WINDOW_KEY ->
            92

        KEY_SELECT_KEY ->
            93

        KEY_NUMPAD_0 ->
            96

        KEY_NUMPAD_1 ->
            97

        KEY_NUMPAD_2 ->
            98

        KEY_NUMPAD_3 ->
            99

        KEY_NUMPAD_4 ->
            100

        KEY_NUMPAD_5 ->
            101

        KEY_NUMPAD_6 ->
            102

        KEY_NUMPAD_7 ->
            103

        KEY_NUMPAD_8 ->
            104

        KEY_NUMPAD_9 ->
            105

        KEY_MULTIPLY ->
            106

        KEY_ADD ->
            107

        KEY_SUBTRACT ->
            109

        KEY_DECIMAL_POINT ->
            110

        KEY_DIVIDE ->
            111

        KEY_F1 ->
            112

        KEY_F2 ->
            113

        KEY_F3 ->
            114

        KEY_F4 ->
            115

        KEY_F5 ->
            116

        KEY_F6 ->
            117

        KEY_F7 ->
            118

        KEY_F8 ->
            119

        KEY_F9 ->
            120

        KEY_F10 ->
            121

        KEY_F11 ->
            122

        KEY_F12 ->
            123

        KEY_NUM_LOCK ->
            144

        KEY_SCROLL_LOCK ->
            145

        KEY_MY_COMPUTER ->
            182

        KEY_MY_CALCULATOR ->
            183

        KEY_SEMI_COLON ->
            186

        KEY_EQUAL_SIGN ->
            187

        KEY_COMMA ->
            188

        KEY_DASH ->
            189

        KEY_PERIOD ->
            190

        KEY_FORWARD_SLASH ->
            191

        KEY_OPEN_BRACKET ->
            219

        KEY_BACK_SLASH ->
            220

        KEY_CLOSE_BRACKET ->
            221

        KEY_SINGLE_QUOTE ->
            222


{-| Get a `Key` from its character code.

    toCode 13 == Just KEY_ENTER

    toCode 40 == Just KEY_ARROW_DOWN

    toCode 420 == Nothing

-}
fromCode : Int -> Maybe Key
fromCode int =
    case int of
        8 ->
            Just KEY_BACKSPACE

        9 ->
            Just KEY_TAB

        13 ->
            Just KEY_ENTER

        16 ->
            Just KEY_SHIFT

        17 ->
            Just KEY_CTRL

        18 ->
            Just KEY_ALT

        19 ->
            Just KEY_PAUSE_BREAK

        20 ->
            Just KEY_CAPS_LOCK

        27 ->
            Just KEY_ESCAPE

        33 ->
            Just KEY_PAGE_UP

        32 ->
            Just KEY_SPACE

        34 ->
            Just KEY_PAGE_DOWN

        35 ->
            Just KEY_END

        36 ->
            Just KEY_HOME

        37 ->
            Just KEY_ARROW_LEFT

        38 ->
            Just KEY_ARROW_UP

        39 ->
            Just KEY_ARROW_RIGHT

        40 ->
            Just KEY_ARROW_DOWN

        44 ->
            Just KEY_PRINT_SCREEN

        45 ->
            Just KEY_INSERT

        46 ->
            Just KEY_DELETE

        48 ->
            Just KEY_0

        49 ->
            Just KEY_1

        50 ->
            Just KEY_2

        51 ->
            Just KEY_3

        52 ->
            Just KEY_4

        53 ->
            Just KEY_5

        54 ->
            Just KEY_6

        55 ->
            Just KEY_7

        56 ->
            Just KEY_8

        57 ->
            Just KEY_9

        65 ->
            Just KEY_A

        66 ->
            Just KEY_B

        67 ->
            Just KEY_C

        68 ->
            Just KEY_D

        69 ->
            Just KEY_E

        70 ->
            Just KEY_F

        71 ->
            Just KEY_G

        72 ->
            Just KEY_H

        73 ->
            Just KEY_I

        74 ->
            Just KEY_J

        75 ->
            Just KEY_K

        76 ->
            Just KEY_L

        77 ->
            Just KEY_M

        78 ->
            Just KEY_N

        79 ->
            Just KEY_O

        80 ->
            Just KEY_P

        81 ->
            Just KEY_Q

        82 ->
            Just KEY_R

        83 ->
            Just KEY_S

        84 ->
            Just KEY_T

        85 ->
            Just KEY_U

        86 ->
            Just KEY_V

        87 ->
            Just KEY_W

        88 ->
            Just KEY_X

        89 ->
            Just KEY_Y

        90 ->
            Just KEY_Z

        91 ->
            Just KEY_LEFT_WINDOW_KEY

        92 ->
            Just KEY_RIGHT_WINDOW_KEY

        93 ->
            Just KEY_SELECT_KEY

        96 ->
            Just KEY_NUMPAD_0

        97 ->
            Just KEY_NUMPAD_1

        98 ->
            Just KEY_NUMPAD_2

        99 ->
            Just KEY_NUMPAD_3

        100 ->
            Just KEY_NUMPAD_4

        101 ->
            Just KEY_NUMPAD_5

        102 ->
            Just KEY_NUMPAD_6

        103 ->
            Just KEY_NUMPAD_7

        104 ->
            Just KEY_NUMPAD_8

        105 ->
            Just KEY_NUMPAD_9

        106 ->
            Just KEY_MULTIPLY

        107 ->
            Just KEY_ADD

        109 ->
            Just KEY_SUBTRACT

        110 ->
            Just KEY_DECIMAL_POINT

        111 ->
            Just KEY_DIVIDE

        112 ->
            Just KEY_F1

        113 ->
            Just KEY_F2

        114 ->
            Just KEY_F3

        115 ->
            Just KEY_F4

        116 ->
            Just KEY_F5

        117 ->
            Just KEY_F6

        118 ->
            Just KEY_F7

        119 ->
            Just KEY_F8

        120 ->
            Just KEY_F9

        121 ->
            Just KEY_F10

        122 ->
            Just KEY_F11

        123 ->
            Just KEY_F12

        144 ->
            Just KEY_NUM_LOCK

        145 ->
            Just KEY_SCROLL_LOCK

        182 ->
            Just KEY_MY_COMPUTER

        183 ->
            Just KEY_MY_CALCULATOR

        186 ->
            Just KEY_SEMI_COLON

        187 ->
            Just KEY_EQUAL_SIGN

        188 ->
            Just KEY_COMMA

        189 ->
            Just KEY_DASH

        190 ->
            Just KEY_PERIOD

        191 ->
            Just KEY_FORWARD_SLASH

        219 ->
            Just KEY_OPEN_BRACKET

        220 ->
            Just KEY_BACK_SLASH

        221 ->
            Just KEY_CLOSE_BRACKET

        222 ->
            Just KEY_SINGLE_QUOTE

        _ ->
            Nothing
