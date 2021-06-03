module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs exposing (value)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), Parser, Step(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    String


init =
    """Thy! nên hay chuẩn bị được nhiêu đây. Happy Birthday
nên nay đã là tui gà
nay Hé lô quên
Hé 
lô 
đã Thy. Một được
Thy. 
Một 
trong lại. Thú chỉ ngày một
lại. dự án sống
dự 
án 
Thú tui bỏ thật
tui 
bỏ """


type Msg
    = GotRaw String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotRaw text_ ->
            text_


view : String -> Html Msg
view raw =
    Html.div []
        [ Html.img [ Attrs.src "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR6KDQjTtkeNJAV7aRDRWAZYOYgykptHcW4WuCQ9Jjo7RKvgoIk4T8VqT3FY2nreMjA35E&usqp=CAU" ] []
        , Html.textarea
            [ onInput GotRaw
            , value raw
            , Attrs.style "width" "100%"
            , Attrs.style "height" "150px"
            ]
            []
        , case Parser.run parserLeft raw |> Result.map treeInfo of
            Err error ->
                Html.text (Debug.toString error)

            Ok infoTree ->
                svg
                    [ Attrs.style "width" "100%"
                    , Attrs.style "height" "auto"
                    , viewBox "0 0 200 200"
                    ]
                <|
                    render 0 (getGen infoTree) infoTree
        ]



-- exampleTree : Tree (Person NoInfo)
-- exampleTree =
--     LeftTree (Person "K" 2 NoInfo)
--         (LeftTree (Person "M" 1 NoInfo)
--             (Leaf (Person "MM" 0 NoInfo))
--             (RightTree (Person "MD" 0 NoInfo)
--                 (Leaf (Person "MDM" 0 NoInfo))
--                 (Leaf (Person "MDD" 0 NoInfo))
--             )
--         )
--         (RightTree (Person "D" 2 NoInfo)
--             (LeftTree (Person "DM" 0 NoInfo)
--                 (Leaf (Person "DMM" 0 NoInfo))
--                 (Leaf (Person "DMD" 0 NoInfo))
--             )
--             (Leaf (Person "DD" 0 NoInfo))
--         )
-- raw =
--     """K M D 1 2 3
-- M MM MD 2 3
-- MM
-- MD MDM MDD 5 6
-- MDM
-- MDD
-- D DM DD 9 10
-- DM DMM DMD 7 8
-- DMM
-- DMD
-- DD """


parserLeft : Parser (Tree (Person NoInfo))
parserLeft =
    Parser.succeed identity
        |= parserName
        |. Parser.symbol " "
        |> Parser.andThen
            (\name ->
                Parser.oneOf
                    [ Parser.succeed (Leaf (Person name [] NoInfo))
                        |. Parser.oneOf
                            [ Parser.symbol "\n"
                            , Parser.end
                            ]
                    , Parser.succeed
                        (\lName rName sibs lTree rTree ->
                            LeftTree (Person name sibs NoInfo) lTree rTree
                        )
                        |= parserName
                        |. Parser.symbol " "
                        |= parserName
                        |= parserSiblings
                        |. Parser.symbol "\n"
                        |= Parser.lazy (\_ -> parserLeft)
                        |= Parser.lazy (\_ -> parserRight)
                    ]
            )


parserRight : Parser (Tree (Person NoInfo))
parserRight =
    Parser.succeed identity
        |= parserName
        |. Parser.symbol " "
        |> Parser.andThen
            (\name ->
                Parser.oneOf
                    [ Parser.succeed (Leaf (Person name [] NoInfo))
                        |. Parser.oneOf
                            [ Parser.symbol "\n"
                            , Parser.end
                            ]
                    , Parser.succeed
                        (\lName rName sibs lTree rTree ->
                            RightTree (Person name sibs NoInfo) lTree rTree
                        )
                        |= parserName
                        |. Parser.symbol " "
                        |= parserName
                        |= parserSiblings
                        |. Parser.symbol "\n"
                        |= Parser.lazy (\_ -> parserLeft)
                        |= Parser.lazy (\_ -> parserRight)
                    ]
            )


parserName =
    Parser.getChompedString
        (Parser.succeed ()
            |. Parser.chompIf (\c -> c /= ' ' && c /= '\n')
            |. Parser.chompWhile (\c -> c /= ' ' && c /= '\n')
        )


parserSiblings : Parser Siblings
parserSiblings =
    Parser.loop []
        (\sibs ->
            Parser.oneOf
                [ Parser.succeed (\name -> Loop (name :: sibs))
                    |. Parser.symbol " "
                    |= parserName
                , Parser.succeed (Done sibs)
                ]
        )



-- TYPES


type Tree a
    = LeftTree a (Tree a) (Tree a)
    | RightTree a (Tree a) (Tree a)
    | Leaf a


type Person a
    = Person Name Siblings a


type alias Name =
    String


type alias Siblings =
    List String


type Info
    = Info Int Int Int


type NoInfo
    = NoInfo


getPerson : Tree (Person a) -> Person a
getPerson tree =
    case tree of
        LeftTree person _ _ ->
            person

        RightTree person _ _ ->
            person

        Leaf person ->
            person


getInfo : Person a -> a
getInfo (Person _ _ info) =
    info


treeInfo : Tree (Person NoInfo) -> Tree (Person Info)
treeInfo tree =
    case tree of
        LeftTree (Person name siblings _) leftTree rightTree ->
            let
                infoTreeLeft =
                    treeInfo leftTree

                infoTreeRight =
                    treeInfo rightTree

                (Info lSize _ lGen) =
                    infoTreeLeft |> getPerson |> getInfo

                (Info rSize _ rGen) =
                    infoTreeRight |> getPerson |> getInfo

                prbOnLeft =
                    (List.length siblings + 1) // 2

                prbOnRight =
                    List.length siblings // 2 + 1

                excessOnRight =
                    if rSize >= prbOnRight then
                        0

                    else
                        prbOnRight - rSize

                info =
                    Info (Basics.max (List.length siblings + 1) (lSize + rSize))
                        (if List.length siblings + 1 >= lSize + rSize then
                            lSize + rSize - (List.length siblings + 1)

                         else
                            clamp 0 lSize (lSize - prbOnLeft - excessOnRight)
                        )
                        (Basics.max lGen rGen + 1)
            in
            LeftTree (Person name siblings info) infoTreeLeft infoTreeRight

        RightTree (Person name siblings _) leftTree rightTree ->
            let
                infoTreeLeft =
                    treeInfo leftTree

                infoTreeRight =
                    treeInfo rightTree

                (Info lSize _ lGen) =
                    infoTreeLeft |> getPerson |> getInfo

                (Info rSize _ rGen) =
                    infoTreeRight |> getPerson |> getInfo

                prbOnLeft =
                    List.length siblings // 2 + 1

                prbOnRight =
                    (List.length siblings + 1) // 2

                excessOnRight =
                    if rSize >= prbOnRight then
                        0

                    else
                        prbOnRight - rSize

                info =
                    Info (Basics.max (List.length siblings + 1) (lSize + rSize))
                        (if List.length siblings + 1 >= lSize + rSize then
                            0

                         else
                            clamp 0 lSize (lSize - prbOnLeft - excessOnRight)
                        )
                        (Basics.max lGen rGen + 1)
            in
            RightTree (Person name siblings info) infoTreeLeft infoTreeRight

        Leaf (Person name siblings _) ->
            Leaf (Person name siblings (Info 1 0 0))


getSize : Tree (Person Info) -> Int
getSize tree =
    case tree of
        LeftTree (Person _ _ (Info size _ _)) _ _ ->
            size

        RightTree (Person _ _ (Info size _ _)) _ _ ->
            size

        Leaf (Person _ _ (Info size _ _)) ->
            size


getIndent : Tree (Person Info) -> Int
getIndent tree =
    case tree of
        LeftTree (Person _ _ (Info _ indent _)) _ _ ->
            indent

        RightTree (Person _ _ (Info _ indent _)) _ _ ->
            indent

        Leaf (Person _ _ (Info _ indent _)) ->
            indent


getGen : Tree (Person Info) -> Int
getGen tree =
    case tree of
        LeftTree (Person _ _ (Info _ _ gen)) _ _ ->
            gen

        RightTree (Person _ _ (Info _ _ gen)) _ _ ->
            gen

        Leaf (Person _ _ (Info _ _ gen)) ->
            gen


getSiblings : Tree (Person a) -> Int
getSiblings tree =
    case tree of
        LeftTree (Person _ siblings _) _ _ ->
            List.length siblings

        RightTree (Person _ siblings _) _ _ ->
            List.length siblings

        Leaf (Person _ siblings _) ->
            List.length siblings


render : Int -> Int -> Tree (Person Info) -> List (Svg msg)
render offsetX offsetY tree =
    case tree of
        LeftTree (Person name siblings (Info size indent gen)) leftTree rightTree ->
            let
                topIndent =
                    if indent < 0 then
                        -indent

                    else
                        0

                bottomIndent =
                    if indent > 0 then
                        indent

                    else
                        0

                topLeftIndent =
                    if getIndent leftTree < 0 then
                        0

                    else
                        getIndent leftTree

                topRightIndent =
                    if getIndent rightTree < 0 then
                        0

                    else
                        getIndent rightTree
            in
            [ drawPerson name (offsetX + bottomIndent + List.length siblings) offsetY
            , drawSLine (offsetX + bottomIndent + List.length siblings) offsetY
            , siblings
                |> List.indexedMap
                    (\i name_ ->
                        g []
                            [ drawPerson name_ (offsetX + bottomIndent + List.length siblings - i - 1) offsetY
                            , drawSLine (offsetX + bottomIndent + List.length siblings - i - 1) offsetY
                            ]
                    )
                |> g []
            , drawSHLine
                (offsetX
                    + bottomIndent
                    - (if List.length siblings == 0 then
                        1

                       else
                        0
                      )
                )
                (offsetX + bottomIndent + List.length siblings)
                offsetY
            , drawHLine
                (offsetX + topIndent + topLeftIndent + getSiblings leftTree)
                (offsetX + topIndent + getSize leftTree + topRightIndent)
                (offsetY - 1)
            , drawVLine
                (offsetX + topIndent + getSize leftTree)
                (offsetY - 1)
            , g [] <| render (offsetX + topIndent) (offsetY - 1) leftTree
            , g [] <| render (offsetX + topIndent + getSize leftTree) (offsetY - 1) rightTree
            ]

        RightTree (Person name siblings (Info size indent gen)) leftTree rightTree ->
            let
                topIndent =
                    if indent < 0 then
                        -indent

                    else
                        0

                bottomIndent =
                    if indent > 0 then
                        indent

                    else
                        0

                topLeftIndent =
                    if getIndent leftTree < 0 then
                        0

                    else
                        getIndent leftTree

                topRightIndent =
                    if getIndent rightTree < 0 then
                        0

                    else
                        getIndent rightTree
            in
            [ drawPerson name (offsetX + bottomIndent) offsetY
            , drawSLine
                (offsetX + bottomIndent)
                offsetY
            , siblings
                |> List.indexedMap
                    (\i name_ ->
                        g []
                            [ drawPerson name_ (offsetX + bottomIndent + i + 1) offsetY
                            , drawSLine (offsetX + bottomIndent + i + 1) offsetY
                            ]
                    )
                |> g []
            , drawSHLine
                (offsetX + bottomIndent)
                (offsetX
                    + bottomIndent
                    + (if List.length siblings == 0 then
                        1

                       else
                        List.length siblings
                      )
                )
                offsetY
            , drawHLine
                (offsetX + topIndent + topLeftIndent + getSiblings leftTree)
                (offsetX + topIndent + getSize leftTree + topRightIndent)
                (offsetY - 1)
            , drawVLine
                (offsetX + topIndent + getSize leftTree)
                (offsetY - 1)
            , g [] <| render (offsetX + topIndent) (offsetY - 1) leftTree
            , g [] <| render (offsetX + topIndent + getSize leftTree) (offsetY - 1) rightTree
            ]

        Leaf (Person name siblings (Info size indent gen)) ->
            [ drawPerson name offsetX offsetY
            ]


drawPerson : String -> Int -> Int -> Svg msg
drawPerson name x_ y_ =
    g []
        [ if (String.length name |> modBy 2) == 0 then
            drawCircle x_ y_

          else
            drawSquare x_ y_
        , text_
            [ x (String.fromInt (10 + x_ * 20))
            , y (String.fromInt (19 + y_ * 20))
            , fontSize "4"
            , textAnchor "middle"
            ]
            [ text name ]
        ]


drawCircle : Int -> Int -> Svg msg
drawCircle x y =
    circle
        [ cx (String.fromInt (10 + x * 20))
        , cy (String.fromInt (10 + y * 20))
        , r "5"
        , stroke "black"
        , fill "transparent"
        ]
        []


drawSquare : Int -> Int -> Svg msg
drawSquare x_ y_ =
    rect
        [ x (String.fromInt (5 + x_ * 20))
        , y (String.fromInt (5 + y_ * 20))
        , width "10"
        , height "10"
        , stroke "black"
        , fill "transparent"
        ]
        []


drawHLine : Int -> Int -> Int -> Svg msg
drawHLine x1_ x2_ y =
    line
        [ x1 (String.fromInt (15 + x1_ * 20))
        , x2 (String.fromInt (5 + x2_ * 20))
        , y1 (String.fromInt (10 + y * 20))
        , y2 (String.fromInt (10 + y * 20))
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


drawVLine : Int -> Int -> Svg msg
drawVLine x y =
    line
        [ x1 (String.fromInt (x * 20))
        , x2 (String.fromInt (x * 20))
        , y1 (String.fromInt (10 + y * 20))
        , y2 (String.fromInt (20 + y * 20))
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


drawSLine : Int -> Int -> Svg msg
drawSLine x y =
    line
        [ x1 (String.fromInt (10 + x * 20))
        , x2 (String.fromInt (10 + x * 20))
        , y1 (String.fromInt (y * 20))
        , y2 (String.fromInt (5 + y * 20))
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


drawSHLine : Int -> Int -> Int -> Svg msg
drawSHLine x1_ x2_ y =
    line
        [ x1 (String.fromInt (10 + x1_ * 20))
        , x2 (String.fromInt (10 + x2_ * 20))
        , y1 (String.fromInt (y * 20))
        , y2 (String.fromInt (y * 20))
        , stroke "black"
        , strokeLinecap "round"
        ]
        []
