module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Errors exposing (Errors)
import Html exposing (..)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, string)
import List as L exposing (..)
import Parser exposing ((|.), (|=), Parser, number)
import Set


type Tree a
    = One (List a)
    | Many (List (Tree a))


classifyOn : (a -> comparable) -> List a -> List (List a)
classifyOn field =
    let
        classification a b =
            field a == field b
    in
    classifyBy classification


classifyBy : (a -> a -> Bool) -> List a -> List (List a)
classifyBy eq l =
    case l of
        [] ->
            []

        x :: xs ->
            let
                neq a b =
                    not (eq a b)
            in
            (x :: L.filter (eq x) xs) :: classifyBy eq (L.filter (neq x) xs)


clasify : (a -> comparable) -> Tree a -> Tree a
clasify field tree =
    case tree of
        One leaves ->
            Many (L.map One (classifyOn field leaves))

        Many ts ->
            Many (L.map (clasify field) ts)


type alias Card =
    { color : String, cmc : String, name : String }


unlines : List String -> String
unlines ls =
    foldr (\x y -> x ++ "\n" ++ y) "" ls


pre : List String -> List String
pre l =
    let
        prefix =
            if length l < 2 then
                singleton "──"

            else
                singleton "┌─" ++ repeat (length l - 2) "│ " ++ singleton "└─"
    in
    zipWith (++) prefix l


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f als bls =
    let
        go =
            zipWith f
    in
    case ( als, bls ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            f x y :: go xs ys


renderTree : (a -> String) -> Tree a -> List String
renderTree show tree =
    case tree of
        One leaves ->
            L.map show leaves
                |> pre

        Many ts ->
            intersperse (singleton "") (L.map (renderTree show) ts)
                |> concat
                |> pre


intersperse : a -> List a -> List a
intersperse sep ls =
    case ls of
        [] ->
            []

        x :: xs ->
            x :: prependToAll sep xs


prependToAll : a -> List a -> List a
prependToAll sep ls =
    case ls of
        [] ->
            []

        x :: xs ->
            sep :: x :: prependToAll sep xs


dt : List Card -> Tree Card
dt deck =
    One deck
        |> clasify .color
        |> identity


showCard : Card -> String
showCard =
    .name


deckExample : String
deckExample =
    unlines
        [ "4 Counterspell"
        , "20 Island"
        , "4 Ponder"
        , "4 Preordain"
        ]


view : Model -> Html Msg
view m =
    div []
        [ if Errors.isEmpty m.errors then
            span [] []

          else
            div []
                [ h3 [] [ text "errors" ]
                , code [] [ text <| unlines <| Errors.toStrings m.errors ]
                , hr [] []
                ]
        , viewInner m
        ]


viewInner : Model -> Html Msg
viewInner model =
    case model.status of
        Editting ->
            form [ onSubmit LoadCards ]
                [ textarea [ placeholder deckExample, value model.content, onInput Change ] []
                , button [ type_ "submit" ] [ text "Load" ]
                ]

        Showing ->
            div []
                [ button [ onClick Edit ] [ text "<< Edit" ]
                , showTree (lookupCards model.db model.deck)
                ]


showTree : List Card -> Html msg
showTree deck =
    renderTree showCard (dt deck)
        |> unlines
        |> text
        |> singleton
        |> Html.pre []


type Status
    = Editting
    | Showing


type alias Model =
    { content : String
    , errors : Errors
    , status : Status
    , db : Dict CardName Card
    , deck : List CardName
    }


type Msg
    = GotCard (Result Http.Error ( CardName, Card ))
    | Change String
    | LoadCards
    | Edit


type alias MultiResult error value =
    { ok : value
    , err : error
    }


mapMR : (error1 -> error2) -> (value1 -> value2) -> MultiResult error1 value1 -> MultiResult error2 value2
mapMR fErr fVal mr =
    { ok = fVal mr.ok, err = fErr mr.err }


accumulate : List (Result e a) -> MultiResult (List e) (List a)
accumulate xs =
    { ok = List.filterMap Result.toMaybe xs
    , err = List.filterMap errToMaybe xs
    }


errToMaybe : Result e a -> Maybe e
errToMaybe mb =
    case mb of
        Err e ->
            Just e

        _ ->
            Nothing


type alias CardName =
    String


parseDeck : String -> MultiResult (List String) (List CardName)
parseDeck content =
    let
        cards =
            String.split "\n" content
                |> List.filter (not << String.isEmpty)
                |> List.map (Parser.run cardsParser)
                |> accumulate
    in
    mapMR (List.map deadEndsToString) multiply cards


multiply : List ( Int, a ) -> List a
multiply =
    uncurry List.repeat
        |> List.concatMap


uncurry : (a -> b -> c) -> (( a, b ) -> c)
uncurry f ( a, b ) =
    f a b


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString =
    Debug.toString


httpErrorsToString : Http.Error -> String
httpErrorsToString =
    Debug.toString


whitespace : Parser ()
whitespace =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')


cardsParser : Parser ( Int, String )
cardsParser =
    Parser.succeed Tuple.pair
        |. whitespace
        |= Parser.oneOf
            [ Parser.int
            , Parser.succeed 1
            ]
        |. Parser.oneOf
            [ Parser.symbol "x"
            , Parser.succeed ()
            ]
        |. whitespace
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> c /= '$')
           )
        |. Parser.end


loadCard : CardName -> Cmd Msg
loadCard name =
    Http.get
        { url = "https://api.scryfall.com/cards/named?fuzzy=" ++ name
        , expect = Http.expectJson (GotCard << Result.map (Tuple.pair name)) cardDecoder
        }


cardDecoder : Decoder Card
cardDecoder =
    Json.Decode.map3 (\color_ cmc_ name_ -> { color = color_, cmc = cmc_, name = name_ })
        (Json.Decode.map (Maybe.withDefault "") <| Json.Decode.maybe <| Json.Decode.field "colors" colorsDecode)
        (Json.Decode.map String.fromInt <| Json.Decode.field "cmc" Json.Decode.int)
        (Json.Decode.field "name" string)


colorsDecode : Decoder String
colorsDecode =
    Json.Decode.list string
        |> Json.Decode.map (List.sort >> String.concat)


subtract : List comparable -> List comparable -> List comparable
subtract a b =
    List.filter (\x -> not <| List.member x b) a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.status ) of
        ( Change c, Editting ) ->
            ( { model | content = c }, Cmd.none )

        ( Edit, Showing ) ->
            ( { model | status = Editting }, Cmd.none )

        ( LoadCards, Editting ) ->
            let
                res =
                    parseDeck model.content

                errors =
                    if List.isEmpty res.err then
                        Errors.init

                    else
                        List.foldl Errors.add Errors.init res.err

                missingCards =
                    subtract res.ok (Dict.keys model.db)
            in
            ( { model
                | status = Showing
                , errors = errors
                , deck = res.ok
              }
            , Cmd.batch <| List.map loadCard <| unique missingCards
            )

        ( GotCard res, _ ) ->
            case res of
                Err e ->
                    ( { model | errors = Errors.add (httpErrorsToString e) model.errors }, Cmd.none )

                Ok ( name, card ) ->
                    ( { model | db = Dict.insert name card model.db }, Cmd.none )

        _ ->
            ( { model | errors = Errors.add "Shoudnt have gootten a message in this state" model.errors }, Cmd.none )


unique : List comparable -> List comparable
unique =
    Set.fromList >> Set.toList


lookupCards : Dict CardName Card -> List CardName -> List Card
lookupCards db cards =
    List.filterMap (\card -> Dict.get card db) cards


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { content = deckExample
      , errors = Errors.init
      , status = Editting
      , db = Dict.empty
      , deck = []
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
