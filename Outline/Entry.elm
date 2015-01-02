module Outline.Entry
  ( BaseValue(..), BaseZipper(..), Value, Zipper, Result
  , addToInbox
  , addInboxItem, promote, moveInto, missort, moveChildUp, moveChildDown, decoder, toJson, emptyEntry, entry, childZipper, textZipper, inboxZipper, toValue, textZipperAt, childZipperAt, inboxZipperAt, textValue, do, doEntry, descriptionZipper, firstInboxZipper
  ) where

import Html (Html, node, text)
import Html.Attributes (class)
import Core.String
import Core.Array
import String
import Json.Decode
import Json.Decode (..)
import Json.Encode
import Json.Encode as Json
import Core.Action (..)
import Debug
import Maybe
import List
import List (..)
import Maybe (..)

type BaseValue v = Entry {text: v, description: v, inbox: List (BaseValue v), children: List (BaseValue v)}

type BaseZipper v z
  = InText {text: z, description: v, inbox: List (BaseValue v), children: List (BaseValue v)}
  | InDescription {text: v, description: z, inbox: List (BaseValue v), children: List (BaseValue v)}
  | InInbox {text: v, description: v, inbox: Core.Array.Zipper (BaseValue v) (BaseZipper v z), children: List (BaseValue v)}
  | InChild {text: v, description: v, inbox: List (BaseValue v), children: Core.Array.Zipper (BaseValue v) (BaseZipper v z)}

emptyEntry = Entry {text="", description="", inbox=[], children=[]}
textEntry t = Entry {text=t, description="", inbox=[], children=[]}
entry t d i c = Entry {text=t, description=d, inbox=i, children=c}

type alias Value = BaseValue String
type alias Zipper = BaseZipper String Core.String.Zipper
type alias Result = ActionResult Value Zipper

toValue : Zipper -> Value
toValue z = case z of
  InText e -> Entry { e | text <- Core.String.toValue e.text }
  InDescription e -> Entry { e | description <- Core.String.toValue e.description }
  InInbox e -> Entry { e | inbox <- Core.Array.toValue toValue e.inbox }
  InChild e -> Entry { e | children <- Core.Array.toValue toValue e.children }

addToInbox : List String -> Value -> Value
addToInbox list v = case v of
  Entry e -> Entry { e | inbox <- (List.map textEntry list) ++ e.inbox }

textValue : Zipper -> String
textValue z = case toValue z of
  Entry e -> e.text

textZipper : Value -> Zipper
textZipper en = case en of Entry e -> InText { e | text <- Core.String.endZipper e.text }

textZipperAt : Int -> Value -> Zipper
textZipperAt i en = case en of Entry e -> InText { e | text <- Core.String.zipperAt i e.text }

descriptionZipper : Value -> Maybe Zipper
descriptionZipper v = case v of
  Entry e -> case e.description of
    "" -> Nothing
    _ -> Just <| InDescription { e | description <- Core.String.endZipper e.description }

childZipper : (List Value -> Core.Array.Zipper Value Zipper) -> Value -> Zipper
childZipper fn v = case v of Entry e -> InChild { e | children <- fn e.children }

childZipperAt : Int -> (Value -> Zipper) -> Value -> Zipper
childZipperAt i fn v = case v of Entry e -> InChild { e | children <- Core.Array.zipperAt i fn e.children }

inboxZipper : (List Value -> Core.Array.Zipper Value Zipper) -> Value -> Zipper
inboxZipper fn v = case v of Entry e -> InInbox { e | inbox <- fn e.inbox }

inboxZipperAt : Int -> (Value -> Zipper) -> Value -> Zipper
inboxZipperAt i fn v = case v of Entry e -> InInbox { e | inbox <- Core.Array.zipperAt i fn e.inbox }

findLastCursor : Value -> Zipper
findLastCursor en = case en of
  Entry e -> if
    | List.length e.children > 0 -> InChild { e | children <- Core.Array.lastZipper findLastCursor e.children }
    | List.length e.inbox > 0 -> InInbox { e | inbox <- Core.Array.lastZipper findLastCursor e.inbox }
    | otherwise -> InText { e | text <- Core.String.endZipper e.text }

addInboxItem__ : BaseValue a -> BaseValue a -> BaseValue a
addInboxItem__ s en = case en of Entry e -> Entry { e | inbox <- s :: e.inbox }

addInboxItem_ : Value -> Zipper -> Result
addInboxItem_ newEntry z = case z of
  InText e -> Update <| InInbox { e | text <- Core.String.toValue e.text, inbox <- Core.Array.firstZipper textZipper (newEntry :: e.inbox) }
  InInbox e -> Update <| InInbox { e | inbox <- Core.Array.firstZipper textZipper (newEntry :: Core.Array.toValue toValue e.inbox) }
  _ -> NoChange

addInboxItem = doEntry (addInboxItem_ emptyEntry)

dropAt : Int -> List a -> List a
dropAt i list = (take i list) ++ (drop (i+1) list)

changeAt : (a -> a) -> Int -> List a -> List a
changeAt fn i list = indexedMap (\n x -> if n == i then fn x else x) list

at : Int -> List a -> a
at i list = head <| drop i list

promote_ : Zipper -> Result
promote_ z = case z of
  InInbox e -> let i = Core.Array.active e.inbox
    in case Core.Array.remove textZipper e.inbox of
      Just newInbox -> Update <| InInbox { e
        | inbox <- newInbox
        , children <- (toValue i) :: e.children
        }
      Nothing -> Update <| InChild { e
        | inbox <- []
        , children <- Core.Array.zipper [] i e.children
        }
  _ -> NoChange

promote = doEntry promote_

unwrap en = case en of Entry e -> e
getInbox en = en |> unwrap |> .inbox

firstInboxZipper : Value -> Maybe Zipper
firstInboxZipper v = case v of
  Entry e -> case e.inbox of
    (head::tail) -> Just <| InInbox { e | inbox <- Core.Array.firstZipper textZipper e.inbox }
    _ -> Nothing

lastInboxZipper : Value -> Maybe Zipper
lastInboxZipper v = case v of
  Entry e -> case e.inbox of
    (head::tail) -> Just <| InInbox { e | inbox <- Core.Array.lastZipper textZipper e.inbox }
    _ -> Nothing

firstChildZipper : Value -> Maybe Zipper
firstChildZipper v = case v of
  Entry e -> Core.Array.firstZipperThat (\x -> Just <| textZipper x) e.children
    |> Maybe.map (\x -> InChild { e | children <- x })

lastChildZipper : Value -> Maybe Zipper
lastChildZipper v = case v of
  Entry e -> Core.Array.lastZipperThat (\x -> Just <| textZipper x) e.children
    |> Maybe.map (\x -> InChild { e | children <- x })

firstChildInboxZipper : Value -> Maybe Zipper
firstChildInboxZipper v = case v of
  Entry e -> Core.Array.firstZipperThat firstInboxZipper e.children
    |> Maybe.map (\x -> InChild { e | children <- x })

tryMap : (a -> Maybe b) -> List a -> b -> b
tryMap fn list default = case list of
  (head::tail) -> case fn head of
    Just result -> result
    Nothing -> tryMap fn tail default
  [] -> default

tryMap2 : (a -> Maybe b) -> List a -> Maybe b -> Maybe b
tryMap2 fn list default = case list of
  (head::tail) -> case fn head of
    Just result -> Just result
    Nothing -> tryMap2 fn tail default
  [] -> default

try : List (Maybe a) -> a -> a
try = tryMap identity

moveToInboxOfFirstChildOrNext : Value -> Result
moveToInboxOfFirstChildOrNext en = case en of
  Entry e -> Maybe.map Update (firstChildInboxZipper en) |> Maybe.withDefault EnterNext

appendToInboxOfChild : Int -> Value -> List Value -> List Value
appendToInboxOfChild n v children = Core.Array.mapAt n (\(Entry e) -> Entry { e | inbox <- Core.Array.prepend v e.inbox }) children

moveInto_ : Int -> Zipper -> Result
moveInto_ n z = case z of
  InInbox e -> if
    | n >= length e.children -> NoChange
    | otherwise -> case Core.Array.remove textZipper e.inbox of
      Just newZ -> Update <| InInbox { e | inbox <- newZ, children <- appendToInboxOfChild n (Core.Array.active e.inbox |> toValue) e.children }
      Nothing -> moveToInboxOfFirstChildOrNext (Entry { e | inbox <- [], children <- appendToInboxOfChild n (Core.Array.active e.inbox |> toValue) e.children })
  _ -> NoChange

moveInto n = doEntry (moveInto_ n)

missort_ : Zipper -> Result
missort_ z = case z of
  InChild e -> case Core.Array.active e.children of
    InInbox e' -> let item = Core.Array.active e'.inbox |> toValue
                      withNewInbox = { e | inbox <- Core.Array.prepend item e.inbox }
      in case Core.Array.remove textZipper e'.inbox of
        Just remaining -> Update <| InChild { withNewInbox | children <- Core.Array.update (InInbox { e' | inbox <- remaining }) e.children }
        Nothing -> Update <| inboxZipper (Core.Array.firstZipper textZipper) (Entry { withNewInbox | children <- Core.Array.toValue (\z -> case z of InInbox e'' -> Entry { e'' | inbox <- [] }) e.children })
    InChild e' -> case Core.Array.active e'.children of
      InChild _ -> NoChange
      InInbox _ -> NoChange
      _ -> let item = Core.Array.active e'.children |> toValue
               withNewInbox = { e | inbox <- Core.Array.prepend item e.inbox }
        in case Core.Array.remove textZipper e'.children of
          Just remaining -> Update <| InChild { withNewInbox | children <- Core.Array.update (InChild { e' | children <- remaining }) e.children }
          Nothing -> Update <| inboxZipper (Core.Array.firstZipper textZipper) (Entry { withNewInbox | children <- Core.Array.toValue (\z -> case z of InChild e'' -> Entry { e'' | children <- [] }) e.children })
    _ -> NoChange
  _ -> NoChange

missort = doEntry missort_

swap : Int -> a -> Int -> a -> List a -> List a
swap ai a bi b list = list |> indexedMap
  (\i x -> if
    | i == ai -> b
    | i == bi -> a
    | otherwise -> x)

swapChildren : (Core.Array.Zipper Value Zipper -> Maybe (Core.Array.Zipper Value Zipper)) -> Zipper -> Result
swapChildren fn z = case z of
  InChild e -> case Core.Array.active e.children of
    InChild _ -> NoChange
    InInbox _ -> NoChange
    _ -> case fn e.children of
      Just result -> Update <| InChild { e | children <- result }
      Nothing -> Update <| InChild e
  _ -> NoChange

moveChildUp = doEntry <| swapChildren Core.Array.moveUp
moveChildDown = doEntry <| swapChildren Core.Array.moveDown

-- TODO: give a better name--maybe "doAtDeepestChild"
doEntry : (Zipper -> Result) -> Zipper -> Result
doEntry action z = case z of
  InChild e -> case action z of
    NoChange -> case Core.Array.do toValue textZipper findLastCursor (doEntry action) e.children of
      Update newChildren -> Update <| InChild { e | children <- newChildren }
      NoChange -> NoChange
    x -> x
  _ -> action z

-- TODO: rewrite to utilize doEntry for the recursion
-- TODO: give a better name
do : (Core.String.Zipper -> Core.String.Result) -> Zipper -> Result
do stringAction z = case z of
  InText e -> case stringAction e.text of
    Update newZ -> Update <| InText { e | text <- newZ }
    Delete -> Delete
    NoChange -> NoChange
    Split left newZ right -> Split (List.map textEntry left) (InText { e | text <- newZ }) (List.map textEntry right)
    EnterPrev -> EnterPrev
    EnterNext -> try
      [ firstInboxZipper (toValue z) |> Maybe.map Update
      , firstChildZipper (toValue z) |> Maybe.map Update
      ] EnterNext
  InDescription e -> case stringAction e.description of
    Update newZ -> Update <| InDescription { e | description <- newZ }
    Delete -> Delete
    NoChange -> NoChange
  InInbox e -> case Core.Array.do toValue textZipper textZipper (do stringAction) e.inbox of
    Update newZ -> Update <| InInbox { e | inbox <- newZ }
    Delete -> Update <| InText { e | inbox <- [], text <- Core.String.endZipper e.text }
    NoChange -> NoChange
    EnterNext -> try
      [ firstChildZipper (toValue z) |> Maybe.map Update
      ] EnterNext
    EnterPrev -> (textZipper (toValue z) |> Update)
  InChild e -> case Core.Array.do toValue textZipper findLastCursor (do stringAction) e.children of
    Update newZ -> Update <| InChild { e | children <- newZ }
    Delete -> Update <| InText { e | children <- [], text <- Core.String.endZipper e.text }
    EnterNext -> EnterNext
    EnterPrev -> try
      [ lastInboxZipper (toValue z) |> Maybe.map Update
      ] (Update <| textZipper (toValue z))
    NoChange -> NoChange

---- RENDER

-- renderValue : Value -> Html
-- renderValue value = case value of
--   Entry e -> node "li" []
--     [ Core.String.renderValue e.text
--     , node "i" [] [ Core.String.renderValue e.description ]
--     , node "ul" [] <| map renderValue e.inbox
--     , node "ol" [] <| map renderValue e.children
--     ]
--
-- renderZipper : Zipper -> Html
-- renderZipper z = case z of
--   InText e -> node "li" []
--     [ Core.String.renderZipper e.text
--     , node "i" [] [ Core.String.renderValue e.description ]
--     , node "ul" [] <| map renderValue e.inbox
--     , node "ol" [] <| map renderValue e.children
--     ]
--   InDescription e -> node "li" []
--     [ Core.String.renderValue e.text
--     , node "i" [] [ Core.String.renderZipper e.description ]
--     , node "ul" [] <| map renderValue e.inbox
--     , node "ol" [] <| map renderValue e.children
--     ]
--   InInbox e -> node "li" []
--     [ Core.String.renderValue e.text
--     , node "i" [] [ Core.String.renderValue e.description ]
--     , node "ul" [] <| Core.Array.map renderValue renderZipper e.inbox
--     , node "ol" [] <| map renderValue e.children
--     ]
--   InChild e -> node "li" []
--     [ Core.String.renderValue e.text
--     , node "i" [] [ Core.String.renderValue e.description ]
--     , node "ul" [] <| map renderValue e.inbox
--     , node "ol" [] <| Core.Array.map renderValue renderZipper e.children
--     ]


---- JSON

toJson : Value -> Json.Value
toJson entry = case entry of
  Entry e -> Json.Encode.object
    [ ("text", Json.Encode.string e.text)
    , ("description", Json.Encode.string e.description)
    , ("inbox", List.map toJson e.inbox |> Json.Encode.list)
    , ("children", List.map toJson e.children |> Json.Encode.list)
    ]

decoder : Json.Decode.Decoder (BaseValue String)
decoder =
  Json.Decode.map (\e -> Entry e) <|
  object4 (\t d i c -> {text=t, description=d, inbox=i, children=c})
    ("text" := Json.Decode.string)
    ("description" := Json.Decode.string)
    ("inbox" := Json.Decode.list (lazy (\_ -> decoder)))
    ("children" := Json.Decode.list (lazy (\_ -> decoder)))

lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  Json.Decode.customDecoder value
      (\js -> Json.Decode.decodeValue (thunk ()) js)
