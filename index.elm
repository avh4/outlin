import Html (Html, text, node, toElement)

type Document = [Block]

data Block =
  Heading Int Span | -- level, content
  Paragraph Span | -- content
  CodeBlock (Maybe String) String -- language, content

data Span =
  Plain String

toHtml : Document -> Html
toHtml d = node "div" [] <| map blockToHtml d

blockToHtml : Block -> Html
blockToHtml b = case b of
  Heading 1 s -> node "h1" [] [ spanToHtml s ]
  Heading _ s -> node "h2" [] [ spanToHtml s ]
  Paragraph s -> node "p" [] [ spanToHtml s ]
  CodeBlock _ s -> node "code" [] [ text s ]

spanToHtml : Span -> Html
spanToHtml span = case span of
  Plain s -> text s

-- -- -- --

exampleDoc = [
  Heading 1 (Plain "Welcome to Elm"),
  Paragraph (Plain "A functional reactive language for interactive applications"),
  CodeBlock (Just "elm") "main = asText \"Hello World\""
  ]

main = toElement 800 600 <| toHtml exampleDoc