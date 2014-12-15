module SampleData where

import Outline.Entry as Entry
import Outline.RichText.Span.Model (Type(..))

outline = Entry.Entry { text="Tasks (LOADING...)", description="", inbox=
  [ Entry.Entry { text="sdfs", description="", inbox=[], children=[] }
  , Entry.Entry { text="Bacon", description="", inbox=[], children=[] }
  , Entry.Entry { text="Freddy", description="", inbox=[], children=[] }
  , Entry.Entry { text="Jo-jo", description="", inbox=[], children=[] }
  , Entry.Entry { text="Fish", description="", inbox=[], children=[] }
  , Entry.Entry { text="Fries", description="", inbox=[], children=[] }
  , Entry.Entry { text="Pork", description="", inbox=[], children=[] }
  , Entry.Entry { text="Lacey", description="", inbox=[], children=[] }
  , Entry.Entry { text="quux", description="", inbox=[], children=[] }
  , Entry.Entry { text="sdfsd", description="", inbox=[], children=[] }
  , Entry.Entry { text="Billy", description="", inbox=[], children=[] }
  , Entry.Entry { text="Camilla", description="", inbox=[], children=[] }
  , Entry.Entry { text="baz", description="", inbox=[], children=[] }
  ], children=[
  Entry.Entry { text="By time (deadline)", description="", inbox=[], children=[
    Entry.Entry { text="daily", description="", inbox=[], children=[] },
    Entry.Entry { text="weekly", description="", inbox=[], children=[] },
    Entry.Entry { text="waiting on", description="", inbox=[], children=[] },
    Entry.Entry { text="monthly", description="", inbox=[], children=[] },
    Entry.Entry { text="yearly, etc.", description="", inbox=[], children=[] }
  ] },
  Entry.Entry { text="Habits", description="", inbox=[], children=[
    Entry.Entry { text="daily", description="", inbox=[], children=[] },
    Entry.Entry { text="weekly", description="", inbox=[], children=[] },
    Entry.Entry { text="monthly", description="", inbox=[], children=[] }
  ] },
  Entry.Entry { text="By priorty", description="", inbox=[], children=[] },
  Entry.Entry { text="By project", description="", inbox=[], children=[] }
  ] }

template =
  { scratch=
    [ [(Normal, "Scratch 3\nThis is "), (Bold, "important"), (Normal, ".")]
    , [(Normal, "Scratch 2")]
    ]
  , outline=outline
  }
