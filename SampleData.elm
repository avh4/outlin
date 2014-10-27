module SampleData where

import Outline.Entry as Entry

template = Entry.Entry { text="Tasks (LOADING...)", description="", inbox=["sdfs","sdfsd"], children=[
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
