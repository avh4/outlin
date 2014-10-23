module SampleData where

import Outline.Entry as Entry

template = Entry.Entry { text="Tasks", description="", children=[
    Entry.Entry { text="Inbox", description="", children=[] },
    Entry.Entry { text="By time (deadline)", description="", children=[
      Entry.Entry { text="daily", description="", children=[] },
      Entry.Entry { text="weekly", description="", children=[] },
      Entry.Entry { text="waiting on", description="", children=[] },
      Entry.Entry { text="monthly", description="", children=[] },
      Entry.Entry { text="yearly, etc.", description="", children=[] }
    ] },
    Entry.Entry { text="Habits", description="", children=[
      Entry.Entry { text="daily", description="", children=[] },
      Entry.Entry { text="weekly", description="", children=[] },
      Entry.Entry { text="monthly", description="", children=[] }
    ] },
    Entry.Entry { text="By priorty", description="", children=[] },
    Entry.Entry { text="By project", description="", children=[] }
    ] }
