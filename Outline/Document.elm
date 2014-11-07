module Outline.Document where

import Outline.Entry as Entry

type Value = Entry.Entry
type Zipper = { value: Value, selection: Entry.Cursor }