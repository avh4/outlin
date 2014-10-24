module Core.Action where

type Action val cur = {
  valueFn:(val -> cur -> val),
  curFn:(val -> cur -> cur)
  }
