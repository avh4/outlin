module Core.Action where

type Action com val cur = {
  valueFn:(com -> val -> cur -> val),
  curFn:(com -> val -> cur -> cur)
  }
