module Lawer.Sugar (app, arrow, (-->), ($$)) where

import Lawer.Type

infixr 9 `arrow`
arrow = Fa noname 

infixl 9 `app`
app = App

infixr 9 -->
(-->) = arrow

infixl 9 $$
($$) = app