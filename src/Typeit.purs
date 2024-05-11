module Typeit
  ( AnimationEffectTiming
  , AnimationEffectTimingFillMode
  , AnimationEffectTimingPlaybackDirection
  , AnimationKeyFrame
  , CursorAnimationOptions
  , CursorOptions
  , Typeit
  , TypeitOptions
  , TypeitTo
  , afterComplete
  , afterStep
  , afterString
  , alternate
  , alternateReverse
  , animation
  , animationEffectTimingDelay
  , animationKeyFrameEasing
  , auto
  , autoPause
  , autoPauseDelay
  , backwards
  , beforeStep
  , beforeString
  , both
  , breakLines
  , cursor
  , cursorChar
  , cursorSpeed
  , delay
  , deleteSpeed
  , destroy
  , direction
  , duration
  , easing
  , end
  , endDelay
  , fill
  , forwards
  , frames
  , go
  , html
  , instant
  , iterationStart
  , iterations
  , lifeLike
  , loop
  , loopDelay
  , nextStringDelay
  , none
  , normal
  , offset
  , opacity
  , options
  , playbackRate
  , reverse
  , speed
  , start
  , startDelay
  , startDelete
  , strings
  , to
  , typeit
  , typeitGo
  , waitUntilVisible
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Contravariant (cmap)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Options as Opt
import Effect (Effect)
import Foreign (Foreign)
import Untagged.Union (type (|+|), asOneOf)
import Web.DOM (Element)

data TypeitOptions

data Typeit

data CursorOptions

autoPause :: Opt.Option CursorOptions Boolean
autoPause = Opt.opt "autoPause"

autoPauseDelay :: Opt.Option CursorOptions Number
autoPauseDelay = Opt.opt "autoPauseDelay"

animation :: Opt.Option CursorOptions (Opt.Options CursorAnimationOptions)
animation = Opt.opt "animation"

data CursorAnimationOptions

data AnimationKeyFrame

animationKeyFrameEasing :: Opt.Option AnimationKeyFrame (String |+| Array String)
animationKeyFrameEasing = Opt.opt "easing"

offset :: Opt.Option AnimationKeyFrame (Either Number (Array (Maybe Number)))
offset = cmap
  ( \i -> case i of
      Left n -> asOneOf n :: Number |+| Array (Nullable Number)
      Right x -> asOneOf $ map toNullable x
  ) $ Opt.opt  "offset"

opacity :: Opt.Option AnimationKeyFrame (Number |+| Array Number)
opacity = Opt.opt "opacity"

frames :: Opt.Option CursorAnimationOptions (Array (Opt.Options AnimationKeyFrame))
frames = Opt.opt "frames"


newtype AnimationEffectTimingPlaybackDirection = AnimationEffectTimingPlaybackDirection String
newtype AnimationEffectTimingFillMode = AnimationEffectTimingFillMode String

normal :: AnimationEffectTimingPlaybackDirection
normal = AnimationEffectTimingPlaybackDirection "normal"

reverse :: AnimationEffectTimingPlaybackDirection
reverse = AnimationEffectTimingPlaybackDirection "reverse"

alternate :: AnimationEffectTimingPlaybackDirection
alternate = AnimationEffectTimingPlaybackDirection "alternate"

alternateReverse :: AnimationEffectTimingPlaybackDirection
alternateReverse = AnimationEffectTimingPlaybackDirection "alternate-reverse"

none :: AnimationEffectTimingFillMode
none = AnimationEffectTimingFillMode "none"

forwards :: AnimationEffectTimingFillMode
forwards = AnimationEffectTimingFillMode "forwards"

backwards :: AnimationEffectTimingFillMode
backwards = AnimationEffectTimingFillMode "backwards"

both :: AnimationEffectTimingFillMode
both = AnimationEffectTimingFillMode "both"

auto :: AnimationEffectTimingFillMode
auto = AnimationEffectTimingFillMode "auto"

data AnimationEffectTiming

animationEffectTimingDelay :: Opt.Option AnimationEffectTiming Number
animationEffectTimingDelay = Opt.opt "delay"

direction :: Opt.Option AnimationEffectTiming AnimationEffectTimingPlaybackDirection
direction = Opt.opt "direction"

duration :: Opt.Option AnimationEffectTiming Number
duration = Opt.opt "duration"

easing :: Opt.Option AnimationEffectTiming String
easing = Opt.opt "easing"

endDelay :: Opt.Option AnimationEffectTiming Number
endDelay = Opt.opt "endDelay"

fill :: Opt.Option AnimationEffectTiming AnimationEffectTimingFillMode
fill = Opt.opt "fill"

iterationStart :: Opt.Option AnimationEffectTiming Number
iterationStart = Opt.opt "iterationStart"

iterations :: Opt.Option AnimationEffectTiming Number
iterations = Opt.opt "iterations"

playbackRate :: Opt.Option AnimationEffectTiming Number
playbackRate = Opt.opt "playbackRate"

options :: Opt.Option CursorAnimationOptions (Opt.Options AnimationEffectTiming)
options = Opt.opt "options"

newtype TypeitTo = TypeitTo String

start :: TypeitTo
start = TypeitTo "START"

end :: TypeitTo
end = TypeitTo "END"

to :: Opt.Option TypeitOptions TypeitTo
to = Opt.opt "to"

instant :: Opt.Option TypeitOptions Boolean
instant = Opt.opt "instant"

delay :: Opt.Option TypeitOptions Number
delay = Opt.opt "delay"

breakLines :: Opt.Option TypeitOptions Boolean
breakLines = Opt.opt "breakLines"

cursorChar :: Opt.Option TypeitOptions String
cursorChar = Opt.opt "cursorChar"

cursor :: Opt.Option TypeitOptions (Boolean |+| Opt.Options CursorOptions)
cursor = Opt.opt "cursor"

cursorSpeed :: Opt.Option TypeitOptions Number
cursorSpeed = Opt.opt "cursorSpeed"

deleteSpeed :: Opt.Option TypeitOptions (Maybe Number)
deleteSpeed = cmap toNullable (Opt.opt "deleteSpeed")

html :: Opt.Option TypeitOptions Boolean
html = Opt.opt "html"

lifeLike :: Opt.Option TypeitOptions Boolean
lifeLike = Opt.opt "lifeLike"

loop :: Opt.Option TypeitOptions Boolean
loop = Opt.opt "loop"

loopDelay :: Opt.Option TypeitOptions (Array Number |+| Number)
loopDelay = Opt.opt "loopDelay"

nextStringDelay :: Opt.Option TypeitOptions (Array Number |+| Number)
nextStringDelay = Opt.opt "nextStringDelay"

speed :: Opt.Option TypeitOptions Number
speed = Opt.opt "speed"

startDelay :: Opt.Option TypeitOptions Number
startDelay = Opt.opt "startDelay"

startDelete :: Opt.Option TypeitOptions Boolean
startDelete = Opt.opt "startDelete"

strings :: Opt.Option TypeitOptions (Array String |+| String)
strings = Opt.opt "strings"

waitUntilVisible :: Opt.Option TypeitOptions Boolean
waitUntilVisible = Opt.opt "waitUntilVisible"

afterComplete :: Opt.Option TypeitOptions (Typeit -> Effect Unit)
afterComplete = Opt.opt "afterComplete"

beforeStep :: Opt.Option TypeitOptions (Typeit -> Effect Unit)
beforeStep = Opt.opt "beforeStep"

beforeString :: Opt.Option TypeitOptions (Typeit -> Array Char -> Effect Unit)
beforeString = Opt.opt "beforeString"

afterStep :: Opt.Option TypeitOptions (Typeit -> Effect Unit)
afterStep = Opt.opt "afterStep"

afterString :: Opt.Option TypeitOptions (Typeit -> Array Char -> Effect Unit)
afterString = Opt.opt "afterString"

foreign import typeitGoImpl :: Element -> Foreign -> Effect Unit

typeitGo :: Element -> Opt.Options TypeitOptions -> Effect Unit
typeitGo el opts = typeitGoImpl el (Opt.options opts)

foreign import typeitImpl :: Element -> Foreign -> Effect Typeit

typeit :: Element -> Opt.Options TypeitOptions -> Effect Typeit
typeit el opts = typeitImpl el (Opt.options opts)

foreign import go :: Typeit -> Effect Unit
foreign import destroy :: Typeit -> Boolean -> Effect Unit