{- | Runtime detection of which terminal graphics protocol to use for
notation rendering.

No single protocol covers all target terminals: Kitty implements only its own
graphics protocol (no Sixel), while iTerm2 supports Sixel but not the Kitty
protocol. WezTerm and Ghostty support the Kitty protocol; iTerm2, foot, and
xterm support Sixel. We therefore pick a backend at runtime and share the same
rendered image buffer between them.

Detection is env-based (cheap, side-effect-free beyond reading the
environment). A terminal Device Attributes (DA) probe for Sixel is a future
refinement; for now an explicit @TERMTAB_GRAPHICS@ override covers the cases
env heuristics miss.
-}
module Termtab.Graphics.Detect (
    GraphicsProtocol (..),
    detectProtocol,
    classifyProtocol,
    classifyProtocolReason,
    DetectionInfo (..),
    detectProtocolInfo,
) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Environment (lookupEnv)

data GraphicsProtocol
    = -- | Kitty graphics protocol (Kitty, WezTerm, Ghostty)
      Kitty
    | -- | Sixel (iTerm2, WezTerm, foot, xterm, mlterm, contour)
      Sixel
    | -- | No graphics support: fall back to the Unicode text staff
      TextOnly
    deriving (Show, Eq)

{- | Detect the graphics protocol from the environment.

Precedence:

1. @TERMTAB_GRAPHICS@ override (@kitty@ | @sixel@ | @text@)
2. Kitty-protocol terminals (@KITTY_WINDOW_ID@, @TERM=*kitty*@, WezTerm, Ghostty)
3. Sixel terminals (iTerm2, @TERM=*sixel*@)
4. 'TextOnly'
-}
detectProtocol :: IO GraphicsProtocol
detectProtocol = do
    override <- lookupEnv "TERMTAB_GRAPHICS"
    term <- lookupEnv "TERM"
    termProgram <- lookupEnv "TERM_PROGRAM"
    kittyId <- lookupEnv "KITTY_WINDOW_ID"
    pure (classifyProtocol override term termProgram kittyId)

{- | Pure classification core (unit-testable).

Arguments, in order: @TERMTAB_GRAPHICS@, @TERM@, @TERM_PROGRAM@,
@KITTY_WINDOW_ID@.
-}
classifyProtocol ::
    Maybe String -> Maybe String -> Maybe String -> Maybe String -> GraphicsProtocol
classifyProtocol override term termProgram kittyId =
    fst (classifyProtocolReason override term termProgram kittyId)

{- | Like 'classifyProtocol' but also returns a human-readable explanation of
which signal drove the decision (used by @--health@).
-}
classifyProtocolReason ::
    Maybe String ->
    Maybe String ->
    Maybe String ->
    Maybe String ->
    (GraphicsProtocol, String)
classifyProtocolReason override term termProgram kittyId =
    case fmap (map toLower) override of
        Just "kitty" -> (Kitty, "TERMTAB_GRAPHICS override set to \"kitty\"")
        Just "sixel" -> (Sixel, "TERMTAB_GRAPHICS override set to \"sixel\"")
        Just "text" -> (TextOnly, "TERMTAB_GRAPHICS override set to \"text\"")
        _
            | Just _ <- kittyId ->
                (Kitty, "KITTY_WINDOW_ID is set (Kitty graphics protocol)")
            | "kitty" `isInfixOf` term' ->
                (Kitty, "TERM contains \"kitty\"")
            | "ghostty" `isInfixOf` term' ->
                (Kitty, "TERM contains \"ghostty\"")
            | "wezterm" `isInfixOf` prog' ->
                (Kitty, "TERM_PROGRAM is WezTerm (supports the Kitty graphics protocol)")
            | "ghostty" `isInfixOf` prog' ->
                (Kitty, "TERM_PROGRAM is Ghostty")
            | "iterm" `isInfixOf` prog' ->
                (Sixel, "TERM_PROGRAM is iTerm (supports Sixel)")
            | "sixel" `isInfixOf` term' ->
                (Sixel, "TERM advertises Sixel")
            | "mlterm" `isInfixOf` term' ->
                (Sixel, "TERM is mlterm (supports Sixel)")
            | "foot" `isInfixOf` term' ->
                (Sixel, "TERM is foot (supports Sixel)")
            | otherwise ->
                (TextOnly, "no graphics-capable terminal detected; using the Unicode text staff")
  where
    term' = maybe "" (map toLower) term
    prog' = maybe "" (map toLower) termProgram

{- | The full result of graphics detection, including the raw environment
signals that were consulted. Rendered by @--health@.
-}
data DetectionInfo = DetectionInfo
    { diProtocol :: GraphicsProtocol
    , diReason :: String
    , diOverride :: Maybe String
    -- ^ @TERMTAB_GRAPHICS@
    , diTerm :: Maybe String
    -- ^ @TERM@
    , diTermProgram :: Maybe String
    -- ^ @TERM_PROGRAM@
    , diKittyWindowId :: Maybe String
    -- ^ @KITTY_WINDOW_ID@
    }
    deriving (Show, Eq)

-- | Detect the protocol and capture the environment signals behind the decision.
detectProtocolInfo :: IO DetectionInfo
detectProtocolInfo = do
    override <- lookupEnv "TERMTAB_GRAPHICS"
    term <- lookupEnv "TERM"
    termProgram <- lookupEnv "TERM_PROGRAM"
    kittyId <- lookupEnv "KITTY_WINDOW_ID"
    let (proto, reason) = classifyProtocolReason override term termProgram kittyId
    pure
        DetectionInfo
            { diProtocol = proto
            , diReason = reason
            , diOverride = override
            , diTerm = term
            , diTermProgram = termProgram
            , diKittyWindowId = kittyId
            }
