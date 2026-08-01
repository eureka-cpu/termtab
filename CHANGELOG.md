# Revision history for termtab

## Unreleased

* Standard notation is now rendered as a real image from the Bravura (SMuFL)
  music font instead of relying on the terminal font for glyphs. A five-line
  staff, treble clef, bar lines, and noteheads/rests are rasterized via FreeType
  and displayed above the tab using the Kitty graphics protocol, column-aligned
  with the tab and drawn in the terminal's foreground color.
* Terminal graphics protocol is auto-detected (Kitty now; Sixel planned);
  terminals without graphics support fall back to the Unicode text staff.
* New `termtab --health` reports the detected protocol, the environment signals
  behind it, and a live Bravura load/rasterize check.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
