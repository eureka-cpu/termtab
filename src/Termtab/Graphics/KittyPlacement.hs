{- | Kitty Unicode placeholder support.

The Kitty graphics protocol can display a "virtual" image (transmitted with
@U=1@, see "Termtab.Graphics.Kitty".'encodeVirtual') through ordinary terminal
cells, which is what lets an image live inside a cell-based TUI like brick/vty.

Each placeholder cell holds the placeholder code point U+10EEEE. The image id is
carried in the cell's foreground color, and the cell's (row, column) within the
image is carried by combining diacritics appended to the placeholder character.
Kitty auto-increments the column for bare placeholder cells, so we only need to
stamp the row (and column 0) on the first cell of each line.
-}
module Termtab.Graphics.KittyPlacement (
    placeholderChar,
    rowColumnDiacritics,
    placeholderRows,
    foregroundIdSGR,
) where

import Data.Bits (shiftR, (.&.))

-- | The Kitty image-placeholder code point.
placeholderChar :: Char
placeholderChar = '\x10EEEE'

{- | Prefix of Kitty's rowcolumn diacritic table: @rowColumnDiacritics !! n@ is
the combining character that encodes the value @n@. This prefix covers the row
counts we need (staff height in cells).
-}
rowColumnDiacritics :: [Char]
rowColumnDiacritics =
    map
        toEnum
        [ 0x0305
        , 0x030D
        , 0x030E
        , 0x0310
        , 0x0312
        , 0x033D
        , 0x033E
        , 0x033F
        , 0x0346
        , 0x034A
        , 0x034B
        , 0x034C
        , 0x0350
        , 0x0351
        , 0x0352
        , 0x0357
        , 0x035B
        , 0x0363
        , 0x0364
        , 0x0365
        , 0x0366
        , 0x0367
        , 0x0368
        , 0x0369
        , 0x036A
        , 0x036B
        , 0x036C
        , 0x036D
        , 0x036E
        , 0x036F
        , 0x0483
        , 0x0484
        ]

-- | The combining diacritic encoding value @n@ (n must be within the table).
diacritic :: Int -> Char
diacritic n = rowColumnDiacritics !! n

{- | Placeholder cell text for a @cols@×@rows@ image grid, one 'String' per row.
The first cell of each row carries the row diacritic and column-0 diacritic;
the remaining cells are bare placeholders whose columns Kitty auto-increments.
All cells must be drawn with 'foregroundIdSGR' as their foreground color.
-}
placeholderRows :: Int -> Int -> [String]
placeholderRows cols rows =
    [placeholderRow r | r <- [0 .. rows - 1]]
  where
    placeholderRow r =
        [placeholderChar, diacritic r, diacritic 0]
            ++ replicate (max 0 (cols - 1)) placeholderChar

{- | Truecolor SGR that stores the image id in the foreground color (Kitty reads
the 24-bit value as the id). Not visible — the placeholder shows image pixels.
-}
foregroundIdSGR :: Int -> String
foregroundIdSGR i =
    let r = (i `shiftR` 16) .&. 0xFF
        g = (i `shiftR` 8) .&. 0xFF
        b = i .&. 0xFF
     in "\ESC[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"
