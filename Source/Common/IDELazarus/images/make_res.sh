#!/bin/sh
# Makes the palette icons of the Lazarus design-time packages dclBold and
# dclBoldZeos (#117):
# converts the Delphi palette bitmaps in Images/Components to PNG, with the
# colour of the bottom left pixel transparent as on the Delphi palette, and
# packs them with Lazarus's lazres into ../boldlazarusreg.res, which the unit
# BoldLazarusReg links in. The resources are named after the classes.
# The same for TBoldDatabaseAdapterZeos, in Source/Persistence/Zeos/IDELazarus.
#
# The PNG files and the .res are checked in; run this only when a component
# is added to BoldLazarusReg (add it to the list below) or a bitmap changes.
# Needs python3 with Pillow, and lazres (in the Lazarus tools).
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
BITMAPS="$HERE/../../../../Images/Components"
# TBoldFilteredHandle has no bitmap, in Delphi it shows the default icon too
CLASSES="TBoldModel TBoldSystemHandle TBoldSystemTypeInfoHandle TBoldExpressionHandle
  TBoldDerivedHandle TBoldVariableHandle TBoldListHandle TBoldCursorHandle
  TBoldReferenceHandle TBoldOclVariables TBoldUnloaderHandle
  TBoldPersistenceHandleDB TBoldPersistenceHandleFileXML
  TBoldFilter TBoldComparer TBoldPlaceableSubscriber TBoldExceptionHandler TBoldDataSet"

to_png() {
  python3 - "$1" "$2" <<'PY'
import sys
from PIL import Image
image = Image.open(sys.argv[1]).convert("RGBA")
width, height = image.size
transparent = image.getpixel((0, height - 1))[:3]
pixels = image.load()
for y in range(height):
    for x in range(width):
        if pixels[x, y][:3] == transparent:
            pixels[x, y] = transparent + (0,)
image.save(sys.argv[2], optimize=True)
PY
}

cd "$HERE"
FILES=""
for c in $CLASSES; do
  to_png "$BITMAPS/$c.bmp" "$c.png"
  FILES="$FILES $c.png"
done
# shellcheck disable=SC2086
lazres ../boldlazarusreg.res $FILES

# The icons of the OCL editor's lists (attribute, class, single link, multi
# link, operation), in dclBold's property editor of Expression: the image
# list of Delphi's editor, Source/ObjectSpace/UtilsGUI/boldoclpropeditor.dfm,
# 16x16 each, with its mask as transparency.
python3 - "$HERE/../../../ObjectSpace/UtilsGUI/boldoclpropeditor.dfm" "$HERE" <<'PY'
import io, re, sys
from PIL import Image
text = open(sys.argv[1], encoding="latin-1").read()
start = text.index("Bitmap = {", text.index("object ImageList1")) + len("Bitmap = {")
data = bytes.fromhex(re.sub(r"\s", "", text[start:text.index("}", start)]))
first = data.index(b"BM")
second = data.index(b"BM", first + 2)
image = Image.open(io.BytesIO(data[first:second])).convert("RGBA")
mask = Image.open(io.BytesIO(data[second:])).convert("L")
names = ["BoldOclAttribute", "BoldOclClass", "BoldOclSingleLink", "BoldOclMultiLink", "BoldOclOperation"]
for index, name in enumerate(names):
    box = (index % 4 * 16, index // 4 * 16, index % 4 * 16 + 16, index // 4 * 16 + 16)
    icon = image.crop(box)
    # In the mask, white is outside the icon
    icon.putalpha(mask.crop(box).point(lambda v: 0 if v else 255))
    icon.save(f"{sys.argv[2]}/{name}.png", optimize=True)
PY
lazres ../boldoclexpressionpropedit.res BoldOclAttribute.png BoldOclClass.png BoldOclSingleLink.png \
  BoldOclMultiLink.png BoldOclOperation.png

# TBoldDatabaseAdapterZeos, in dclBoldZeos: the icon of the general database adapter
ZEOSREG="$HERE/../../../Persistence/Zeos/IDELazarus"
to_png "$BITMAPS/TBoldDatabaseAdapterDB.bmp" "$ZEOSREG/images/TBoldDatabaseAdapterZeos.png"
(cd "$ZEOSREG/images" && lazres ../boldzeoslazarusreg.res TBoldDatabaseAdapterZeos.png)
