#!/bin/bash

TEXT=$(cat)

LAYOUT_CACHE_FILE="./.slint-show.cache.txt"
cat <<EOF > $LAYOUT_CACHE_FILE
_ := Window {
  visible: false;
  no-frame: true;
  property inp_text <=> inp-text.text;
  Flickable {
    interactive: true;
    viewport-height: inp-text.height;
    viewport-width: inp-text.width;
    inp-text := Text {
      wrap: TextWrap.word-wrap;
      font-size: 20px;
    }
  }
}
EOF

echo $PWD >> /c/users/niv/slint.log.txt
echo "{ \"inp_text\": \"$TEXT\" }" | \
    tokay "[^\\n]+ print(\$1); '\n' [^\\n]+ print(\"\\\n\" + \$2)" -- - | \
    tr -d '\n' | \
    slint-viewer.exe $LAYOUT_CACHE_FILE --load-data -  >> /c/users/niv/slint.log.txt
