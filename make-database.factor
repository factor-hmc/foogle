USING: accessors help.html io io.encodings.utf8 io.files json.writer locals vocabs sequences words ;
IN: make-database

! makes a hashmap suitable for serialization to json
:: mk-hm ( word -- hm )
  word "declared-effect" word-prop :> effect
  word name>> :> word-name
  word topic>filename "https://docs.factorcode.org/content/" prepend :> url
  H{ { "name" word-name } { "effect" effect } { "url" url } } ;

"db2.json"
utf8
[ all-words [ mk-hm ] map json-print ]
with-file-writer
