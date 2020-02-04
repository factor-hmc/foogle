USING: accessors io io.encodings.utf8 io.files json.writer locals vocabs sequences words ;
IN: make-database

! makes a hashmap suitable for serialization to json
:: mk-hm ( word -- hm )
  word "declared-effect" word-prop :> effect
  word name>> :> word-name
  H{ { "name" word-name } { "effect" effect } } ;

"db.json"
utf8
[ all-words [ mk-hm ] map json-print ]
with-file-writer
