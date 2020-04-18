USING: accessors assocs classes hashtables help help.html help.markup io io.encodings.utf8 io.files 
  io.streams.string json.writer kernel locals mirrors vocabs sequences words words.symbol ;
IN: make-database

: get-effect-descriptions ( word -- docs )
  ! get the documentation
  word-help
  ! get the part corresponding to effect information (if possible)
  [ dup sequence? [ { $values } head? ] [ drop f ] if ] find nip
  dup 
    [
      rest [ values-row ] map
      ! render each cell and then convert to a hashtable
      [ [ [ print-element ] with-string-writer ] map ] map
      >hashtable 
    ]
    [ drop H{ } ]
  if ;

! makes a hashmap suitable for serialization to json
:: mk-hm ( word -- hm )
  ! don't make into a hashtable if it has no effect
  word "declared-effect" word-prop dup [ <mirror> >hashtable ] [ ] if :> effect
  word get-effect-descriptions :> effect-descriptions
  ! only set if there is an effect
  effect [ effect-descriptions "effect_descriptions" effect set-at ] [ ] if
  word name>> :> word-name
  word topic>filename "https://docs.factorcode.org/content/" prepend :> url
  word vocabulary>> :> vocabulary
  vocabulary escape-filename "https://docs.factorcode.org/content/vocab-" prepend ".html" append :> vocabulary-url
  H{ { "name" word-name } 
     { "effect" effect } 
     { "url" url } 
     { "vocabulary" vocabulary } 
     { "vocabulary_url" vocabulary-url } 
   } ;

"db.json"
utf8
[ all-words [ mk-hm ] map json-print ]
with-file-writer
