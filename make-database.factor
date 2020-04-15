USING: accessors classes hashtables help help.html help.markup io io.encodings.utf8 io.files 
  io.streams.string json.writer kernel locals vocabs sequences words words.symbol ;
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
  word "declared-effect" word-prop :> effect
  word name>> :> word-name
  word topic>filename "https://docs.factorcode.org/content/" prepend :> url
  word vocabulary>> :> vocabulary
  vocabulary escape-filename "https://docs.factorcode.org/content/vocab-" prepend ".html" append :> vocabulary-url
  ! note that this code currently drops the remaining arguments if the effect arrays have more than 2 elements
  ! word "help" word-prop :> help-prop 
  ! word class? word symbol? help-prop f = or or 
  !   [ H{ } ] 
  !   [ 0 help-prop nth dup 1 short head { $values } = 
  !     [ 1 short tail >hashtable ]
  !     [ drop H{ } ]
  !     if
  !   ]
  !   if :> effect-descriptions
  word get-effect-descriptions :> effect-descriptions
  H{ { "name" word-name } 
     { "effect" effect } 
     { "url" url } 
     { "vocabulary" vocabulary } 
     { "vocabulary_url" vocabulary-url } 
     { "effect_descriptions" effect-descriptions } 
   } ;

"db.json"
utf8
[ all-words [ mk-hm ] map json-print ]
with-file-writer
