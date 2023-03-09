
type OrderedMapRelations key val =
    ( Rel "ord" (key @ +2, key @ +2)
    , PkRel "map" (key @ +1) (key @ +1, val)
    , Rel "keys" key
    )
