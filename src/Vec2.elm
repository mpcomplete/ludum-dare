module Vec2 exposing (..)

--

type alias Vec2 = { x: Float, y: Float }

add : Vec2 -> Vec2 -> Vec2
add u v = { x = u.x + v.x, y = u.y + v.y}

sub : Vec2 -> Vec2 -> Vec2
sub u v = { x = u.x - v.x, y = u.y - v.y}

scale : Float -> Vec2 -> Vec2
scale s v = { x = s*v.x, y = s*v.y }

toTuple v = (v.x, v.y)

fromTuple (x, y) = Vec2 x y