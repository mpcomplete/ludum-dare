module AABB exposing (AABB, intersects)

import Vec2 exposing (Vec2)

type alias AABB = { position: Vec2, size: Vec2 }

-- translate : Vec2 -> AABB -> AABB
-- translate amount box = { box | position <- box.position <+> amount }

intersects : AABB -> AABB -> Bool
intersects box1 box2 =
    let (l1, t1) = Vec2.toTuple box1.position
        (w1, h1) = Vec2.toTuple box1.size
        (l2, t2) = Vec2.toTuple box2.position
        (w2, h2) = Vec2.toTuple box2.size
        r1 = l1 + w1
        b1 = t1 - h1
        r2 = l2 + w2
        b2 = t2 - h2
    in
    if w1 == 0 || w2 == 0 then False else not (l2 > r1 || r2 < l1 || t2 < b1 || b2 > t1)

-- corners : AABB -> (Vec2, Vec2)
-- corners box = (box.position <-> box.size, box.position <+> box.size)