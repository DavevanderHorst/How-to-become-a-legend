# How-to-become-a-legend
rogue like game

Next programming steps.

Make monster turn after hero moved.    
    -- can't make 2 animations at the same time...
    -- so monster need to do their thing one by one.
    -- monsters have settled damage for now and always move towards hero or attack when next to it    
    -- monster animation need to be made    

when a monster is attacking its spot can't be entered by other monsters.
test by setting monster in a hallway with hero before, what will monster do then...

Give hero and monster hit-points. Damage removes hit-points.
Make a monster death animation.



IDEAS!

Monster always moves now right around, so with 2 same shortest roads, it takes always same. Make a list with directions,
        and go around the direction that's head, and remove and add to the back of the list.
Sticky terrain, terrain with a chance to get stick...





for rebuilding changes / live watching 

elm make src/Main.elm --output main.js

then open index.html with any explorer


to view errors, use elm reactor and open Main.elm


 -- THINGS TODO - if we feel like it
 
- attack animation, tilt back, tilt forward