# MuPRL
MuPRL is a small, mainly instructional, proof assistant in the style of NuPRL.

# Examples
```
(\x.x) = (\x.x) in unit -> unit
H >> \x. x = \x. x in (_:unit) -> unit
by intro-lambda at 1 new y
H >> y = y in unit
by hypothesis
H >> unit = unit in universe 1
by intro-unit
```

# Building
```
stack build && stack exec muprl
```
