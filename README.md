# MuPRL
MuPRL is a small, mainly instructional, proof assistant in the style of NuPRL. For a good introduction to Computational Type Theory, see [this](http://www.nuprl.org/documents/Constable/naive.pdf).

# Examples
Let's try to prove a very simple tautology: "for every proposition a, a implies a".
```
Theorem i : (a:universe 0) -> (x:a) -> a {
    id
}
```
If we try running this, we will get the following message:
```
Error:
    Unproved Subgoals:   ⊢ (a:universe 0) -> (x:a) -> a
```
To start, we are going to need to add `(a:universe 0)` and `(x:a)` as hypotheses. For this we can use the `fun/intro` rule.

```
Theorem i : (a:universe 0) -> (x:a) -> a {
    rule fun/intro
}
```
This generates the following subgoals.

```
Unproved Subgoals:  a:universe 0 ⊢ (x:a) -> a
                        ⊢ universe 0 = universe 0 in universe 1
```
The first one should be pretty self explanitory, but the second one is a bit more bizzare. It 
essentially is asking us to show that `universe 0` is a valid proposition.

Both of these subgoals are going to need different rules applied to them. To do this, we can use `[]` to specify which
goal we want to apply a rule to, and `;` to sequence.

```
Theorem i : (a:universe 0) -> (x:a) -> a {
    rule fun/intro; [rule fun/intro, rule universe/eqtype]
}
```

```
Unproved Subgoals:  a:universe 0, x:a ⊢ a
                    a:universe 0 ⊢ a = a in universe 0
```

The 1st goal should be pretty easy to prove, as it is already in our hypotheses! As for the second one, we can use `eq/intro` to prove it.

```
Theorem i : (a:universe 0) -> (x:a) -> a {
    rule fun/intro; [rule fun/intro; [rule assumption, rule eq/intro], rule universe/eqtype]
}
```

If we run this, we get the output `\a. \x. x`, which is a program that meets our specification! This means that our proof is complete. However, this seems like quite a lot of busy work for what should be a very simple proof. To remedy this, we can use some more complicated tactics. For starters, we can use the `intro` tactic to automatically select the proper introduction rule for us.

```
Theorem i : (a:universe 0) -> (x:a) -> a {
    intro; [intro; [rule assumption, intro], rule universe/eqtype]
}
```

Also, we seem to be applying the `intro` rule quite a few times in a row, so we can use the `*` tactic to run the `intro` tactic repeatedly until it fails.

```
Theorem i : (a:universe 0) -> (x:a) -> a {
    *intro; [rule assumption, rule universe/eqtype]
}
```


# Building
```
stack build && stack exec muprl
```
