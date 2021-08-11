---
layout: post
title:  "Rhythm and Fibonacci Numbers"
date:   2021-08-10 00:45:45 -0400
categories: jekyll update
usemathjax: true
---

How many distinct rhythms in any given amount of beats?

There isn't any crazy math involved with this problem, but I solved this without any help from others and I like its solution. I'm sure this is given as some exercise in a combinatorics textbook out there.



The answer to this question where held beats aren't considered is pretty obviously \\(2^n\\), where \\(n\\) is the number of indivisible beats allowed. Either a beat is an attack beat or a rest beat. So there answer for drummers, where the two measures below are considered equivalent, is solved.

![Equivalent measures for drummers](/assets/img/drumrhythm.png)

However, the question gets a bit more complex when we account for sustained notes. In some instruments (woodwind instruments, synthesizers, etc.), there is a audible distinction between the following two measures of music.

![These measures are different for non-drummers](/assets/img/saxrhythm.png)

We can model these rhythms using a method similar to the previous scenario with a few more rules. Each beat is either an attack beat, a sustain beat, or a rest beat. Also, we cannot have a sustain beat after a rest beat or at the start.

In the code below, we generate all possible strings of "a", "\_", "s" (representing attack, rest, and sustain respectively) with length 4 not starting with "s". Then we remove all strings containing "\_s" (a sustain beat after a rest beat).

~~~
In[1]:= Select[
 Table[StringJoin[
   IntegerDigits[n, 3, 4] /. (0 -> "a") /. (1 -> "_") /. (2 -> 
      "s")], {n, 0, 2*3^3 - 1}], ! StringContainsQ[#, "_s"] &]

Out[1]= {"aaaa", "aaa_", "aaas", "aa_a", "aa__", "aasa", "aas_", \
"aass", "a_aa", "a_a_", "a_as", "a__a", "a___", "asaa", "asa_", \
"asas", "as_a", "as__", "assa", "ass_", "asss", "_aaa", "_aa_", \
"_aas", "_a_a", "_a__", "_asa", "_as_", "_ass", "__aa", "__a_", \
"__as", "___a", "____"}
~~~

As an example, the two measures in the last image would be represented as `"asas"` and `"a_a_"` respectively.

With 4 beats, we got 34 possible measures of music. The code below shows the possible rhythms for 1 beat to 8 beats.

~~~
In[2]:= Table[
 Length[Select[
   Table[StringJoin[
     IntegerDigits[n, 3, l] /. (0 -> "a") /. (1 -> "_") /. (2 -> 
        "s")], {n, 0, 2*3^(l - 1) - 1}], ! 
     StringContainsQ[#, "_s"] &]], {l, 1, 8}]

Out[2]= {2, 5, 13, 34, 89, 233, 610, 1597}
~~~

If this pattern looks somewhat familiar, it's because it probably somewhat is.

~~~
In[3]:= Table[Fibonacci[2 n + 1], {n, 1, 8}]

Out[3]= {2, 5, 13, 34, 89, 233, 610, 1597}
~~~

But why?

From here, I thought about recurrence relations, since a rhythm with \\(n\\) beats is just a rhythm with \\(n-1\\) beats with one beat added on. Following the rules of valid rhythms, we start with either an attack or a rest. Then if the rhythm doesn't end with a rest we can add any of the three possibilities, otherwise we can only add an attack or a rest.

The trick here is to split the counting of the rhythms into two categories: ending with an attack or sustain (playing at the end) or ending with a rest (stopped at the end). We will represent these with \\(p\_n\\) and \\(s\_n\\). Our desired amount of rhythms \\(r_n\\) is the sum of these, or \\(p\_n+s\_n\\)

$$
\begin{align*}
s_1 &= 1 \\
p_1 &= 1 \\
s_n &= p_{n-1} + s_{n-1} \\
p_n &= 2 p_{n-1} + s_{n-1} \\
\end{align*}
$$

We start with one stopped rhythm (rest) and one playing rhythm (attack). To get a rhythm that's stopped at the end, we can add a rest to a playing rhythm or a rest to a stopped rhythm. To get a rhythm that's playing at the end, we can add an attack or sustain to a playing rhythm or an attack to a stopped rhythm. These possibilies give us our recurrence relation.

The Fibonacci numbers appear more obviously when we slightly rewrite the last equation in the recurrence relation using \\(s\_n = p\_{n-1} + s\_{n-1}\\).

$$
\begin{align*}
s_1 &= 1 \\
p_1 &= 1 \\
s_n &= p_{n-1} + s_{n-1} \\
p_n &= s_{n} + p_{n-1} \\
\end{align*}
$$

From this we can see that \\(s_n = F_{2n}\\) and \\(p_n = F_{2n+1}\\) (where \\(F_0 = 1\\)).

This gives us the final answer of \\(r_n=p_n+s_n=F_{2n}+F_{2n+1}=F_{2n+2}\\)

For fun, here's a tree of the possible rhythms up to three, where the parent nodes are the beginnings of the children.
~~~
In[4]:= l = {"a", "_"}; e = {"" \[DirectedEdge] "a", "" \[DirectedEdge] "_"};
For[i = 0, i < 3 - 1, i++,
  e = Join[e, 
    If[StringTake[#, -1] == 
        "_", {# \[DirectedEdge] # <> "a", # \[DirectedEdge] # <> 
          "_"}, {# \[DirectedEdge] # <> "a", # \[DirectedEdge] # <> 
          "s", # \[DirectedEdge] # <> "_"}] & /@ l];
  l = Flatten[
    If[StringTake[#, -1] == 
        "_", {# <> "a", # <> "_"}, {# <> "a", # <> "s", # <> "_"}] & /@
      l]
  ];
TreePlot[Flatten[e], VertexLabels -> Placed["Name", Center]]
~~~

![Rhythm Tree](/assets/img/rhythmtree.png)

