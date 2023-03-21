# Mog: MRDTs on Git

## A slightly reduced LCA-merge formula

The definition of LCA-merge (the double-diamond operator on page 9 of the MRDTs paper) is:

```
F(A, B, C)  :=  (A ^ B ^ C) v (B \ A) v (C \ A)
```

As it happens, the intersection term need not include A. That is, the following definition is equivalent:

```
G(A, B, C)  :=  (    B ^ C) v (B \ A) v (C \ A)
```

This is probably easiest to confirm via model-checking -- that is, with an explicit truth table.

```
A B C | B \ A | C \ A | B ^ C | A ^ B ^ C | F | G
------+-------+-------+-------+-----------+---+---
0 0 0 | 0     | 0     | 0     | 0         | 0 | 0
0 0 1 | 0     | 1     | 0     | 0         | 1 | 1
0 1 0 | 1     | 0     | 0     | 0         | 1 | 1
0 1 1 | 1     | 1     | 1     | 0         | 1 | 1
1 0 0 | 0     | 0     | 0     | 0         | 0 | 0
1 0 1 | 0     | 0     | 0     | 0         | 0 | 0
1 1 0 | 0     | 0     | 0     | 0         | 0 | 0
1 1 1 | 0     | 0     | 1     | 1         | 1 | 1
```


## Git as LCA-merge on relations over unmergeable fields

Consider a Git repository, and suppose -- for the moment -- that all files in the repository are empty. Then any single version of this Git repository is like a mere set of file-paths. Merging two versions then takes all of the paths in both versions, together with the files new to either version relative to their least common ancestor. Because the files are (by assumption) empty, no conflicts occur on any path.

This behavior is exactly that required of the LCA-merge for MRDT relations. In other words, if we can represent a relation as a set of filepaths, we can leverage Git to store that relation.

While we could *theoretically* assemble a path directly from the N components of an element in an N-ary relation, real-world length and symbol constraints make this unwieldy and brittle. Instead, we can hash each tuple to obtain a content-based path component, and store the tuple itself as the contents of a file at that path. Just as empty files cannot conflict, we know that two files at the same path must have the same contents (barring collisions), so conflicts cannot occur under this scheme either.

As a concrete example, consider the Queue MRDT discussed in the original paper. This MRDT has two characteristic relations: first, the unary relation `mem`, consisting of single elements of the queue's element type; and second, the binary relation `ob`, ordering pairs of members of the queue. We can represent the characteristic relations for a queue like `[5, 3, 1]` as the following file tree:

```
list/
    mem/
        ${hash(3)}/
            element := 3
        ${hash(1)}/
            element := 1
        ${hash(2)}/
            element := 2
    ob/
        ${hash(3, 1)}/
            before := 3
            after  := 1
        ${hash(1, 2)}/
            before := 1
            after  := 2
```

* Rather than putting the whole tuple in one file, we break its components out into separate files. This will be important for supporting mergeable component types (see below); but it also happens to allow finer-grained deduplication of these components within Git's object store. Otherwise, changing any one component of a tuple causes the entire containing directory to be re-hashed and moved, so this group of files still acts as one unit.

* Likewise, while the hash of a tuple should be sufficient to identify it, we group whole relations into a directory -- and whole groups of relations for a data structure -- for the benefit of human eyes. (It also happens to confer some degree of collision-resistance by way of scoping; but legibility is the overriding reason.)

Now, if we perform a Git merge of two such filetrees, all tuples present in both sides of the merge -- together with any added fresh since their LCA -- will be present in the resulting filetree.


## Git conflict resolution for mergeable types

To this point, we have studiously avoided conflicts by giving every tuple a content-based address. Two different tuples cannot conflict, because if they differ on any component, they will have different hashes, and thus be placed at different paths in the filesystem. This models the kinds of MRDTs seen in the original paper prior to Section 4.1 -- those involving only types considered "not mergeable".

However, Git has a well-developed system for conflict detection and resolution: given two versions of a file (i.e. distinct file contents assigned to the same path), together with their LCA, we can produce a file capturing the changes in both versions. That is, Git provides LCA-merges both at the level of trees and at the level of files.

We can modify our hashing scheme to *exclude* components of mergeable types from consideration. Then, if two versions have a tuple with the same nonmergeable values but different mergeable values, their component files will exist at the same path in the tree. The nonmergeable values, as before, will be identical; while the mergeable values will be subjected to Git's conflict resolution apparatus.

Following the MRDTs paper, a data structure consisting of triples `(int, int, int)` is used to motivate the introduction of mergeable types. In particular, this type is equivalent to functions `{1, 2, 3} -> int`, and a single characteristic relation on `{1, 2, 3} * int` models the graph of such functions:

```
triple/
    graph/
        ${hash(1)}/
            index := 1
            value := [left field]
        ${hash(2)}/
            index := 2
            value := [middle field]
        ${hash(2)}/
            index := 3
            value := [right field]
```

Because unmergeable types do not contribute to the hash, there are only three places -- one for each index in the triple -- for a tuple in the relation to go. Thus, triples `(30, 31, 32)` and `(100, 31, 32)` will yield three tuples each, with like-indexed values going to the same file. The latter two indices will merge cleanly under Git, while the values at the first index will undergo conflict resolution.

Incidentally, one drawback of this scheme is that our triples are *homogeneous*: all components have the same type, `int`. If we want to model the type of heterogeneous pairs `(int, string)` in this way, we will need a dependent function `(ix : Fin 2) -> F ix`, where `F 0 = int` and `F 1 = string`.

However, if we relax the restriction of Definition 4.3 in the MRDTs paper -- which requires that there always be at least one unmergeable component -- we can model these pairs as the graph of functions `() -> (int, string)`, with no need for dependent types. We'll justify this against relational databases below.


## MRDTs through the lens of relational databases

Because the unmergeable fields in a relation serve to identify individual tuples, we take a perspective rooted in relational databases. A tuple in a relation is a *row* in a *table*; the types describing the components of a relation are the *columns* of its table. Most importantly for us, the unmergeable components form a *primary key*, and components referencing elements of another relation form *foreign keys* that repeat the columns of the referenced primary key. We can then see these tables as sets of *facts* that must be true of -- or are inferrable from -- the data structure instance they represent.

From our discussion above --- and more directly, by Definition 4.3 of the MRDTs paper --- all relations in an MRDT factor into a set of unmergeable types, which serve as a primary key for the relation, and a set of mergeable types, whose values are uniquely identified given values for the key. (That is, *at most* one tuple in a relation will match any given key values.)

Most starkly, primary keys can be characterized in terms of *functional dependencies*, which are written like `k1 k2 k3 -> v1 v2`. If all columns in the table are listed here, this says that `(k1, k2, k3)` is a primary key for the table. This precisely mirrors the functional notation for our type of triples, `{1, 2, 3} -> int`; the value column, of type `value`, functionally depends on the key column, of type `{1, 2, 3}`.

We can reframe our triples example using tables, where we separate the primary key's columns from the associated data using double-bars:

```
 index || value
-------++-------
 1     || 30
 2     || 31
 3     || 32
```

Our model of heterogeneous tuples, `() -> (int, string)`, yields the functional dependency `-> int string` -- that is, we have an empty key. Tables with an empty primary key possess *at most one row*. As such, *any two distinc pairs* will be merged component-wise.

MRDTs, then, provide a mechanism for merging entire databases under (only) primary key constraints. Any violation of a primary key constraint is resolved by merging the offending rows.
