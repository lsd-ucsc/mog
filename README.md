# Mog: MRDTs on Git

## Relational encodings

An object in Mog is encoded as a collection of *tables*. For instance, consider a queue -- that is, an ordered list -- containing the elements `a`, `b`, and `c`, in that order. We can represent this using two tables: a membership table and an ordering table relating the membership table to itself:

```
mem | element |
    +---------+
    | a       |
    | b       |
    | c       |
    +---------+

ord | left | right |
    +------+-------+
    | a    | b     |
    | a    | c     |
    | b    | c     |
    +------+-------+
```

To make this structure amenable to Git, we organize each table into a directory, each row into a subdirectory, and each column into a file. Rows are identified by their hash -- here, by concatenating columns in order, though a more robust hashing scheme would be used in practice to avoid collisions due to crossfeed between columns.

```
queue1
├── mem
│   ├── 2cd6ee2c70b0bde53fbe6cac3c8b8bb1
│   │   └── element ("c")
│   ├── 3b5d5c3712955042212316173ccf37be
│   │   └── element ("b")
│   └── 60b725f10c9c85c70d97880dfe8191b3
│       └── element ("a")
└── ord
    ├── 672b4cf9efe641771a1f1e9d64665653
    │   ├── left  ("/mem/60b725f10c9c85c70d97880dfe8191b3")
    │   └── right ("/mem/3b5d5c3712955042212316173ccf37be")
    ├── 6defb9e12e6fbf3aee1b76cf3908a585
    │   ├── left  ("/mem/60b725f10c9c85c70d97880dfe8191b3")
    │   └── right ("/mem/2cd6ee2c70b0bde53fbe6cac3c8b8bb1")
    └── 91a564699a9fce57aa54a7c2b2f86ba2
        ├── left  ("/mem/3b5d5c3712955042212316173ccf37be")
        └── right ("/mem/2cd6ee2c70b0bde53fbe6cac3c8b8bb1")
```

To avoid duplicating (potentially large) data objects, the actual data held by our queue only appears in the `mem` hierarchy. In the `ord` hierarchy, we reference members by their content-based addresses, much like a foreign key in a true relational database might reference a foreign row by its primary key.

We can see these tables as sets of *facts* that must be true of -- or are inferable from -- the data structure instance they represent. From this perspective, the tables above are the *maximal* sets of facts for this relation -- adding any more facts would either change the instance, or introduce an inconsistency. We could just as well compute a *minimal* set of facts; a database that uniquely identifies an instance, but where removing any one fact would cause the data structure instance to be underdetermined. We will return to these extremal databases when discussing merges.


## Merging with Git

TODO
