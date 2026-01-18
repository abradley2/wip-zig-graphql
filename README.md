# WIP - Zig GraphQL

**This project primarily lives on [Codeberg](https://codeberg.org/abradley2/wip-zig-graphql),
for many of the [same reasons many other projects are](https://ziglang.org/news/migrating-from-github-to-codeberg/).**

**The Github repository is just a mirror including only the master branch**

# Purpose

Work in progress Zig GraphQL library

This is a complete re-imagining of typical GraphQL implementations
with the aim of "putting the Graph back in GraphQL" 

# Graph Data Structure

The underlying Data Structure of a Graph is not lost as a concept. 
Types are vertices, fields are edges.

# The focus is on the Vertex, not the Edge

"Resolvers" are not for fields, they are for types. GraphQL represents a
multigraph in which there may be multiple edges between two vertices. By focusing
on the types over fields our implementation becomes more organized and less
repetitive.


