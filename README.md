# nbt
aNother Build Tool for Scala

nbt project is started with aim to rewrite sbt "the right way".
Namely:

- No ugly configuration-like/Scala mix with esoteric Scala calls
- transparent behaviour
- to be (really) simple to use
- easier dependency management
- "less is more" approach to functionality

Elaboration:

Configuration to be written in a human-friendly language.
For typical projects you should be able to cope without writing any configuration at all using "convention over configuration" approach.
For easier dependency management you are provided with pre-built profiles for different kinds of typical projects.
You are also able to define your own profiles with dependencies OR ASK ME TO CREATE YOUR PROFILE FOR YOU!
During a build (whatever it means) you should be able to understand what happens under hood.
nbt uses a scenario which maps each of execution/build phases to a list of specific commands.
nbt provides a default execution scenario which you are able customize per-project.
