# Fatty acid biosynthesis

This is a proof of concept to demonstrate that functional programming can be used to explore biochemical pathways. In this example, the biochemical pathway for the synthesis of DHA (22:6 n-3) from alpha-linolenic acid (18:3 n-3) is computed.

The underlying idea is that classes of molecules can be represented as data types and enzymatic reactions can be modelled as functions. For example, fatty acids can be represented by the type `FA` and an enzymatic reaction, such as `elongase`, can be modelled as a function which takes an `FA` and returns a new `FA` with two additional carbons (inserted at the carboxylic acid end). If we have a series of known enzymatic reactions and a target product molecule, we can explore all the possible biochemical pathways to find any pathway/s which produce the target product molecule. In this current implementation, the `Writer` monad is used to track biochemical pathways.

The output for this program is:

Biosynthetic pathway/s found:
9Z,12Z,15Z-18:3 -> 6Z,9Z,12Z,15Z-18:4 -> 8Z,11Z,14Z,17Z-20:4 -> 5Z,8Z,11Z,14Z,17Z-20:5 -> 7Z,10Z,13Z,16Z,19Z-22:5 -> 9Z,12Z,15Z,18Z,21Z-24:5 -> 6Z,9Z,12Z,15Z,18Z,21Z-24:6 -> 4Z,7Z,10Z,13Z,16Z,19Z-22:6
