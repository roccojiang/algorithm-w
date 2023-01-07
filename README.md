# ML Language REPL

(Work in progress)

A REPL for a bare-bones subset of ML, as presented in the course notes for the [Type Systems for Programming Languages](https://www.doc.ic.ac.uk/~svb/TSfPL/) course at Imperial.

ML expressions are defined as a combination of what's presented in Milner's [A Theory of Type Polymorphism in Programming](https://doi.org/10.1016/0022-0000(78)90014-4) and Damas's PhD thesis [Type Assignment in Programming Languages](https://era.ed.ac.uk/bitstream/handle/1842/13555/Damas1984.Pdf):

$E ::= x\ |\ c\ |\ \lambda x . E\ |\ E_1 E_2\ |\ \text{let } x = E_1 \text{ in } E_2\ |\ \text{fix } g .\ E$

Type inference using Algorithm W is implemented in a manner which is visually very similar to the way it is presented in the course notes.

## TODO
- [x] Fully implement ML expressions
- [x] Parse ML expressions
- [x] Implement Algorithm W type inference
- [ ] Implement expression evaluation via beta reduction
- [ ] Improve error messages
- [ ] Add option to show intermediate steps for inference/evaluation
- [ ] Polish REPL program
