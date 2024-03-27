type expr
= C1 of expr2
| C2 of expr2 * expr
and expr2 
= X of string | Paren of expr