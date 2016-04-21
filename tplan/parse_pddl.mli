type token =
  | CP
  | OP
  | Dash
  | Eq
  | And
  | At
  | Define
  | Domain
  | Not
  | Over
  | Problem
  | Action
  | Condition
  | Constants
  | PDomain
  | Duration
  | Durative
  | Effect
  | Goal
  | Init
  | Metric
  | Objects
  | Parameters
  | Precondition
  | Predicates
  | Requirements
  | Types
  | Name of (string)
  | Variable of (string)
  | Number of (float)

val domain :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tpl_domain.domain
val problem :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tpl_domain.problem
