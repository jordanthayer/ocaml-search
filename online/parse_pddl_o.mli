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
  | PProblem
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
  | Newgoals
  | Commitments
  | Until
  | Within
  | Disable
  | Enable
  | Quit
  | Name of (string)
  | Variable of (string)
  | Number of (float)

val domain :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Domain.domain
val problem :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Domain.problem
val message :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Message.message
