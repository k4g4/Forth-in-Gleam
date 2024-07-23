import gleam/bool.{guard}
import gleam/dict.{type Dict, get, insert}
import gleam/int.{parse, to_string}
import gleam/list.{fold, length, try_fold}
import gleam/result.{is_error, lazy_unwrap, map, try_recover, unwrap}
import gleam/string.{contains, lowercase, split, trim}

pub opaque type Forth {
  Forth(stack: List(Int), state: State, defs: Dict(String, String))
}

type State {
  Eval
  NewIdent
  DefIdent(ident: String, def: String)
}

pub type ForthError {
  DivisionByZero
  StackUnderflow
  InvalidWord
  UnknownWord
}

pub fn new() -> Forth {
  Forth(stack: [], state: Eval, defs: dict.new())
}

pub fn format_stack(f: Forth) -> String {
  f.stack |> fold("", fn(acc, item) { to_string(item) <> " " <> acc }) |> trim
}

pub fn eval(f: Forth, prog: String) -> Result(Forth, ForthError) {
  prog |> lowercase |> trim |> split(" ") |> try_fold(f, do_cmd)
}

fn do_cmd(f: Forth, cmd: String) -> Result(Forth, ForthError) {
  case f.state {
    NewIdent -> new_ident(f, cmd)
    DefIdent(_, _) -> Ok(def_ident(f, cmd))
    Eval -> {
      use <- guard(cmd == ":", Ok(Forth(..f, state: NewIdent)))
      use _ <- try_recover(try_parse(f, cmd))
      use <- lazy_unwrap(try_def(f, cmd))
      use <- lazy_unwrap(try_op(f, cmd))
      try_keyword(f, cmd)
    }
  }
}

fn new_ident(f: Forth, cmd: String) -> Result(Forth, ForthError) {
  case is_error(parse(cmd)) && cmd != ":" && cmd != ";" {
    True -> Ok(Forth(..f, state: DefIdent(cmd, "")))
    _ -> Error(InvalidWord)
  }
}

fn def_ident(f: Forth, cmd: String) -> Forth {
  let assert DefIdent(ident, def) = f.state
  case cmd {
    ";" -> Forth(..f, state: Eval, defs: insert(f.defs, ident, def))
    _ -> {
      let suffix = f.defs |> get(cmd) |> unwrap(cmd)
      Forth(..f, state: DefIdent(ident, def <> " " <> suffix))
    }
  }
}

fn try_parse(f: Forth, cmd: String) -> Result(Forth, Nil) {
  use item <- map(parse(cmd))
  Forth(..f, stack: [item, ..f.stack])
}

fn try_def(f: Forth, cmd: String) -> Result(Result(Forth, ForthError), Nil) {
  f.defs |> get(cmd) |> map(eval(f, _))
}

fn try_op(f: Forth, cmd: String) -> Result(Result(Forth, ForthError), Nil) {
  use <- guard(!contains("+*-/", cmd), Error(Nil))
  use <- guard(length(f.stack) < 2, Ok(Error(StackUnderflow)))

  let assert [first, second, ..rest] = f.stack
  case cmd {
    "+" -> Ok(Forth(..f, stack: [second + first, ..rest]))
    "*" -> Ok(Forth(..f, stack: [second * first, ..rest]))
    "-" -> Ok(Forth(..f, stack: [second - first, ..rest]))
    "/" if first == 0 -> Error(DivisionByZero)
    "/" -> Ok(Forth(..f, stack: [second / first, ..rest]))
    _ -> panic
  }
  |> Ok
}

fn try_keyword(f: Forth, cmd: String) -> Result(Forth, ForthError) {
  case cmd {
    "dup" ->
      case f {
        Forth(stack: [first, ..rest], ..) ->
          Ok(Forth(..f, stack: [first, first, ..rest]))
        _ -> Error(StackUnderflow)
      }

    "drop" ->
      case f {
        Forth([_, ..rest], ..) -> Ok(Forth(..f, stack: rest))
        _ -> Error(StackUnderflow)
      }

    "swap" ->
      case f {
        Forth([first, second, ..rest], ..) ->
          Ok(Forth(..f, stack: [second, first, ..rest]))
        _ -> Error(StackUnderflow)
      }

    "over" ->
      case f {
        Forth([first, second, ..rest], ..) ->
          Ok(Forth(..f, stack: [second, first, second, ..rest]))
        _ -> Error(StackUnderflow)
      }

    _ ->
      case get(f.defs, cmd) {
        Ok(def) -> eval(f, def)
        _ -> Error(UnknownWord)
      }
  }
}
