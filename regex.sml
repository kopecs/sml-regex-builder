(* James Gallicchio & Cooper Pierce 
 * 6 March 2020
 *
 * Code for accept from Michael Erdmann & Frank Pfenning
 *)


signature REGEX =
sig
  type t
  val build : string -> t option
  val accept : t -> string -> bool
end

structure Regex : REGEX =
struct
datatype t =
    Zero
  | One
  | Char of char
  | Plus of t * t
  | Times of t * t
  | Star of t

  fun reducel (f : 'a * 'a -> 'a) (L : 'a list) : 'a option =
  let
    fun recRed (f : 'a * 'a -> 'a) ([] : 'a list) (acc : 'a) = acc
      | recRed f (x::xs) acc = recRed f xs (f (acc, x))
  in
    case L of [] => NONE | (x::xs) => SOME (recRed f xs x)
  end

  (* list of characters to parse, and list of tokens that have been parsed
  * and will eventually be concatted together. acc is in reverse order. *)
  fun parseCs ([] : char list, [] : t list) : t option = SOME One
    | parseCs ([], acc) =
        reducel (fn (x, y) => Times(x,y)) (List.rev acc)
    | parseCs (c::cs, acc) =
        case c of
             #"\\" => (case cs of
                           [] => NONE
                         | c' :: cs' => parseCs (cs', Char c'::acc))
           | #"(" =>
               let
                 (* Partition cs into inner and rem
                  * Both end up in reverse order *)
                 val (inner, _, rem) =
                   foldl (fn (x, (inner, depth, rem)) =>
                           if depth = 0 then
                             (inner, depth, x::rem)
                           else
                             case x of
                                  #"(" => (#"("::inner, depth+1, rem)
                                | #")" => (#")"::inner, depth-1, rem)
                                | a    => (a::inner, depth, rem)
                         ) ([], 1, []) cs
               in
                 case inner of
                      #")"::inner =>
                       (case parseCs (List.rev inner, []) of
                            SOME innerR => parseCs (List.rev rem, innerR::acc)
                          | NONE => NONE)
                    | _ => NONE
                end
           | #"*" => (case acc of [] => NONE
                         | h::t => parseCs (cs, (Star h)::t))
           | #"|" =>
                Option.mapPartial (fn (left : t) =>
                    Option.map (fn (right : t) =>
                        Plus (left, right)
                    ) (parseCs (cs, []))
                ) (parseCs ([], acc))
           | #")" => NONE
           | a => parseCs (cs, Char a::acc)

fun build (regex : string) : t option =
  parseCs (String.explode regex, [])

(* Some basic tests *)
val SOME One = build ""
val SOME (Char #"a") = build "a"
val SOME (Plus (Char #"a", Char #"b")) = build "a|b"
val SOME (Times (Char #"a", Char #"b")) = build "ab"
val SOME (Star (Char #"a")) = build "a*"


(* 15-150, Spring 2020                                                *)
(* Michael Erdmann & Frank Pfenning                                   *)
(* Code for Lecture 15:  Regular Expression Matching with Combinators *)
(* Here to end of file *)
type matcher = char list -> (char list -> bool) -> bool

val REJECT : matcher = fn _ => fn _ => false
val ACCEPT : matcher = fn cs => fn k => k cs


fun CHECK_FOR (a : char) : matcher =
      fn cs => fn k => case cs of
                         [] => false
                       | (c::cs') => (a=c) andalso (k cs')

(* Here is a version of CHECK_FOR that stages the character check: *)
fun CHECK_FOR (a : char) : matcher =
      fn [] => REJECT []
       | c::cs => if a=c then ACCEPT cs else REJECT (c::cs)


infixr 8 ORELSE
infixr 9 THEN

fun (m1 : matcher) ORELSE (m2 : matcher) : matcher = 
       fn cs => fn k => m1 cs k orelse m2 cs k

fun (m1 : matcher) THEN (m2 : matcher) : matcher = 
       fn cs => fn k => m1 cs (fn cs' => m2 cs' k)

(* If we allow arbitrary regular expressions,  *)
(* then we might implement REPEAT like this:   *)
fun REPEAT (m : matcher) : matcher = fn cs => fn k =>
    let
       fun mstar cs' = k cs' orelse
                       m cs' (fn cs'' => not (cs' = cs'') andalso mstar cs'')
    in
       mstar cs
    end

(* Build a matcher from a regexp.
   match : regexp -> char list -> (char list -> bool) -> bool
   Specs are as in Lecture 14.

   Observe how match can traverse the regular expression passed to it, 
   creating a matcher for that regular expression, prior to seeing 
   any character input or continuations.
*)
fun match (Char a) = CHECK_FOR a
  | match One = ACCEPT
  | match Zero = REJECT
  | match (Times (r1, r2)) = (match r1) THEN (match r2)
  | match (Plus (r1, r2)) = (match r1) ORELSE (match r2)
  | match (Star r) = REPEAT (match r)


(* accept : regexp -> string -> bool
   Specs are as in Lecture 14.
   We write code that is a little more complicated than we did in Lecture 14
   in order to take good advantage of the staging now present in match.
*)
   
fun accept (r : t) : string -> bool = 
    let
       val m = match r
    in
       fn s => m (String.explode s) List.null
    end
end
