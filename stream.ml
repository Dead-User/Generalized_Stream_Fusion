
module type Functor = sig
  type ('a, 'b) t
  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end;;

module type Zipper = sig
  include Functor
  val zip : ('a -> 'b -> 'e)
         -> ('c -> 'd -> 'f)
         -> ('a, 'c) t
         -> ('b, 'd) t
         -> ('e, 'f) t
end;;

module MakeStream(X : Functor) = struct
  type ('a, 's) streamFunc =
    { call : 'b. (('a code, 's code) X.t -> 'b code) -> 'b code }

  type ('a, 's) stream =
    { start : 's code
    ; step  : 's code -> ('a, 's) streamFunc }

  let fold : (('a code, 'b code) X.t -> 'b code)
          -> ('a, 's) stream
          -> 'b code
  = fun folder {start; step} ->
    .<let start = .~start in
      let rec iterate state =
        .~((step .<state>.).call @@ fun v ->
           v |> X.map (fun x -> .<iterate .~x>.)
             |> folder)
      in
      iterate start>.

  let map : (('a code, 's code) X.t -> ('b, 's) streamFunc) 
         -> ('a, 's) stream
         -> ('b, 's) stream
  = fun mapper {start; step} ->
    let step' state =
      { call = fun k ->
        (step state).call @@ fun v0 ->
          (mapper v0).call k }
    in
    {start; step=step'}

end;;

module MakeZipStream(X : Zipper) = struct
  include MakeStream(X)
  let zip : ('a, 's1) stream
         -> ('b, 's2) stream
         -> (('a * 'b), ('s1 * 's2)) stream
  = fun {start=s1; step=step1} {start=s2; step=step2} ->
    let step' state =
      { call = fun k ->
        .<let (s1, s2) = .~state in
          .~((step1 .<s1>.).call @@ fun v1 ->
             (step2 .<s2>.).call @@ fun v2 ->
               let makePair x y = .<.~x, .~y>. in
               X.zip makePair makePair v1 v2 |> k)>. }
    in
    {start = .<.~s1, .~s2>.; step = step'}
end;;
