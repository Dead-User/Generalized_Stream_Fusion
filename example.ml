
module ListFunctor = struct
  type ('a, 'b) t =
    | Nil
    | Cons of ('a * 'b)
    | Skip of 'b

  let map f = function
    | Nil -> Nil
    | Cons(a, b) -> Cons(a, f b)
    | Skip b -> Skip(f b)

  let zip f g x y =
    match (x, y) with
    | (Cons(a1, b1), Cons(a2, b2)) ->
        Cons(f a1 a2, g b1 b2)
    | (Skip b1, Skip b2) ->
        Skip(g b1 b2)
    | _ -> Nil
end;;

module List = struct
  module Stream = Stream.MakeZipStream(ListFunctor)
  open ListFunctor
  type ('a, 'b) t = ('a, 'b) Stream.stream

  let fold : ('a code -> 'b code -> 'b code)
          -> 'b code
          -> ('a, 's) t
          -> 'b code
  = fun forCons forNil ->
    let folder = function
      | Nil -> forNil
      | Cons(a, b) -> forCons a b
      | Skip b -> b
    in
    fun str -> Stream.fold folder str

  let map : ('a code -> 'b code)
         -> ('a, 's) t
         -> ('b, 's) t
  = fun func ->
    let mapper x =
      Stream.{ call = fun k ->
        match x with
        | Nil -> k Nil
        | Cons(a, b) -> k @@ Cons(func a, b)
        | Skip b -> k @@ Skip b
      }
    in
    fun str -> Stream.map mapper str

  let filter : ('a code -> bool code)
            -> ('a, 's) t
            -> ('a, 's) t
  = fun predicate ->
    let mapper x = 
      Stream.{ call = fun k ->
        match x with
        | Nil -> k Nil
        | Cons(a, b) ->
            .<if .~(predicate a) then
                .~(k @@ Skip b)
              else
                .~(k @@ Cons(a, b))>.
        | Skip b -> k @@ Skip b
      }
    in
    fun str -> Stream.map mapper str

  let unfold : ('s code -> bool code)
            -> ('s code -> ('a * 's) code)
            -> 's code
            -> ('a, 's) t
  = fun predicate forCons start ->
    let step state =
      Stream.{ call = fun k ->
        .<if .~(predicate state) then
            .~(k Nil)
          else
            let (this, rest) = .~(forCons state) in
            .~(k @@ Cons( .<this>., .<rest>.))>.
      }
    in
    Stream.{start; step}

  let iota : int code -> (int, int) t = fun n ->
    unfold (fun x -> .<.~x = .~n>.)
           (fun x -> .<.~x, .~x + 1>.)
           n

  let zip : ('a, 's1) t
         -> ('b, 's2) t
         -> (('a * 'b), ('s1 * 's2)) t
  = Stream.zip
end;;

let sumOfSquares = .<
  fun n ->
    .~(List.(iota .<n>.
          |> map (fun x -> .<.~x * .~x>.)
          |> fold (fun x y -> .<.~x + .~y>.) .<0>.))
    >.;;

module TreeFunctor = struct
  type ('a, 'b) t =
    | Leaf of 'a
    | Tree of ('b * 'b)

  let map f = function
    | Leaf a -> Leaf a
    | Tree(l, r) -> Tree(f l, f r)
end;;

module Tree = struct
  module Stream = Stream.MakeStream(TreeFunctor)
  open TreeFunctor
  type ('a, 's) t = ('a, 's) Stream.stream

  let fold : ('b code -> 'b code -> 'b code)
          -> ('a code -> 'b code)
          -> ('a, 's) t
          -> 'b code
  = fun forTree forLeaf ->
    let folder = function
      | Leaf a -> forLeaf a
      | Tree(l, r) -> forTree l r
    in
    fun str -> Stream.fold folder str

  let map : ('a code -> 'b code)
         -> ('a, 's) t
         -> ('b, 's) t
  = fun func ->
    let mapper x =
      Stream.{ call = fun k ->
        match x with
        | Leaf a -> k @@ Leaf(func a)
        | Tree(l, r) -> k @@ Tree(l, r)
      }
    in
    fun str -> Stream.map mapper str

  let unfold : ('s code -> bool code)
            -> ('s code -> 'a code)
            -> ('s code -> ('b * 'b) code)
            -> 's code
            -> ('a, 's) t
  = fun predicate forLeaf forTree start ->
    let step state =
      Stream.{ call = fun k ->
        .<if .~(predicate state) then
            .~(k @@ Leaf(forLeaf state))
          else
            let (l, r) = .~(forTree state) in
            .~(k @@ Tree( .<l>., .<r>.))>.
      }
    in
    Stream.{start; step}
end;;

let fib = .<
  fun n ->
    .~(Tree.(unfold (fun x -> .<.~x <= 1>.)
                    (fun x -> x)
                    (fun x -> .<.~x - 1, .~x - 2>.)
                    .<n>.
          |> fold (fun x y -> .<.~x + .~y>.)
                  (fun x -> x)))
    >.;;
