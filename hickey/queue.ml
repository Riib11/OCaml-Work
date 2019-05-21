open List;;

type 'a queue = ('a list * 'a list) ref

let create () = ref ([], [])

let add queue x =
    let (front, back) = !queue in
    queue := (x::front, back)


let rec take queue =
    match !queue with
        (front, x::back) ->
            queue := (front, back);
            x
      | ([], []) ->
            raise (Invalid_argument "queue is empty")
      | (front, []) ->
            queue := ([], List.rev front);
            take queue

