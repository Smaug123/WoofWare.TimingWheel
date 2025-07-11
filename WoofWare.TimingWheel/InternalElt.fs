namespace WoofWare.TimingWheel

type internal InternalElt = ExternalElt

[<Struct>]
type internal Header =
    | Null
    | Used
    | Free of nextFreeHeader : int

type internal Pool<'a> =
    {
        Headers : Header[]
        Elements : 'a[]
        mutable FirstFreeHeader : int voption
        mutable Length : int
        Dummy : 'a option
        CopyTo : int -> 'a[] -> 'a -> unit
    }

[<RequireQualifiedAccess>]
module internal Pool =

    let create<'a> (dummy : 'a option) (copyTo : int -> 'a[] -> 'a -> unit) (capacity : int) : 'a Pool =
        if capacity <= 0 then
            invalidArg "capacity" "capacity must be strictly positive"

        let headers = Array.init capacity (fun i -> Header.Free (i + 1))
        headers.[capacity - 1] <- Header.Null

        let elts =
            match dummy with
            | None -> Array.replicate capacity Unchecked.defaultof<_>
            | Some a -> Array.replicate capacity a

        {
            Elements = elts
            Headers = headers
            FirstFreeHeader = ValueSome 0
            Length = 0
            Dummy = dummy
            CopyTo = copyTo
        }

    let isValid (pool : 'a Pool) (ptr : int) : bool =
        if ptr < 0 || ptr >= pool.Elements.Length then
            false
        else
            match pool.Headers.[ptr] with
            | Header.Used -> true
            | _ -> false

    let isFull (p : Pool<'a>) : bool = p.FirstFreeHeader.IsNone
    let grow (capacity : int) (p : Pool<'a>) : Pool<'a> = failwith "TODO"
    (*
        let { Metadata.slots_per_tuple
        ; capacity = old_capacity
        ; length
        ; next_id
        ; first_free = _
        ; dummy
        }
          =
          metadata t
        in
        let capacity =
          min (max_capacity ~slots_per_tuple) (grow_capacity ~capacity ~old_capacity)
        in
        if capacity = old_capacity
        then
          failwiths
            "Pool.grow cannot grow pool; capacity already at maximum"
            capacity
            [%sexp_of: int];
        let metadata =
          { Metadata.slots_per_tuple
          ; capacity
          ; length
          ; next_id
          ; first_free = Header.null
          ; dummy
          }
        in
        let t' = create_array metadata in
        Uniform_array.blit
          ~src:t
          ~src_pos:start_of_tuples_index
          ~dst:t'
          ~dst_pos:start_of_tuples_index
          ~len:(old_capacity * Metadata.array_indices_per_tuple metadata);
        destroy t;
        unsafe_init_range t' metadata ~lo:old_capacity ~hi:capacity;
        for tuple_num = old_capacity - 1 downto 0 do
          let header_index = tuple_num_to_header_index metadata tuple_num in
          let header = unsafe_header t' ~header_index in
          if not (Header.is_used header)
          then unsafe_add_to_free_list t' metadata ~header_index
        done;
        t'
        *)

    let private malloc (p : Pool<'a>) : int =
        match p.FirstFreeHeader with
        | ValueNone -> failwith "pool is full!"
        | ValueSome firstFree ->

        let nextFreeHeader =
            match p.Headers.[firstFree] with
            | Null -> ValueNone
            | Free nextFreeHeader -> ValueSome nextFreeHeader
            | Used -> failwith "invariant violated: first free header was in fact used"

        p.FirstFreeHeader <- nextFreeHeader
        p.Length <- p.Length + 1

        p.Headers.[firstFree] <- Header.Used

        firstFree

    let private unsafeAddToFreeList (p : Pool<'a>) (ptr : int) : unit =
        let nextFree =
            match p.FirstFreeHeader with
            | ValueNone -> Header.Null
            | ValueSome p -> Header.Free p

        p.Headers.[ptr] <- nextFree
        p.FirstFreeHeader <- ValueSome ptr

    let private unsafeFree (p : Pool<'a>) (ptr : int) =
        p.Length <- p.Length - 1
        unsafeAddToFreeList p ptr

        match p.Dummy with
        | None ->
            // let the garbage collector clean up old stuff
            p.Elements.[ptr] <- Unchecked.defaultof<_>
        | Some d -> p.CopyTo ptr p.Elements d

    let free (p : Pool<'a>) (ptr : int ValueOption) =
        match ptr with
        | ValueNone -> failwith "free of null pointer"
        | ValueSome ptr ->

        if not (isValid p ptr) then
            failwith "free of invalid pointer"

        unsafeFree p ptr

    let new' (p : Pool<'a>) (data : 'a) : int =
        let ptr = malloc p
        p.CopyTo ptr p.Elements data
        ptr

[<RequireQualifiedAccess>]
module internal InternalElt =

    let null' : InternalElt = ValueNone

    let isNull (v : InternalElt) = v.IsNone

    let isValid (p : Pool<'a>) (t : InternalElt) : bool =
        match t with
        | ValueNone -> false
        | ValueSome t -> Pool.isValid p t

    let externalIsValid (p : Pool<'a>) (e : ExternalElt) : bool = isValid p e

    let toExternal (i : InternalElt) : ExternalElt = i

    let ofExternalThrowing (p : Pool<'a>) (e : ExternalElt) : InternalElt =
        if isValid p e then
            e
        else
            failwith "TimingWheel got invalid alarm"

    let key (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : Key =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Key

    let setKey (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (k : Key) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Key <- k

    let value (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : 'a =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Value

    let setValue (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (a : 'a) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Value <- a

    let atTime (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : TimeNs =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].AtTime

    let setAtTime (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (time : TimeNs) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].AtTime <- time

    let levelIndex (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : int =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].LevelIndex

    let setLevelIndex (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (i : int) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].LevelIndex <- i

    let prev (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : InternalElt =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Prev

    let setPrev (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (prev : InternalElt) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Prev <- prev

    let next (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : InternalElt =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Next

    let setNext (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) (n : InternalElt) : unit =
        match t with
        | ValueNone -> failwith "tried to dereference null element pointer"
        | ValueSome t -> p.Elements.[t].Next <- n

    let invariant (pool : Pool<ExternalEltValue<'a>>) (inv : 'a -> unit) (i : InternalElt) : unit =
        if not (isValid pool i) then
            failwith "expected element to be valid for pool"

        inv (value pool i)
        let n = next pool i
        assert (isNull n || i = prev pool n)
        let p = prev pool i
        assert (isNull p || i = next pool p)

    /// Creates an element whose prev and next are both null.
    let create
        (p : Pool<ExternalEltValue<'a>>)
        (key : Key)
        (atTime : TimeNs)
        (v : 'a)
        (levelIndex : int)
        : InternalElt
        =
        let ptr =
            {
                Key = key
                AtTime = atTime
                Value = v
                LevelIndex = levelIndex
                Prev = ValueNone
                Next = ValueNone
            }
            |> Pool.new' p

        ValueSome ptr

    let free (p : Pool<ExternalEltValue<'a>>) (ptr : InternalElt) : unit = Pool.free p ptr

    /// Unlink ptr from the circularly doubly-linked list it is in. This does not mutate the element pointed-to.
    /// Meaningless if ptr is a singleton.
    let unlink (p : Pool<ExternalEltValue<'a>>) (ptr : InternalElt) : unit =
        setNext p (prev p ptr) (next p ptr)
        setPrev p (next p ptr) (prev p ptr)

    let link (p : Pool<ExternalEltValue<'a>>) (prev : InternalElt) (next : InternalElt) : unit =
        setNext p prev next
        setPrev p next prev

    /// Makes t into a singleton circular doubly-linked list.
    let linkToSelf (p : Pool<ExternalEltValue<'a>>) (t : InternalElt) : unit = link p t t

    /// Treats `head` as the head of the list; adds `toAdd` to the end of it.
    let insertAtEnd (p : Pool<ExternalEltValue<'a>>) (head : InternalElt) (toAdd : InternalElt) : unit =
        let prev = prev p head
        link p prev toAdd
        link p toAdd head

    /// Visit each element in the doubly-linked list containing `head`, starting at `head` and following the `next`
    /// pointers.
    let iter (pool : Pool<ExternalEltValue<'a>>) (head : InternalElt) (f : InternalElt -> unit) : unit =
        let mutable current = head
        let mutable cont = true

        while cont do
            // `f` is allowed to mutate the pool, so get `next` before calling it
            let next = next pool current
            f current
            if next = head then cont <- false else current <- next

    /// Walk the circular doubly-linked list to obtain its length.
    let length (pool : Pool<ExternalEltValue<'a>>) (head : InternalElt) : int =
        let mutable result = 0
        iter pool head (fun _ -> result <- result + 1)
        result

    /// Finds the max `AtTime` in the circular doubly-linked list of which `head` is a member among elements whose
    /// `Key` is `withKey`.
    /// Returns TimeNs.epoch if the list was empty.
    let maxAlarmTime (pool : Pool<ExternalEltValue<'a>>) (head : InternalElt) (withKey : Key) : TimeNs =
        let mutable maxAlarmTime = TimeNs.epoch

        iter
            pool
            head
            (fun current ->
                if key pool current = withKey then
                    maxAlarmTime <- max (atTime pool current) maxAlarmTime
            )

        maxAlarmTime

    let minAlarmTime (pool : Pool<ExternalEltValue<'a>>) (head : InternalElt) (withKey : Key) : TimeNs =
        let mutable minAlarmTime = TimeNs.MaxValue

        iter
            pool
            head
            (fun current ->
                if key pool current = withKey then
                    minAlarmTime <- min (atTime pool current) minAlarmTime
            )

        minAlarmTime
