module ScanRat.Matcher

open System
open System.Collections.Generic

// // Performance: Define an struct option here
// [<Struct>]
// // fsharplint:disable-next-line
// type 'v opt =
//     | Some of 'v
//     | None
//     member inline this.Value =
//         match this with
//         | Some v -> v
//         | None -> failwith "internal error (Value is None)"

type internal Dictionary<'k, 'v> with
    member inline internal this.TryFind key =
        match this.TryGetValue key with
        | (true, v) -> ValueSome v
        | (false, _) -> ValueNone

[<Struct>]
type ParseSuccess<'v> = {
    Value: 'v
    Index: int
    Next: int
}

[<Struct>]
type ParseFailure = { Index: int }

type IItem =
    abstract member Next : int
    abstract member IsSuccess : bool

type ParseResult<'v> =
    | Success of ParseSuccess<'v>
    | Failure of ParseFailure

    interface IItem with
        member this.Next =
            match this with
            | Success ps -> ps.Next
            | _ -> failwith "no next"
        member this.IsSuccess =
            match this with
            | Success _ -> true
            | _ -> false

type Key = obj

[<Struct>]
type Error = {
    Message: string
    Index: int
}

type internal RuleTable = Dictionary<int, IItem ValueOption>
type internal ExpansionTable = Dictionary<int, RuleTable>

type internal MemoTable = Dictionary<Key, ExpansionTable>

[<NoComparison; Struct>]
type internal Expansion = {
    Key: Key
    Num : int
}

[<NoComparison>]
type internal LRRecord = {
    mutable Expansion: Expansion
    mutable LRDetected: bool
    mutable NextIndex: int
    mutable Result: IItem ValueOption
    Involved: HashSet<Key>
    Name : string
}

type internal RecordTable = Dictionary<int, LRRecord>
type internal LRTable = Dictionary<Key, RecordTable>

type Stats = {
    // Number of actual production calls
    mutable Productions: int
    // Number of successful memo lookups
    mutable Memo: int
    // Number of successful lr memo lookupos
    mutable MemoLR: int
} with
    member this.TrackProduction() = this.Productions <- this.Productions + 1
    member this.TrackMemo() = this.Memo <- this.Memo + 1
    member this.TrackMemoLR() = this.MemoLR <- this.MemoLR + 1

[<NoComparison; Struct>]
type internal ErrorRecord = {
    Key: Object
    Expected: string
    CallStack: LRRecord list
}

[<NoComparison>]
type ParsingError = {
    Expected: string
    Stack: string seq
} with
    override this.ToString() =
        this.Expected

[<NoComparison>]
type internal Memo = {
    Table: MemoTable
    Recursions: LRTable
    mutable CallStack: LRRecord list
    LastErrorRecords: List<ErrorRecord>
    mutable LastErrorPos: int
    Stats: Stats
} with
    static member Create() = {
        Table = new MemoTable()
        Recursions = new LRTable()
        CallStack = List.empty
        LastErrorRecords = new List<_>()
        LastErrorPos = -1
        Stats = {
            Memo = 0
            MemoLR = 0
            Productions = 0
        }
    }
    member this.LastError : ParsingError seq =
        let keys = new HashSet<_>()
        seq {
            for er in this.LastErrorRecords do
                if not (keys.Contains er.Key) then
                    yield { ParsingError.Expected = er.Expected; Stack = List.map (fun lr -> lr.Name) er.CallStack }
                    List.iter (fun lr -> keys.Add(lr.Expansion.Key) |> ignore) er.CallStack
                    keys.Add(er.Key) |> ignore
        }

type internal IParseContext =
    abstract member Memo : Memo
    abstract member Index : int

exception MatcherException of Error

let rec internal memoCall (context: 'c :> IParseContext) (name: string) (production : 'c -> 'r :> IItem) : (IItem ValueOption) =
    let memo = context.Memo
    let index = context.Index
    let key = production :> Key
    let expansion = { Key = key; Num = 0 }

    match tryGetMemo memo expansion index with
    | ValueSome result ->
        memo.Stats.TrackMemo()
        result
    | _ ->

    match tryGetLRRecord memo expansion index with
    | ValueSome record ->
        record.LRDetected <- true

        let involved = memo.CallStack |> Seq.takeWhile(fun lr -> lr.Expansion.Key <> expansion.Key) |> Seq.map(fun lr -> lr.Expansion.Key)
        record.Involved.UnionWith involved

        match tryGetMemo memo record.Expansion index with
        | ValueNone -> raise (MatcherException({ Index = index; Message = "Problem with expansion" }))
        | ValueSome result ->
            memo.Stats.TrackMemoLR()
            result
    | ValueNone ->

    // no lr information

    let recordExpansion = { expansion with Num = 1 }
    memoize memo recordExpansion index ValueNone

    let record = {
        LRRecord.LRDetected = false
        Expansion = recordExpansion
        NextIndex = -1
        Result = ValueNone
        Name = name
        Involved = new HashSet<Key>()
    }

    startLRRecord memo expansion index record
    memo.CallStack <- record :: memo.CallStack

    let rec resolveItem() : IItem ValueOption=

        // printf "%d %d %s\n" context.index record.expansion.num name
        let pResult = production context :> IItem
        memo.Stats.TrackProduction()

        let result = if pResult.IsSuccess then (ValueSome pResult) else ValueNone
        // do we need to keep trying the expansions?

        if record.LRDetected && result <> ValueNone && result.Value.Next > record.NextIndex then
            record.Expansion <- { expansion with Num = record.Expansion.Num + 1 }
            record.NextIndex <- result.Value.Next
            memoize memo record.Expansion index result
            record.Result <- result
            resolveItem()
        else
            // we are done trying to expand
            if record.LRDetected then record.Result else result

    let result = resolveItem()

    memo.CallStack <- memo.CallStack.Tail
    forgetLRRecord memo expansion index

    // if there are no LR-processing rules at or above us in the stack, memoize
    let foundLR = memo.CallStack |> Seq.exists(fun lr -> lr.Involved.Contains expansion.Key)
    if not foundLR then
        memoize memo expansion index result
    //else
        //printf "%d %s: can't memoize, because of lr above\n" index name

    if result = ValueNone then
        addError memo index { Key = key; Expected = name; CallStack = memo.CallStack }

    result


and internal tryGetMemo memo expansion index : (IItem ValueOption ValueOption) =
    match memo.Table.TryFind expansion.Key with
    | ValueNone -> ValueNone
    | ValueSome expansionDict ->
    match expansionDict.TryFind expansion.Num with
    | ValueNone -> ValueNone
    | ValueSome ruleDict -> ruleDict.TryFind index

and internal tryGetLRRecord memo expansion index : ValueOption<_>=
    match memo.Recursions.TryFind expansion.Key with
    | ValueNone -> ValueNone
    | ValueSome recordDict -> recordDict.TryFind index

and internal memoize memo expansion index (item : IItem ValueOption) =
    let expansionDict =
        match memo.Table.TryFind expansion.Key with
        | ValueSome expansionDict -> expansionDict
        | ValueNone ->
        let dict = new ExpansionTable()
        memo.Table.Add(expansion.Key, dict)
        dict

    let ruleDict =
        match expansionDict.TryFind expansion.Num with
        | ValueSome ruleDict -> ruleDict
        | ValueNone ->
        let ruleDict = new RuleTable()
        expansionDict.Add(expansion.Num, ruleDict)
        ruleDict

    ruleDict.[index] <- item

and internal startLRRecord memo expansion index record =
    let recordDict =
        match memo.Recursions.TryFind expansion.Key with
        | ValueSome recordDict -> recordDict
        | ValueNone ->
        let rdict = new RecordTable()
        memo.Recursions.Add(expansion.Key, rdict)
        rdict

    recordDict.[index] <- record

and internal forgetLRRecord memo expansion index =
    match memo.Recursions.TryFind expansion.Key with
    | ValueSome recordDict -> recordDict.Remove index |> ignore
    | ValueNone -> ()

and internal addError memo pos error =
    if pos > memo.LastErrorPos then
        memo.LastErrorRecords.Clear()

    if pos >= memo.LastErrorPos then
        memo.LastErrorRecords.Add(error)
        memo.LastErrorPos <- pos
