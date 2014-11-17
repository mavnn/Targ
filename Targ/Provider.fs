namespace Targ.Provider

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Targ.Parse

type IDict = System.Collections.Generic.Dictionary<char, obj>

type FlagType =
    | Int of int
    | String of string
    | Bool of bool

type FlagDef =
    {
        Flag : char
        Name : string
        Description : string
        Default : FlagType
    }

[<AutoOpen>]
module ProviderHelpers =
    let tryParseInt str =
        match Int32.TryParse str with
        | true, i -> Some i
        | false, _ -> None

    let private parseLine (line : string) =
        line.Split('|')
        |> Array.map (fun s -> s.Trim())
        |> function
           | [|flag;name;desc;flagType;defaultValue|] ->
                let defaultValue =
                    match flagType with
                    | "Int" ->
                        match tryParseInt defaultValue with
                        | Some i -> Int i
                        | None -> failwithf "Flag %s should have default type of Int, given %s" flag defaultValue
                    | "Bool" ->
                        Bool false
                    | "String" ->
                        String defaultValue
                    | _ -> failwithf "Unknow flag type of flag %s of %s" flag flagType
                let flagChar =
                    match flag |> Seq.toList with
                    | (Letter f)::[] -> f
                    | _ -> failwithf "Flags must consist of a single letter, given %s" flag
                {
                    Flag = flagChar
                    Name = name
                    Description = desc
                    Default = defaultValue
                }
           | _ ->
                failwithf "Incorrect schema at line:\n%s" line

    let parseSchema (str : string) =
        str.Split([|"\r\n";"\n"|], StringSplitOptions.None)
        |> Array.filter ((<>) "")
        |> Array.map parseLine
        |> Array.toList

    let unbox' t =
        ((AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.find (fun a -> a.GetName().Name = "FSharp.Core")).GetTypes()
        |> Seq.find (fun t -> t.FullName = "Microsoft.FSharp.Core.Operators"))
            .GetMethod("Unbox")
            .MakeGenericMethod [|t|]

[<TypeProvider>]
type TargProvider (cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    let ns = "Targ"
    let asm = Assembly.GetExecutingAssembly()
    let tempAsmPath = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let tempAsm = ProvidedAssembly tempAsmPath

    let provider = ProvidedTypeDefinition(asm, ns, "Args", Some typeof<obj>, IsErased = false, SuppressRelocation = false)

    let createInternalMapField (parent : ProvidedTypeDefinition) =
        let f = ProvidedField("_internalMap", typeof<Map<char, string option>>)
        f.SetFieldAttributes FieldAttributes.Private
        parent.AddMember f
        f

    let createInternalDefsField (parent : ProvidedTypeDefinition) =
        let f = ProvidedField("_internalDefs", typeof<FlagDef list>)
        f.SetFieldAttributes FieldAttributes.Private
        parent.AddMember f
        f

    let createInternalDict (parent : ProvidedTypeDefinition) =
        let f = ProvidedField("_internalDict", typeof<IDict>)
        f.SetFieldAttributes FieldAttributes.Private
        parent.AddMember f
        f

    let createFlagProperty (parent : ProvidedTypeDefinition) idict def =
        let propType, init =
            match def.Default with
            | Int i -> typeof<int>, box i
            | Bool b -> typeof<bool>, box b
            | String s -> typeof<string>, box s
        let p = ProvidedProperty(def.Name, propType)
        p.GetterCode <- fun [this] ->
            let iDict = Expr.FieldGet(this, idict)
            let f = def.Flag
            Expr.Call(unbox' propType, [ <@@ (%%iDict:IDict).[f] @@> ])
        p.AddXmlDoc def.Description
        let initCode = fun this ->
            let f = def.Flag
            <@@ (%%Expr.FieldGet(this, idict):IDict).[f] <- (box init) @@>
        parent.AddMember(p)
        p, initCode

    let createArgsType name schema =
        let t = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, IsErased = false, SuppressRelocation = false)
        let iDict = createInternalDict t
        let props, propInits =
            parseSchema schema
            |> List.map (createFlagProperty t iDict)
            |> List.unzip
        let internalMap = createInternalMapField t
        let internalDefs = createInternalDefsField t
        let c = ProvidedConstructor([ProvidedParameter("cliArgs", typeof<string []>)])
        c.InvokeCode <-
            fun [this;cliArgs] ->
                let setUpInternalDict =
                    Expr.FieldSet(this, iDict,
                        <@@ System.Collections.Generic.Dictionary<char, obj> () @@>)
                let setUpInternalMap =
                    Expr.FieldSet(this, internalMap, 
                        <@@ 
                            let str = String.concat " " (%%cliArgs:string [])
                            parse str
                        @@>)
                let setUpInternalDefs =
                    Expr.FieldSet(this, internalDefs, <@@ parseSchema schema @@>)
                let setDefaultValues =
                    propInits
                    |> List.fold (fun acc valueInit -> Expr.Sequential(valueInit this, acc)) (Expr.Value (()))
                let overrideValues =
                    <@@
                        let defs = (%%Expr.FieldGet(this, internalDefs):FlagDef list)
                        let iDict = (%%Expr.FieldGet(this, iDict):IDict)
                        let overrideValue (flag, value : string option) =
                            let def =
                                defs
                                |> List.tryFind (fun d -> d.Flag = flag)
                            match def with
                            | Some d ->
                                match d.Default with
                                | Int _ ->
                                    match value with
                                    | None ->
                                        failwithf "Flag %A should be of type Int, but no value given" flag
                                    | Some v ->
                                        match tryParseInt v with
                                        | Some i ->
                                            iDict.[d.Flag] <- (box i)
                                        | None ->
                                            failwithf "Flag %A should be of type Int, value given was: %s" flag v
                                | Bool _ ->
                                    match value with
                                    | Some _ ->
                                        failwithf "Flag %A is of type Bool. No value should be given, just provide the flag to set true." flag
                                    | None ->
                                        iDict.[d.Flag] <- (box true)
                                | String _ ->
                                    match value with
                                    | None ->
                                        failwithf "Flag %A should be of type String, but no value given" flag
                                    | Some v ->
                                        iDict.[d.Flag] <- (box v)
                            | None ->
                                failwithf "Flag %A was set, but is not a valid flag." flag
                        (%%Expr.FieldGet(this, internalMap):Map<char, string option>)
                        |> Map.toSeq
                        |> Seq.iter overrideValue
                    @@>
                <@@
                    (%%setUpInternalDict:unit)
                    (%%setUpInternalMap:unit)
                    (%%setUpInternalDefs:unit)
                    (%%setDefaultValues:unit)
                    (%%overrideValues:unit)
                @@>
        t.AddMember c
        tempAsm.AddTypes [t]
        t

    let parameters = [ProvidedStaticParameter("ArgSchema", typeof<string>)]

    do provider.DefineStaticParameters(parameters,
        fun typeName args ->
            let schema = args.[0] :?> string
            createArgsType typeName schema)
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder cfg
        tempAsm.AddTypes [provider]
        this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do ()