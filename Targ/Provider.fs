namespace Targ.Provider

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Targ.Parse

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
module internal ProviderHelpers =
    let private parseLine (line : string) =
        line.Split('|')
        |> Array.map (fun s -> s.Trim())
        |> function
           | [|flag;name;desc;flagType;defaultValue|] ->
                let defaultValue =
                    match flagType with
                    | "Int" ->
                        match Int32.TryParse defaultValue with
                        | true, i -> Int i
                        | false, _ -> failwithf "Flag %s should have default type of Int, given %s" flag defaultValue
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

[<TypeProvider>]
type TargProvider (cfg : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()
    let ns = "Targ"
    let asm = Assembly.GetExecutingAssembly()
    let tempAsmPath = System.IO.Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
    let tempAsm = ProvidedAssembly tempAsmPath

    let provider = ProvidedTypeDefinition(asm, ns, "Args", Some typeof<obj>, IsErased = false, SuppressRelocation = false)

    let createFlagProperty (parent : ProvidedTypeDefinition) def =
        let propType, init =
            match def.Default with
            | Int i -> typeof<int>, box i
            | Bool b -> typeof<bool>, box b
            | String s -> typeof<string>, box s
        let f = ProvidedField("_" + (string def.Flag), propType)
        f.SetFieldAttributes FieldAttributes.Private
        parent.AddMember(f)
        let p = ProvidedProperty(def.Name, propType)
        p.GetterCode <- fun [this] -> Expr.FieldGet(this, f)
        p.SetterCode <- fun [this;value] -> Expr.FieldSet(this, f, value)
        p.AddXmlDoc def.Description
        let initCode = fun this -> Expr.PropertySet(this, p, Expr.Value(init, propType))
        parent.AddMember(p)
        initCode

    let createArgsType name defs =
        let t = ProvidedTypeDefinition(asm, ns, name, Some typeof<obj>, IsErased = false, SuppressRelocation = false)
        let propInits =
            defs
            |> List.map (createFlagProperty t)
        let c = ProvidedConstructor([])
        c.InvokeCode <-
            fun [this] ->
                propInits
                |> List.fold (fun acc valueInit -> Expr.Sequential(valueInit this, acc)) (Expr.Value (()))
        t.AddMember c
        tempAsm.AddTypes [t]
        t

    let parameters = [ProvidedStaticParameter("ArgSchema", typeof<string>)]

    do provider.DefineStaticParameters(parameters,
        fun typeName args ->
            let schema = args.[0] :?> string
            createArgsType typeName (parseSchema schema))
    do
        this.RegisterRuntimeAssemblyLocationAsProbingFolder cfg
        tempAsm.AddTypes [provider]
        this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do ()